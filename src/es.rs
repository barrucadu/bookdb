use elasticsearch::http::request::JsonBody;
use elasticsearch::indices::{IndicesCreateParts, IndicesDeleteParts};
use elasticsearch::params::OpType;
use elasticsearch::{
    BulkParts, ClearScrollParts, DeleteParts, Elasticsearch, Error, GetParts, IndexParts,
    ScrollParts, SearchParts,
};
use serde_json::{json, Value};
use std::collections::HashMap;
use std::default::Default;

use crate::book::{Book, Code, ParseCodeError};
use crate::config::Slug;

static INDEX_NAME: &str = "bookdb";

pub async fn create(client: &Elasticsearch) -> Result<(), Error> {
    client
        .indices()
        .create(IndicesCreateParts::Index(INDEX_NAME))
        .body(json!({
            "mappings": {
                "properties": {
                    "_display_title": { "type": "text", "analyzer": "english" },
                    "title": { "type": "text", "analyzer": "english" },
                    "subtitle": { "type": "text", "analyzer": "english" },
                    "volume_title": { "type": "text", "analyzer": "english" },
                    "volume_number": { "type": "keyword" },
                    "fascicle_number": { "type": "keyword" },
                    "authors": { "type": "keyword" },
                    "translators": { "type": "keyword" },
                    "editors": { "type": "keyword" },
                    "has_been_read": { "type": "boolean" },
                    "last_read_date": { "type": "date", "format": "yyyy-MM-dd" },
                    "cover_image_mimetype": { "type": "keyword" },
                    "holdings": {
                        "type": "nested",
                        "dynamic": "strict",
                        "properties": {
                            "location": { "type": "keyword" },
                            "note": { "type": "text" },
                        },
                    },
                    "bucket": { "type": "keyword" },
                    "category": { "type": "keyword" },
                }
            }
        }))
        .send()
        .await
        .map(|_| ())
}

pub async fn drop(client: &Elasticsearch) -> Result<(), Error> {
    client
        .indices()
        .delete(IndicesDeleteParts::Index(&[INDEX_NAME]))
        .send()
        .await
        .map(|_| ())
}

pub async fn get(client: &Elasticsearch, code: Code) -> Result<Option<Book>, Error> {
    let response = client
        .get(GetParts::IndexId(INDEX_NAME, &code.to_string()))
        .send()
        .await?;
    let response_body = response.json::<Value>().await?;
    Ok(try_deserialise(&response_body).ok())
}

pub async fn put(client: &Elasticsearch, book: Book) -> Result<(), Error> {
    let source = &serialise(&book)["_source"];
    client
        .index(IndexParts::IndexId(
            INDEX_NAME,
            &book.inner.code.to_string(),
        ))
        .op_type(OpType::Create)
        .body(source)
        .send()
        .await?;
    Ok(())
}

pub async fn delete(client: &Elasticsearch, code: Code) -> Result<(), Error> {
    client
        .delete(DeleteParts::IndexId(INDEX_NAME, &code.to_string()))
        .send()
        .await?;
    Ok(())
}

#[derive(Default)]
pub struct SearchQuery {
    pub keywords: Option<String>,
    pub read: Option<bool>,
    pub location: Option<Slug>,
    pub categories: Vec<Slug>,
    pub people: Vec<String>,
}

impl SearchQuery {
    pub fn build_query_json(self) -> Value {
        let mut queries = vec![json!({"match_all": {}})];

        if let Some(keywords) = self.keywords {
            queries.push(
                json!({"query_string": {"query": keywords, "default_field": "_display_title"}}),
            );
        }
        if let Some(has_been_read) = self.read {
            queries.push(json!({"term": {"has_been_read": has_been_read}}));
        }
        if let Some(location) = self.location {
            queries.push(json!({
                "nested": {
                    "path": "holdings",
                    "query": {"bool": {"must": {"term": {"holdings.location": location}}}},
                },
            }));
        }
        if !self.categories.is_empty() {
            queries.push(json!({"terms": {"category": self.categories}}));
        }
        if !self.people.is_empty() {
            queries.push(json!({
                "bool": {
                    "should": [
                        {"terms": {"authors": self.people}},
                        {"terms": {"editors": self.people}},
                        {"terms": {"translators": self.people}},
                    ],
                },
            }));
        }

        json!({"bool": {"must": queries}})
    }
}

pub struct SearchResult {
    pub count: usize,
    pub hits: Vec<Book>,
    pub aggs: SearchResultAggs,
}

pub struct SearchResultAggs {
    pub author: HashMap<String, u64>,
    pub translator: HashMap<String, u64>,
    pub editor: HashMap<String, u64>,
    pub read: u64,
    pub unread: u64,
    pub category: HashMap<Slug, u64>,
    pub location: HashMap<Slug, u64>,
}

pub async fn search(client: &Elasticsearch, query: SearchQuery) -> Result<SearchResult, Error> {
    let res = scroll(client, json!({
        "query": query.build_query_json(),
        "aggs": {
            "author": {"terms": {"field": "authors", "size": 1000}},
            "editor": {"terms": {"field": "editors", "size": 500}},
            "translator": {"terms": {"field": "translators", "size": 500}},
            "has_been_read": {"terms": {"field": "has_been_read", "size": 500}},
            "category": {"terms": {"field": "category", "size": 500}},
            "holdings": {"nested": {"path": "holdings"}, "aggs": {"location": {"terms": {"field": "holdings.location", "size": 500}}}},
        },
    })).await?;

    let mut aggs = SearchResultAggs {
        author: agg_buckets_to_hashmap(&res.aggs["author"]["buckets"]),
        editor: agg_buckets_to_hashmap(&res.aggs["editor"]["buckets"]),
        translator: agg_buckets_to_hashmap(&res.aggs["translator"]["buckets"]),
        read: 0,
        unread: 0,
        category: HashMap::new(),
        location: HashMap::new(),
    };
    for (key, doc_count) in agg_buckets_to_hashmap(&res.aggs["has_been_read"]["buckets"]).drain() {
        if key == "true" {
            aggs.read = doc_count;
        } else {
            aggs.unread = doc_count;
        }
    }
    for (key, doc_count) in agg_buckets_to_hashmap(&res.aggs["category"]["buckets"]).drain() {
        aggs.category.insert(Slug(key), doc_count);
    }
    for (key, doc_count) in
        agg_buckets_to_hashmap(&res.aggs["holdings"]["location"]["buckets"]).drain()
    {
        aggs.location.insert(Slug(key), doc_count);
    }

    Ok(SearchResult {
        count: res.hits.len(),
        hits: res.hits,
        aggs,
    })
}

pub async fn export(client: &Elasticsearch) -> Result<Vec<Book>, Error> {
    let res = scroll(client, json!({"query": {"match_all": {}}})).await?;
    Ok(res.hits)
}

pub async fn import(client: &Elasticsearch, books: Vec<Book>) -> Result<Option<usize>, Error> {
    let mut body: Vec<JsonBody<_>> = Vec::with_capacity(books.len() * 2);
    for book in &books {
        let value = serialise(book);
        let id = value["_id"].as_str().unwrap();
        let source = value["_source"].as_object().unwrap().clone();
        body.push(json!({"index": {"_id": id}}).into());
        body.push(Value::Object(source).into());
    }

    let response = client
        .bulk(BulkParts::Index(INDEX_NAME))
        .body(body)
        .send()
        .await?;
    let response_body = response.json::<Value>().await?;
    if response_body["errors"].as_bool().unwrap() {
        Ok(None)
    } else {
        Ok(Some(books.len()))
    }
}

#[derive(Debug)]
pub enum DeserialiseBookError {
    InvalidCode(ParseCodeError),
    Serde(serde_json::Error),
}

pub fn serialise(book: &Book) -> Value {
    let mut source = serde_json::to_value(book).unwrap();
    let map = source.as_object_mut().unwrap();
    map.insert(
        "_display_title".to_string(),
        Value::String(book.display_title()),
    );
    map.remove("code");
    json!({ "_id": book.inner.code.to_string(), "_source": source })
}

pub fn try_deserialise(hit: &Value) -> Result<Book, DeserialiseBookError> {
    let code: Code = hit["_id"]
        .as_str()
        .unwrap()
        .parse()
        .map_err(DeserialiseBookError::InvalidCode)?;

    let mut source: Value = hit["_source"].clone();
    let map = source.as_object_mut().unwrap();
    map.insert("code".to_string(), json!(code));

    Book::deserialize(&source).map_err(DeserialiseBookError::Serde)
}

struct ScrollResult {
    hits: Vec<Book>,
    aggs: Value,
}

async fn scroll(client: &Elasticsearch, body: Value) -> Result<ScrollResult, Error> {
    let mut books = Vec::new();

    let mut response = client
        .search(SearchParts::Index(&[INDEX_NAME]))
        .scroll("5m")
        .body(body)
        .send()
        .await?;
    let mut response_body = response.json::<Value>().await?;
    let mut scroll_id = response_body["_scroll_id"].as_str().unwrap();
    let mut hits = response_body["hits"]["hits"].as_array().unwrap();
    let aggs = response_body["aggregations"].clone();

    while !hits.is_empty() {
        books.extend(hits.iter().map(|hit| try_deserialise(hit).unwrap()));
        response = client
            .scroll(ScrollParts::ScrollId(scroll_id))
            .scroll("5m")
            .send()
            .await?;
        response_body = response.json::<Value>().await?;
        scroll_id = response_body["_scroll_id"].as_str().unwrap();
        hits = response_body["hits"]["hits"].as_array().unwrap();
    }

    client
        .clear_scroll(ClearScrollParts::ScrollId(&[scroll_id]))
        .send()
        .await?;

    books.sort();
    Ok(ScrollResult { hits: books, aggs })
}

fn agg_buckets_to_hashmap(buckets: &Value) -> HashMap<String, u64> {
    if let Some(buckets) = buckets.as_array() {
        let mut out = HashMap::with_capacity(buckets.len());
        for bucket in buckets {
            let key_field = bucket["key"].as_str();
            let key_as_string_field = bucket["key_as_string"].as_str();

            let key = key_field.or(key_as_string_field).unwrap().to_string();
            let doc_count = bucket["doc_count"].as_u64().unwrap();
            out.insert(key, doc_count);
        }
        out
    } else {
        HashMap::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::book::test_helpers::arbitrary_book;

    use proptest::prelude::*;

    #[test]
    fn default_searchquery_fetches_all() {
        assert_eq!(
            json!({"bool": {"must": vec![json!({"match_all": {}})]}}),
            SearchQuery::default().build_query_json(),
        );
    }

    proptest! {
        #[test]
        fn roundtrip(book in arbitrary_book()) {
            if let Ok(deserialised) = try_deserialise(&serialise(&book)) {
                assert_eq!(book, deserialised);
            } else {
                panic!("could not deserialise");
            }
        }
    }
}
