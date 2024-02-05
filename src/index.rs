use elasticsearch::http::request::JsonBody;
use elasticsearch::indices::{IndicesCreateParts, IndicesDeleteParts};
use elasticsearch::{BulkParts, ClearScrollParts, Elasticsearch, Error, ScrollParts, SearchParts};
use serde::Deserialize;
use serde_json::{json, Value};
use time::macros::*;
use time::Date;

use crate::book::{Book, Code, Holding, ParseCodeError};
use crate::config::Slug;

static SERIALISER_ES_SERDE_1: &str = "es_serde_1";
static INDEX_NAME: &str = "bookdb";

pub async fn create(client: &Elasticsearch) -> Result<(), Error> {
    client
        .indices()
        .create(IndicesCreateParts::Index(INDEX_NAME))
        .body(json!({
            "mappings": {
                "properties": {
                    "_serialiser": { "type": "keyword" },
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

pub async fn export(client: &Elasticsearch) -> Result<Vec<Book>, Error> {
    let mut books = Vec::new();

    let mut response = client
        .search(SearchParts::Index(&[INDEX_NAME]))
        .scroll("5m")
        .body(json!({"query": {"match_all": {}}}))
        .send()
        .await?;
    let mut response_body = response.json::<Value>().await?;
    let mut scroll_id = response_body["_scroll_id"].as_str().unwrap();
    let mut hits = response_body["hits"]["hits"].as_array().unwrap();

    while !hits.is_empty() {
        books.extend(hits.iter().map(|hit| Book::try_from(hit).unwrap()));
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

    Ok(books)
}

pub async fn import(client: &Elasticsearch, books: Vec<Book>) -> Result<Option<usize>, Error> {
    let mut body: Vec<JsonBody<_>> = Vec::with_capacity(books.len() * 2);
    for book in &books {
        let value = Into::<Value>::into(book);
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
pub enum ValueToBookError {
    InvalidCode(ParseCodeError),
    UnknownSerialiserVersion,
    Serde(serde_json::Error),
    MissingTitle,
    MissingAuthors,
    MissingHoldings,
    MissingBucket,
    MissingCategory,
}

impl TryFrom<&Value> for Book {
    type Error = ValueToBookError;

    fn try_from(hit: &Value) -> Result<Self, Self::Error> {
        let code = hit["_id"]
            .as_str()
            .unwrap()
            .parse()
            .map_err(ValueToBookError::InvalidCode)?;
        let source = &hit["_source"];
        match hit["_source"]["_serialiser"].as_str() {
            Some(s) if s == SERIALISER_ES_SERDE_1 => deserialise_es_serde_1(code, source),
            Some(_) => Err(ValueToBookError::UnknownSerialiserVersion),
            None => deserialise_legacy(code, source),
        }
    }
}

impl From<&Book> for Value {
    fn from(book: &Book) -> Self {
        let mut source = serde_json::to_value(book).unwrap();
        let map = source.as_object_mut().unwrap();
        map.insert(
            "_serialiser".to_string(),
            Value::String(SERIALISER_ES_SERDE_1.to_string()),
        );
        map.remove("code");
        json!({ "_id": book.code.to_string(), "_source": source })
    }
}

fn deserialise_es_serde_1(code: Code, source: &Value) -> Result<Book, ValueToBookError> {
    let mut binding: Value = source.clone();
    if let Some(map) = binding.as_object_mut() {
        map.insert("code".to_string(), json!(code));
    }
    Book::deserialize(&binding).map_err(ValueToBookError::Serde)
}

fn deserialise_legacy(code: Code, source: &Value) -> Result<Book, ValueToBookError> {
    let last_read_date = if let Some(s) = source["last_read_date"].as_str() {
        Date::parse(s, format_description!("[year]-[month]-[day]")).ok()
    } else {
        None
    };

    let holdings = if let Some(vals) = source["holdings"].as_array() {
        let mut out = Vec::with_capacity(vals.len());
        for val in vals {
            let location = Slug(get_string(val, "location_uuid").unwrap());
            let note = get_string(val, "notes").filter(|s| !s.is_empty());
            out.push(Holding { location, note });
        }
        if out.is_empty() {
            None
        } else {
            Some(out)
        }
    } else {
        None
    };

    Ok(Book {
        code,
        title: get_string(source, "title").ok_or(ValueToBookError::MissingTitle)?,
        subtitle: get_string(source, "subtitle"),
        volume_title: get_string(source, "volume_title"),
        volume_number: source["volume_number"]["raw"]
            .as_str()
            .map(|s| s.to_string()),
        fascicle_number: source["fascicle_number"]["raw"]
            .as_str()
            .map(|s| s.to_string()),
        authors: get_legacy_person_list(source, "authors")
            .ok_or(ValueToBookError::MissingAuthors)?,
        translators: get_legacy_person_list(source, "translators"),
        editors: get_legacy_person_list(source, "editors"),
        has_been_read: source["has_been_read"].as_bool().unwrap_or(false),
        last_read_date,
        cover_image_mimetype: get_string(source, "cover_image_mimetype"),
        holdings: holdings.ok_or(ValueToBookError::MissingHoldings)?,
        bucket: get_string(source, "bucket").ok_or(ValueToBookError::MissingBucket)?,
        category: get_string(source, "category_uuid")
            .map(Slug)
            .ok_or(ValueToBookError::MissingCategory)?,
    })
}

fn get_legacy_person_list(source: &Value, key: &str) -> Option<Vec<String>> {
    if let Some(vals) = source["people"][key].as_array() {
        let mut people = Vec::with_capacity(vals.len());
        for val in vals {
            people.push(val.as_str().unwrap().to_string());
        }
        if people.is_empty() {
            None
        } else {
            Some(people)
        }
    } else {
        None
    }
}

fn get_string(source: &Value, key: &str) -> Option<String> {
    source[key].as_str().map(|s| s.to_string())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::book::test_helpers::{arbitrary_book, number_bits_vec};
    use crate::book::BookSortKey;

    use proptest::prelude::*;

    proptest! {
        #[test]
        fn book_serialise_deserialise_roundtrip(book in arbitrary_book()) {
            let serialised = Into::<Value>::into(&book);
            let deserialised = Book::try_from(&serialised).unwrap();
            assert_eq!(book, deserialised);
        }

        #[test]
        fn deserialise_bookdbpy(book in arbitrary_book()) {
            let sort_key = Into::<BookSortKey>::into(&book);
            let volume_number = book.volume_number.as_ref().
                map(|num| json!({"raw": num, "bits": number_bits_vec(&sort_key, true)}));
            let fascicle_number = book.fascicle_number.as_ref().
                map(|num| json!({"raw": num, "bits": number_bits_vec(&sort_key, false)}));
            let mut holdings = Vec::with_capacity(book.holdings.len());
            for holding in &book.holdings {
                holdings.push(json!({ "location_uuid": holding.location.0, "notes": holding.note }));
            }

            let bookdbpy = json!({
                "_id": book.code.to_string(),
                "_source": {
                    "title": book.title,
                    "subtitle": book.subtitle,
                    "volume_title": book.volume_title,
                    "display_title": book.display_title(),
                    "volume_number": volume_number,
                    "fascicle_number": fascicle_number,
                    "people": {
                        "authors": book.authors,
                        "translators": book.translators,
                        "editors": book.editors,
                    },
                    "has_been_read": book.has_been_read,
                    "last_read_date": book.last_read_date,
                    "cover_image_mimetype": book.cover_image_mimetype,
                    "holdings": holdings,
                    "bucket": book.bucket,
                    "category_uuid": book.category.0,
                    "created_at": "1234-05-06T07:08:09",
                    "updated_at": "1234-05-06T07:08:09",
                },
            });

            let deserialised = Book::try_from(&bookdbpy).unwrap();
            assert_eq!(book, deserialised);
        }
    }
}
