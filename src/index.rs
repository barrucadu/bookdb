use elasticsearch::indices::IndicesCreateParts;
use elasticsearch::{ClearScrollParts, Elasticsearch, Error, ScrollParts, SearchParts};
use serde_json::{json, Value};
use time::macros::*;
use time::Date;

use crate::book::{Book, Holding, ParseCodeError};
use crate::config::Slug;

static INDEX_NAME: &str = "bookdb";

pub async fn create(client: &Elasticsearch) -> Result<(), Error> {
    client
        .indices()
        .create(IndicesCreateParts::Index(INDEX_NAME))
        .body(json!({
            "mappings": {
                "properties": {
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

pub async fn drop(_client: &Elasticsearch) {
    println!("bookdb::index::drop");
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

pub async fn import(_client: &Elasticsearch) {
    println!("bookdb::index::import");
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum ValueToBookError {
    InvalidCode(ParseCodeError),
    MissingTitle,
    MissingAuthors,
    MissingHoldings,
    MissingBucket,
    MissingCategory,
}

impl TryFrom<&Value> for Book {
    type Error = ValueToBookError;

    fn try_from(hit: &Value) -> Result<Self, Self::Error> {
        let id = hit["_id"].as_str().unwrap();
        let source = hit["_source"].as_object().unwrap();

        Ok(Book {
            code: id.parse().map_err(ValueToBookError::InvalidCode)?,
            title: get_string(source, "title").ok_or(ValueToBookError::MissingTitle)?,
            subtitle: get_string(source, "subtitle"),
            volume_title: get_string(source, "volume_title"),
            volume_number: get_part_number(source, "volume_number"),
            fascicle_number: get_part_number(source, "fascicle_number"),
            authors: get_person_list(source, "authors").ok_or(ValueToBookError::MissingAuthors)?,
            translators: get_person_list(source, "translators"),
            editors: get_person_list(source, "editors"),
            has_been_read: get_bool(source, "has_been_read").unwrap_or(false),
            last_read_date: get_date(source, "last_read_date"),
            cover_image_mimetype: get_string(source, "cover_image_mimetype"),
            holdings: get_holdings(source, "holdings").ok_or(ValueToBookError::MissingHoldings)?,
            bucket: get_string(source, "bucket").ok_or(ValueToBookError::MissingBucket)?,
            category: get_slug(source, "category").ok_or(ValueToBookError::MissingCategory)?,
        })
    }
}

fn get_bool(source: &serde_json::Map<String, serde_json::Value>, key: &str) -> Option<bool> {
    match source.get(key) {
        Some(Value::Bool(b)) => Some(*b),
        _ => None,
    }
}

fn get_date(source: &serde_json::Map<String, serde_json::Value>, key: &str) -> Option<Date> {
    match source.get(key) {
        Some(Value::String(s)) => Date::parse(s, format_description!("[year]-[month]-[day]")).ok(),
        _ => None,
    }
}

fn get_holdings(
    source: &serde_json::Map<String, serde_json::Value>,
    key: &str,
) -> Option<Vec<Holding>> {
    if let Some(vals) = source.get(key) {
        let vals_arr = vals.as_array().unwrap();
        let mut holdings = Vec::with_capacity(vals_arr.len());
        for val in vals_arr {
            let val_obj = val.as_object().unwrap();
            let location = get_slug(val_obj, "location").unwrap();
            let note = get_string(val_obj, "note")
                .filter(|s| !s.is_empty())
                .or(get_string(val_obj, "notes").filter(|s| !s.is_empty()));
            holdings.push(Holding { location, note });
        }
        Some(holdings)
    } else {
        None
    }
}

fn get_part_number(
    source: &serde_json::Map<String, serde_json::Value>,
    key: &str,
) -> Option<String> {
    match source.get(key) {
        Some(Value::Object(val)) => Some(val["raw"].as_str().unwrap().to_string()),
        Some(Value::String(val)) => Some(val.to_string()),
        _ => None,
    }
}

fn get_person_list(
    source: &serde_json::Map<String, serde_json::Value>,
    key: &str,
) -> Option<Vec<String>> {
    let names = if let Some(nested) = source.get("people") {
        nested.get(key)
    } else {
        source.get(key)
    };

    match names {
        Some(Value::Array(vals)) => {
            let mut people = Vec::with_capacity(vals.len());
            for val in vals {
                people.push(val.as_str().unwrap().to_string());
            }
            if people.is_empty() {
                None
            } else {
                Some(people)
            }
        }
        _ => None,
    }
}

fn get_slug(source: &serde_json::Map<String, serde_json::Value>, key: &str) -> Option<Slug> {
    get_string(source, key)
        .or(get_string(source, &format!("{key}_uuid")))
        .map(Slug)
}

fn get_string(source: &serde_json::Map<String, serde_json::Value>, key: &str) -> Option<String> {
    match source.get(key) {
        Some(Value::String(s)) => Some(s.to_string()),
        _ => None,
    }
}
