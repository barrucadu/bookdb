use serde_json::Value;
use time::macros::*;
use time::Date;

use crate::book::{Book, Code, Holding};
use crate::config::Slug;

#[derive(Debug)]
pub enum Error {
    MissingTitle,
    MissingAuthors,
    MissingHoldings,
    MissingBucket,
    MissingCategory,
}

pub fn try_deserialise(code: Code, source: &Value) -> Result<Book, Error> {
    Ok(Book {
        code,
        title: get_string(source, "title").ok_or(Error::MissingTitle)?,
        subtitle: get_string(source, "subtitle"),
        volume_title: get_string(source, "volume_title"),
        volume_number: source["volume_number"]["raw"]
            .as_str()
            .map(|s| s.to_string()),
        fascicle_number: source["fascicle_number"]["raw"]
            .as_str()
            .map(|s| s.to_string()),
        authors: get_person_list(source, "authors").ok_or(Error::MissingAuthors)?,
        translators: get_person_list(source, "translators"),
        editors: get_person_list(source, "editors"),
        has_been_read: source["has_been_read"].as_bool().unwrap_or(false),
        last_read_date: get_date(source, "last_read_date"),
        cover_image_mimetype: get_string(source, "cover_image_mimetype"),
        holdings: get_holdings(source).ok_or(Error::MissingHoldings)?,
        bucket: get_string(source, "bucket").ok_or(Error::MissingBucket)?,
        category: get_string(source, "category_uuid")
            .map(Slug)
            .ok_or(Error::MissingCategory)?,
    })
}

fn get_date(source: &Value, key: &str) -> Option<Date> {
    get_string(source, key)
        .and_then(|s| Date::parse(&s, format_description!("[year]-[month]-[day]")).ok())
}

fn get_person_list(source: &Value, key: &str) -> Option<Vec<String>> {
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

fn get_holdings(source: &Value) -> Option<Vec<Holding>> {
    if let Some(vals) = source["holdings"].as_array() {
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
    use serde_json::json;

    proptest! {
        #[test]
        fn legacy(book in arbitrary_book()) {
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
            });

            if let Ok(deserialised) = try_deserialise(book.code.clone(), &bookdbpy) {
                assert_eq!(book, deserialised);
            } else {
                panic!("could not deserialise");
            }
        }
    }
}
