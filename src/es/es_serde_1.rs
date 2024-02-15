use serde::Deserialize;
use serde_json::{json, Value};

use crate::book::{Book, Code};

/// The name of the serialiser, persisted to the `_serialiser` field of the
/// elasticsearch record.
pub static SERIALISER: &str = "es_serde_1";

#[derive(Debug)]
pub enum Error {
    IncorrectSerialiser,
    Serde(serde_json::Error),
}

pub fn serialise(book: &Book) -> Value {
    let mut source = serde_json::to_value(book).unwrap();
    let map = source.as_object_mut().unwrap();
    map.insert(
        "_serialiser".to_string(),
        Value::String(SERIALISER.to_string()),
    );
    map.insert(
        "_display_title".to_string(),
        Value::String(book.display_title()),
    );
    map.remove("code");
    json!({ "_id": book.code.to_string(), "_source": source })
}

pub fn try_deserialise(code: &Code, source: &Value) -> Result<Book, Error> {
    if source["_serialiser"].as_str() != Some(SERIALISER) {
        return Err(Error::IncorrectSerialiser);
    }

    let mut binding: Value = source.clone();
    if let Some(map) = binding.as_object_mut() {
        map.insert("code".to_string(), json!(code));
    }
    Book::deserialize(&binding).map_err(Error::Serde)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::book::test_helpers::arbitrary_book;

    use proptest::prelude::*;

    proptest! {
        #[test]
        fn roundtrip(book in arbitrary_book()) {
            let serialised = serialise(&book);
            if let Ok(deserialised) = try_deserialise(&book.code, &serialised["_source"]) {
                assert_eq!(book, deserialised);
            } else {
                panic!("could not deserialise");
            }
        }
    }
}
