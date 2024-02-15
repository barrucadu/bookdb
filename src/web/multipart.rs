pub use actix_multipart::form::tempfile::TempFile;

use actix_multipart::form::text::Text;
use actix_multipart::form::MultipartForm;
use mime::Mime;
use serde_json::{json, Value};
use std::collections::HashMap;
use time::macros::*;
use time::Date;

use crate::book::{Book, Code, Holding};
use crate::config::Slug;

// MULTIPART FORM HANDLING (add / edit)
//
// This is horrible, surely there's a better way to do this.

pub type PutBookForm = MultipartForm<PutBook>;

#[derive(Debug, MultipartForm)]
pub struct PutBook {
    code: Option<Text<String>>,
    #[multipart(limit = "10MiB")]
    cover: Option<TempFile>,
    category: Option<Text<String>>,
    title: Option<Text<String>>,
    subtitle: Option<Text<String>>,
    volume_title: Option<Text<String>>,
    volume_number: Option<Text<String>>,
    fascicle_number: Option<Text<String>>,
    has_been_read: Option<Text<String>>,
    last_read_date: Option<Text<String>>,
    #[multipart(rename = "authors[]")]
    authors: Vec<Text<String>>,
    #[multipart(rename = "editors[]")]
    editors: Vec<Text<String>>,
    #[multipart(rename = "translators[]")]
    translators: Vec<Text<String>>,
    #[multipart(rename = "locations[]")]
    locations: Vec<Text<String>>,
    #[multipart(rename = "notes[]")]
    notes: Vec<Text<String>>,
    bucket: Option<Text<String>>,
}

pub fn putbook_to_book(
    form: PutBook,
    category_fullname_map: &HashMap<Slug, Vec<String>>,
    location_name_map: &HashMap<Slug, String>,
) -> Result<(Book, Option<TempFile>), (Value, Vec<String>)> {
    let (book_json, tempfile, errors) =
        validate_putbook(form, category_fullname_map, location_name_map);

    if !errors.is_empty() {
        return Err((book_json, errors));
    }

    // all the unsafe stuff is fine here because of the prior validation
    let nonempty_strvec = |s: Vec<String>| if s.is_empty() { None } else { Some(s) };
    let nonempty_str = |s: String| if s.is_empty() { None } else { Some(s) };
    let json_str = |s: Value| s.as_str().map(|s| s.to_string());
    let get_json_str = |k| json_str(book_json[k].clone());
    let get_json_strvec = |k| {
        book_json[k]
            .as_array()
            .unwrap()
            .iter()
            .map(|v| json_str(v.clone()).unwrap())
            .collect()
    };

    let authors: Vec<String> = get_json_strvec("authors");
    let bucket = if let Some(b) = get_json_str("bucket").and_then(nonempty_str) {
        b
    } else {
        let mut surnames: Vec<String> = authors
            .iter()
            .map(|a| a.split_whitespace().last().unwrap().to_string())
            .collect();
        surnames.sort();
        surnames[0].clone()
    };
    let book = Book {
        code: get_json_str("code").unwrap().parse().unwrap(),
        title: get_json_str("title").unwrap(),
        subtitle: get_json_str("subtitle").and_then(nonempty_str),
        volume_title: get_json_str("volume_title").and_then(nonempty_str),
        volume_number: get_json_str("volume_number").and_then(nonempty_str),
        fascicle_number: get_json_str("fascicle_number").and_then(nonempty_str),
        authors,
        editors: nonempty_strvec(get_json_strvec("editors")),
        translators: nonempty_strvec(get_json_strvec("translators")),
        has_been_read: book_json["has_been_read"].as_bool().unwrap(),
        last_read_date: get_json_str("last_read_date")
            .and_then(|s| Date::parse(&s, format_description!("[year]-[month]-[day]")).ok()),
        cover_image_mimetype: tempfile
            .as_ref()
            .map(|f| f.content_type.clone().unwrap().essence_str().to_string()),
        holdings: {
            let arr = book_json["holdings"].as_array().unwrap();
            let mut out = Vec::with_capacity(arr.len());
            for h in arr {
                let location = Slug(json_str(h["location_slug"].clone()).unwrap());
                let note = json_str(h["notes"].clone());
                out.push(Holding { location, note });
            }
            out
        },
        bucket,
        category: Slug(get_json_str("category_slug").unwrap()),
    };
    Ok((book, tempfile))
}

// this has to return something that can be stuck into a template context, so
// that I don't need to type out all the details again just because I typo'd the
// ISBN.
fn validate_putbook(
    form: PutBook,
    category_fullname_map: &HashMap<Slug, Vec<String>>,
    location_name_map: &HashMap<Slug, String>,
) -> (Value, Option<TempFile>, Vec<String>) {
    let mut errors = Vec::new();
    let code = multipart_str(form.code);
    let title = multipart_str(form.title);
    let volume_number = multipart_str(form.volume_number);
    let fascicle_number = multipart_str(form.fascicle_number);
    let authors = multipart_vec(form.authors);
    let last_read_date = multipart_str(form.last_read_date);
    let category_slug = multipart_str(form.category);
    let mut holdings = Vec::new();

    if let Some(s) = code.clone() {
        if s.parse::<Code>().is_err() {
            errors.push(format!("The code '{s}' is invalid."));
        }
    } else {
        errors.push("The code cannot be blank.".to_string());
    }
    if title.is_none() {
        errors.push("The title cannot be blank.".to_string());
    }
    if !volume_number.as_deref().map(is_alnum).unwrap_or(true) {
        errors.push("The volume number can only contain letters and numbers.".to_string());
    }
    if !fascicle_number.as_deref().map(is_alnum).unwrap_or(true) {
        errors.push("The fascicle number can only contain letters and numbers.".to_string());
    }
    if authors.is_none() {
        errors.push("There must be at least one author.".to_string());
    }
    if let Some(s) = last_read_date.clone() {
        if Date::parse(&s, format_description!("[year]-[month]-[day]")).is_err() {
            errors.push(format!("The last read date '{s}' is invalid."));
        }
    }
    if let Some(s) = category_slug.clone() {
        let slug = Slug(s.clone());
        if !category_fullname_map.contains_key(&slug) {
            errors.push(format!("There is no such category '{s}'."));
        }
    } else {
        errors.push("The category cannot be blank.".to_string());
    }
    for (l, n) in std::iter::zip(form.locations, form.notes) {
        if let Some(s) = multipart_str(Some(l)) {
            let location = Slug(s.clone());
            let note = multipart_str(Some(n));
            holdings.push(json!({ "location_slug": location, "note": note }));
            if !location_name_map.contains_key(&location) {
                errors.push(format!("There is no such location '{s}'"));
            }
        }
    }
    if holdings.is_empty() {
        errors.push("There must be at least one holding.".to_string());
    }
    let cover = match validate_image(form.cover) {
        Ok(cover) => cover,
        Err(error) => {
            errors.push(error);
            None
        }
    };

    let book_json = json!({
        "code": code,
        "title": title,
        "subtitle": multipart_str(form.subtitle),
        "volume_title": multipart_str(form.volume_title),
        "volume_number": volume_number,
        "fascicle_number": fascicle_number,
        "authors": authors.unwrap_or_default(),
        "translators": multipart_vec(form.translators).unwrap_or_default(),
        "editors": multipart_vec(form.editors).unwrap_or_default(),
        "has_been_read": form.has_been_read.is_some(),
        "last_read_date": last_read_date,
        "category_slug": category_slug,
        "holdings": holdings,
        "bucket": multipart_str(form.bucket),
    });

    (book_json, cover, errors)
}

fn validate_image(image: Option<TempFile>) -> Result<Option<TempFile>, String> {
    if let Some(tempfile) = image {
        if tempfile.size > 0 {
            return match tempfile.content_type {
                Some(ref mime) if allowed_image_type(mime) => Ok(Some(tempfile)),
                Some(_) => Err("Cover image must be a JPEG or PNG.".to_string()),
                None => Ok(None),
            };
        }
    }

    Ok(None)
}

fn allowed_image_type(mime: &Mime) -> bool {
    let ty = mime.type_();
    let subty = mime.subtype();
    ty == mime::IMAGE && (subty == mime::JPEG || subty == mime::PNG)
}

fn is_alnum(s: &str) -> bool {
    s.chars().all(|c| c.is_alphanumeric())
}

fn multipart_str(txt: Option<Text<String>>) -> Option<String> {
    if let Some(s) = txt {
        if s.is_empty() {
            None
        } else {
            Some(s.to_string())
        }
    } else {
        None
    }
}

fn multipart_vec(vec: Vec<Text<String>>) -> Option<Vec<String>> {
    let flattened: Vec<String> = vec
        .into_iter()
        .flat_map(|s| multipart_str(Some(s)))
        .collect();
    if flattened.is_empty() {
        None
    } else {
        Some(flattened)
    }
}
