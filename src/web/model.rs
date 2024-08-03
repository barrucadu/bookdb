use axum::extract::{multipart, Multipart};
use mime;
use serde_json::{json, Value};
use std::collections::HashMap;
use tempfile::NamedTempFile;
use time::macros::format_description;
use time::Date;
use tokio::fs::File as AsyncFile;
use tokio::io::AsyncWriteExt;

use crate::book::{Book, BookDisplayTitle, Code, Holding};
use crate::config::Slug;

#[derive(Debug, Default)]
pub struct BookForm {
    code: Option<Code>,
    cover_image_mimetype: Option<String>,
    category_slug: Option<Slug>,
    title: Option<String>,
    subtitle: Option<String>,
    volume_title: Option<String>,
    volume_number: Option<String>,
    fascicle_number: Option<String>,
    has_been_read: bool,
    last_read_date: Option<Date>,
    authors: Vec<String>,
    editors: Vec<String>,
    translators: Vec<String>,
    holdings: Vec<(Slug, String)>,
    bucket: Option<String>,
    form_errors: Vec<String>,
}

impl std::convert::From<Book> for BookForm {
    fn from(book: Book) -> Self {
        Self {
            code: Some(book.code),
            cover_image_mimetype: book.cover_image_mimetype,
            category_slug: Some(book.category),
            title: Some(book.title),
            subtitle: book.subtitle,
            volume_title: book.volume_title,
            volume_number: book.volume_number,
            fascicle_number: book.fascicle_number,
            has_been_read: book.has_been_read,
            last_read_date: book.last_read_date,
            authors: book.authors,
            editors: book.editors.unwrap_or_default(),
            translators: book.translators.unwrap_or_default(),
            holdings: book
                .holdings
                .into_iter()
                .map(|h| (h.location, h.note.unwrap_or_default()))
                .collect(),
            bucket: Some(book.bucket),
            form_errors: Vec::new(),
        }
    }
}

impl std::convert::TryFrom<&BookForm> for BookDisplayTitle {
    type Error = ();

    fn try_from(form: &BookForm) -> Result<Self, Self::Error> {
        if let Some(title) = form.title.clone() {
            Ok(Self {
                title,
                subtitle: form.subtitle.clone(),
                volume_title: form.volume_title.clone(),
                volume_number: form.volume_number.clone(),
                fascicle_number: form.fascicle_number.clone(),
            })
        } else {
            Err(())
        }
    }
}

impl BookForm {
    pub async fn from_multipart(mut multipart: Multipart) -> Option<(Self, Option<NamedTempFile>)> {
        let mut bf = BookForm::default();
        let mut file = None;
        let mut locations = Vec::new();
        let mut notes = Vec::new();
        while let Some(field) = multipart.next_field().await.ok()? {
            let name = field.name()?.to_string();
            if name == "cover" {
                let (tempfile, mimetype) = handle_upload(field).await?;
                if tempfile.as_file().metadata().unwrap().len() > 0 {
                    file = Some(tempfile);
                    bf.cover_image_mimetype = Some(mimetype);
                }
            } else {
                let text = get_text(field).await?;
                // allow empty notes as there could be a gap in the middle
                if text.is_empty() && name != "notes[]" {
                    continue;
                }
                match name.as_str() {
                    "code" => match text.parse::<Code>() {
                        Ok(code) => bf.code = Some(code),
                        Err(_) => bf
                            .form_errors
                            .push(format!("The code '{text}' is invalid.")),
                    },
                    "category" => bf.category_slug = Some(Slug(text)),
                    "title" => bf.title = Some(text),
                    "subtitle" => bf.subtitle = Some(text),
                    "volume_title" => bf.volume_title = Some(text),
                    "volume_number" => bf.volume_number = Some(text),
                    "fascicle_number" => bf.fascicle_number = Some(text),
                    "has_been_read" => bf.has_been_read = true,
                    "last_read_date" => {
                        match Date::parse(&text, format_description!("[year]-[month]-[day]")) {
                            Ok(date) => bf.last_read_date = Some(date),
                            Err(_) => bf
                                .form_errors
                                .push(format!("The last read date '{text}' is invalid.")),
                        }
                    }
                    "authors[]" => bf.authors.push(text),
                    "translators[]" => bf.translators.push(text),
                    "editors[]" => bf.editors.push(text),
                    "locations[]" => locations.push(text),
                    "notes[]" => notes.push(text),
                    "bucket" => bf.bucket = Some(text),
                    _ => return None,
                }
            }
        }
        bf.holdings = std::iter::zip(locations.into_iter(), notes.into_iter())
            .map(|(l, n)| (Slug(l), n))
            .collect();

        Some((bf, file))
    }

    pub fn to_book(
        self,
        category_fullname_map: &HashMap<Slug, Vec<String>>,
        location_name_map: &HashMap<Slug, String>,
    ) -> Result<Book, (Value, Vec<String>)> {
        let errors = self.validate(category_fullname_map, location_name_map);
        if !errors.is_empty() {
            return Err((
                self.to_context(category_fullname_map, location_name_map),
                errors,
            ));
        }

        let default_bucket = {
            let mut surnames: Vec<String> = self
                .authors
                .iter()
                .map(|a| a.split_whitespace().last().unwrap().to_string())
                .collect();
            surnames.sort();
            surnames[0].clone()
        };

        Ok(Book {
            code: self.code.unwrap(),
            title: self.title.unwrap(),
            subtitle: self.subtitle,
            volume_title: self.volume_title,
            volume_number: self.volume_number,
            fascicle_number: self.fascicle_number,
            authors: self.authors,
            editors: Some(self.editors),
            translators: Some(self.translators),
            has_been_read: self.has_been_read,
            last_read_date: self.last_read_date,
            cover_image_mimetype: self.cover_image_mimetype,
            holdings: self
                .holdings
                .into_iter()
                .map(|(location, n)| Holding {
                    location,
                    note: if n.is_empty() { None } else { Some(n) },
                })
                .collect(),
            bucket: self.bucket.unwrap_or(default_bucket),
            category: self.category_slug.unwrap(),
        })
    }

    pub fn to_context(
        self,
        category_fullname_map: &HashMap<Slug, Vec<String>>,
        location_name_map: &HashMap<Slug, String>,
    ) -> Value {
        let mut holdings = Vec::with_capacity(self.holdings.len());
        for (l, n) in &self.holdings {
            holdings.push(json!({
                "location": location_name_map.get(l),
                "location_slug": l,
                "note": n,
            }));
        }

        json!({
            "has_cover_image": self.cover_image_mimetype.is_some(),
            "display_title": BookDisplayTitle::try_from(&self).ok().map(|t| t.to_string()),
            "code": self.code,
            "title": self.title,
            "subtitle": self.subtitle,
            "volume_title": self.volume_title,
            "volume_number": self.volume_number,
            "fascicle_number": self.fascicle_number,
            "authors": self.authors,
            "translators": self.translators,
            "editors": self.editors,
            "has_been_read": self.has_been_read,
            "last_read_date": self.last_read_date,
            "category": self.category_slug.as_ref().map(|c| category_fullname_map.get(c)),
            "category_slug": self.category_slug,
            "holdings": holdings,
            "bucket": self.bucket,
        })
    }

    pub fn validate(
        &self,
        category_fullname_map: &HashMap<Slug, Vec<String>>,
        location_name_map: &HashMap<Slug, String>,
    ) -> Vec<String> {
        let mut errors = self.form_errors.clone();

        if self.code.is_none() {
            errors.push("The code cannot be blank.".to_string());
        }
        if self.title.is_none() {
            errors.push("The title cannot be blank.".to_string());
        }
        if !self.volume_number.as_deref().map_or(true, is_alnum) {
            errors.push("The volume number can only contain letters and numbers.".to_string());
        }
        if !self.fascicle_number.as_deref().map_or(true, is_alnum) {
            errors.push("The fascicle number can only contain letters and numbers.".to_string());
        }
        if self.authors.is_empty() {
            errors.push("There must be at least one author.".to_string());
        }
        if let Some(slug) = &self.category_slug {
            if !category_fullname_map.contains_key(slug) {
                errors.push(format!("There is no such category '{s}'.", s = slug.0));
            }
        } else {
            errors.push("The category cannot be blank.".to_string());
        }
        for (location, _) in &self.holdings {
            if !location_name_map.contains_key(location) {
                errors.push(format!("There is no such location '{s}'", s = location.0));
            }
        }
        if self.holdings.is_empty() {
            errors.push("There must be at least one holding.".to_string());
        }
        if !self
            .cover_image_mimetype
            .as_deref()
            .map_or(true, is_allowed_image_mimetype)
        {
            errors.push("Cover image must be a JPEG or PNG.".to_string());
        }

        errors
    }
}

async fn get_text(field: multipart::Field<'_>) -> Option<String> {
    let text = field.text().await.ok()?;
    Some(text.trim().to_string())
}

async fn handle_upload(mut field: multipart::Field<'_>) -> Option<(NamedTempFile, String)> {
    let tempfile = NamedTempFile::new().ok()?;
    let std_file = tempfile.reopen().ok()?;
    let mut async_file = AsyncFile::from_std(std_file);

    while let Some(chunk) = field.chunk().await.ok()? {
        async_file.write_all(&chunk).await.ok()?;
    }

    async_file.flush().await.ok()?;

    let mimetype = field.content_type()?;
    Some((tempfile, mimetype.to_string()))
}

fn is_allowed_image_mimetype(ty: &str) -> bool {
    ty == mime::IMAGE_JPEG.essence_str() || ty == mime::IMAGE_PNG.essence_str()
}

fn is_alnum(s: &str) -> bool {
    s.chars().all(char::is_alphanumeric)
}
