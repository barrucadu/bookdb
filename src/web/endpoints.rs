use actix_files::NamedFile;
use actix_multipart::form as multipart;
use actix_multipart::form::MultipartForm;
use actix_web::http::header::{ContentDisposition, DispositionType};
use actix_web::{get, post, web, HttpRequest, HttpResponse, Responder};
use lazy_static::lazy_static;
use mime::Mime;
use serde::{Deserialize, Serialize};
use serde_html_form;
use serde_json::{json, Value};
use std::collections::HashMap;
use std::default::Default;
use std::path::PathBuf;
use tera::Tera;
use time::macros::*;
use time::Date;
use tokio::{fs, process};

use crate::book::{Book, Code, Holding};
use crate::config::Slug;
use crate::es;
use crate::web::errors;
use crate::web::state::AppState;

lazy_static! {
    static ref TEMPLATES: Tera = {
        let mut tera = Tera::default();
        let res = tera.add_raw_templates(vec![
            ("delete.html", include_str!("_resources/delete.html.tera")),
            ("edit.html", include_str!("_resources/edit.html.tera")),
            ("notice.html", include_str!("_resources/notice.html.tera")),
            ("search.html", include_str!("_resources/search.html.tera")),
        ]);
        if let Err(error) = res {
            panic!("could not parse templates: {error}");
        }
        tera
    };
}

#[get("/")]
pub async fn index() -> impl Responder {
    web::Redirect::to("/search").permanent()
}

#[derive(Default, Deserialize, Serialize)]
struct FormSearchQuery {
    pub keywords: Option<String>,
    pub r#match: Option<Match>,
    pub location: Option<Slug>,
    pub category: Option<Slug>,
    pub person: Option<String>,
}

#[derive(PartialEq, Eq, Deserialize, Serialize)]
enum Match {
    #[serde(rename = "only-read")]
    OnlyRead,
    #[serde(rename = "only-unread")]
    OnlyUnread,
}

#[get("/search")]
pub async fn search(
    request: HttpRequest,
    state: web::Data<AppState>,
) -> Result<HttpResponse, errors::Error> {
    let query =
        serde_html_form::from_str(request.query_string()).unwrap_or(FormSearchQuery::default());

    let mut context = state.context().await?;
    context.insert("query", &query);

    let result = state.search(query).await?;
    let books: Vec<Value> = result
        .hits
        .into_iter()
        .map(|b| to_book_context(b, &state.category_fullname_map, &state.location_name_map))
        .collect();
    context.insert("num_books", &result.count);
    context.insert("books", &books);
    context.insert("num_authors", &result.aggs.author.len());
    context.insert("num_read", &result.aggs.read);
    context.insert(
        "percent_read",
        &((result.aggs.read as f64 / result.count as f64 * 100.0) as u64),
    );

    render_html("search.html", context)
}

#[get("/book/{code}/cover")]
pub async fn cover(
    state: web::Data<AppState>,
    code: web::Path<String>,
) -> Result<NamedFile, errors::Error> {
    let book = state.get(code.into_inner()).await?;

    if let Some(mime) = book.cover_image_mimetype {
        serve_static_file(
            state.cover_image_path(book.code.clone()),
            mime.parse().unwrap(),
        )
    } else {
        Err(errors::book_does_not_have_cover_image(&book))
    }
}

#[get("/book/{code}/thumb")]
pub async fn thumb(
    state: web::Data<AppState>,
    code: web::Path<String>,
) -> Result<NamedFile, errors::Error> {
    let book = state.get(code.into_inner()).await?;

    if let Some(mime) = book.cover_image_mimetype {
        let thumb_path = state.cover_thumb_path(book.code.clone());
        let cover_path = state.cover_image_path(book.code.clone());
        serve_static_file(thumb_path, mime::IMAGE_JPEG)
            .or(serve_static_file(cover_path, mime.parse().unwrap()))
    } else {
        Err(errors::book_does_not_have_cover_image(&book))
    }
}

#[get("/new")]
pub async fn new_book(state: web::Data<AppState>) -> Result<HttpResponse, errors::Error> {
    let mut context = state.context().await?;
    context.insert("action", "/new");
    context.insert(
        "book",
        &json!({
            "has_cover_image": false,
            "code": "",
            "display_title": "",
            "title": "",
            "subtitle": "",
            "volume_title": "",
            "volume_number": "",
            "fascicle_number": "",
            "authors": Vec::<String>::new(),
            "translators": Vec::<String>::new(),
            "editors": Vec::<String>::new(),
            "has_been_read": false,
            "last_read_date": "",
            "category": "",
            "category_slug": "",
            "holdings": Vec::<Value>::new(),
            "bucket": "",
        }),
    );

    render_html("edit.html", context)
}

#[post("/new")]
pub async fn new_book_commit(
    state: web::Data<AppState>,
    form: MultipartForm<PutBook>,
) -> Result<HttpResponse, errors::Error> {
    match putbook_to_book(
        form.into_inner(),
        &state.category_fullname_map,
        &state.location_name_map,
    ) {
        Ok((book, tempfile)) => {
            let code = book.code.clone();
            state.put(book).await?;
            if let Some(tempfile) = tempfile {
                state.save_cover_image(code, tempfile).await?;
            }

            let mut context = tera::Context::new();
            context.insert("message", "The book has been created.");
            render_html("notice.html", context)
        }
        Err((partial_book, errors)) => {
            let mut context = state.context().await?;
            context.insert("action", "/new");
            context.insert("errors", &errors);
            context.insert("book", &partial_book);
            render_html("edit.html", context)
        }
    }
}

#[get("/book/{code}/delete")]
pub async fn delete_book(
    state: web::Data<AppState>,
    code: web::Path<String>,
) -> Result<HttpResponse, errors::Error> {
    let book = state.get(code.into_inner()).await?;

    let mut context = tera::Context::new();
    context.insert(
        "book",
        &to_book_context(book, &state.category_fullname_map, &state.location_name_map),
    );

    render_html("delete.html", context)
}

#[post("/book/{code}/delete")]
pub async fn delete_book_commit(
    state: web::Data<AppState>,
    code: web::Path<String>,
) -> Result<HttpResponse, errors::Error> {
    let book = state.get(code.into_inner()).await?;
    state.delete(book, true).await?;

    let mut context = tera::Context::new();
    context.insert("message", "The book has been deleted.");

    render_html("notice.html", context)
}

#[get("/book/{code}/edit")]
pub async fn edit_book(
    state: web::Data<AppState>,
    code: web::Path<String>,
) -> Result<HttpResponse, errors::Error> {
    let mut context = state.context().await?;
    context.insert("action", &format!("/book/{code}/edit"));

    let book = state.get(code.into_inner()).await?;
    context.insert(
        "book",
        &to_book_context(book, &state.category_fullname_map, &state.location_name_map),
    );

    render_html("edit.html", context)
}

#[post("/book/{code}/edit")]
pub async fn edit_book_commit(
    state: web::Data<AppState>,
    code: web::Path<String>,
    form: MultipartForm<PutBook>,
) -> Result<HttpResponse, errors::Error> {
    let original = state.get(code.clone()).await?;

    match putbook_to_book(
        form.into_inner(),
        &state.category_fullname_map,
        &state.location_name_map,
    ) {
        Ok((mut book, tempfile)) => {
            book.cover_image_mimetype = book
                .cover_image_mimetype
                .or(original.cover_image_mimetype.clone());
            state.replace(original.clone(), book.clone()).await?;
            if let Some(tempfile) = tempfile {
                state.save_cover_image(book.code.clone(), tempfile).await?;
            }

            let mut context = tera::Context::new();
            context.insert("message", "The book has been updated.");
            render_html("notice.html", context)
        }
        Err((partial_book, errors)) => {
            let mut context = state.context().await?;
            context.insert("action", &format!("/book/{code}/edit"));
            context.insert("errors", &errors);
            context.insert("book", &partial_book);
            render_html("edit.html", context)
        }
    }
}

#[get("/style.css")]
pub async fn stylesheet() -> HttpResponse {
    HttpResponse::Ok()
        .content_type(mime::TEXT_CSS_UTF_8)
        .body(include_str!("_resources/style.css"))
}

fn serve_static_file(path: PathBuf, mime: Mime) -> Result<NamedFile, errors::Error> {
    let file = NamedFile::open(path).map_err(|_| errors::file_not_found())?;

    Ok(file
        .set_content_type(mime)
        .set_content_disposition(ContentDisposition {
            disposition: DispositionType::Inline,
            parameters: vec![],
        }))
}

fn render_html(template: &str, context: tera::Context) -> Result<HttpResponse, errors::Error> {
    match TEMPLATES.render(template, &context) {
        Ok(rendered) => Ok(HttpResponse::Ok()
            .content_type(mime::TEXT_HTML_UTF_8)
            .body(rendered)),
        Err(_) => Err(errors::something_went_wrong()),
    }
}

fn to_book_context(
    book: Book,
    category_fullname_map: &HashMap<Slug, Vec<String>>,
    location_name_map: &HashMap<Slug, String>,
) -> Value {
    let mut holdings = Vec::with_capacity(book.holdings.len());
    for holding in &book.holdings {
        let location_name = location_name_map.get(&holding.location).unwrap();
        holdings.push(json!({
            "location": location_name,
            "location_slug": holding.location,
            "note": holding.note.clone(),
        }));
    }

    json!({
        "has_cover_image": book.cover_image_mimetype.is_some(),
        "code": book.code,
        "display_title": book.display_title(),
        "title": book.title,
        "subtitle": book.subtitle.unwrap_or_default(),
        "volume_title": book.volume_title.unwrap_or_default(),
        "volume_number": book.volume_number.unwrap_or_default(),
        "fascicle_number": book.fascicle_number.unwrap_or_default(),
        "authors": book.authors,
        "translators": book.translators.unwrap_or_default(),
        "editors": book.editors.unwrap_or_default(),
        "has_been_read": book.has_been_read,
        "last_read_date": book.last_read_date,
        "category": category_fullname_map.get(&book.category).unwrap(),
        "category_slug": book.category,
        "holdings": holdings,
        "bucket": book.bucket,
    })
}

impl AppState {
    async fn context(&self) -> Result<tera::Context, errors::Error> {
        let mut context = tera::Context::new();
        context.insert("allow_writes", &self.allow_writes);
        context.insert("locations", &self.locations);
        context.insert("categories", &self.categories);

        let result = self.search(FormSearchQuery::default()).await?;

        let mut authors: Vec<String> = result.aggs.author.keys().cloned().collect();
        authors.sort();
        context.insert("authors", &authors);

        let mut translators: Vec<String> = result.aggs.translator.keys().cloned().collect();
        translators.sort();
        context.insert("translators", &translators);

        let mut editors: Vec<String> = result.aggs.editor.keys().cloned().collect();
        editors.sort();
        context.insert("editors", &editors);

        let mut people: Vec<String> =
            Vec::with_capacity(authors.len() + translators.len() + editors.len());
        people.extend(authors.clone());
        people.extend(translators.clone());
        people.extend(editors.clone());
        people.sort();
        people.dedup();
        context.insert("people", &people);

        Ok(context)
    }

    async fn search(&self, query: FormSearchQuery) -> Result<es::SearchResult, errors::Error> {
        let categories = if let Some(cat) = query.category {
            let mut out = vec![cat.clone()];
            if let Some(children) = self.category_children_map.get(&cat) {
                out.append(&mut children.clone());
            }
            out
        } else {
            Vec::new()
        };

        let es_query = es::SearchQuery {
            keywords: query.keywords,
            read: match query.r#match {
                Some(Match::OnlyRead) => Some(true),
                Some(Match::OnlyUnread) => Some(false),
                None => None,
            },
            location: query.location,
            categories,
            people: query.person.into_iter().collect(),
        };

        let client = self
            .elasticsearch()
            .map_err(|_| errors::cannot_connect_to_search_server())?;
        es::search(&client, es_query)
            .await
            .map_err(|_| errors::cannot_connect_to_search_server())
    }

    async fn get(&self, code: String) -> Result<Book, errors::Error> {
        let code: Code = code.parse().map_err(|_| errors::invalid_code(&code))?;
        let client = self
            .elasticsearch()
            .map_err(|_| errors::cannot_connect_to_search_server())?;
        let result = es::get(&client, code.clone())
            .await
            .map_err(|_| errors::cannot_connect_to_search_server())?;

        if let Some(book) = result {
            Ok(book)
        } else {
            Err(errors::book_not_found(&code))
        }
    }

    async fn put(&self, book: Book) -> Result<(), errors::Error> {
        let client = self
            .elasticsearch()
            .map_err(|_| errors::cannot_connect_to_search_server())?;

        es::put(&client, book)
            .await
            .map_err(|_| errors::cannot_connect_to_search_server())?;

        Ok(())
    }

    async fn delete(&self, book: Book, delete_files: bool) -> Result<(), errors::Error> {
        let client = self
            .elasticsearch()
            .map_err(|_| errors::cannot_connect_to_search_server())?;

        es::delete(&client, book.code.clone())
            .await
            .map_err(|_| errors::cannot_connect_to_search_server())?;

        if book.cover_image_mimetype.is_some() && delete_files {
            fs::remove_file(self.cover_image_path(book.code.clone()))
                .await
                .map_err(|_| errors::something_went_wrong())?;
            fs::remove_file(self.cover_thumb_path(book.code.clone()))
                .await
                .map_err(|_| errors::something_went_wrong())?;
        }

        Ok(())
    }

    async fn replace(&self, from: Book, to: Book) -> Result<(), errors::Error> {
        let old_images_exist = from.cover_image_mimetype.is_some();
        let new_images_exist = to.cover_image_mimetype.is_some();

        self.delete(from.clone(), !new_images_exist).await?;
        self.put(to.clone()).await?;

        if from.code != to.code && old_images_exist && new_images_exist {
            fs::rename(
                self.cover_image_path(from.code.clone()),
                self.cover_image_path(to.code.clone()),
            )
            .await
            .map_err(|_| errors::something_went_wrong())?;
            fs::rename(
                self.cover_thumb_path(from.code.clone()),
                self.cover_thumb_path(to.code.clone()),
            )
            .await
            .map_err(|_| errors::something_went_wrong())?;
        }

        Ok(())
    }

    async fn save_cover_image(
        &self,
        code: Code,
        file: multipart::tempfile::TempFile,
    ) -> Result<(), errors::Error> {
        let tmp_path = file.file.path();
        let cover_path = self.cover_image_path(code.clone());
        let thumb_path = self.cover_thumb_path(code.clone());

        // copy / remove to handle the case where the temporary directory is on
        // a different mountpoint (e.g. a tmpfs)
        fs::copy(tmp_path, cover_path.clone())
            .await
            .map_err(|_| errors::something_went_wrong())?;

        fs::remove_file(tmp_path)
            .await
            .map_err(|_| errors::something_went_wrong())?;

        tokio::spawn(generate_thumbnail_task(
            cover_path.to_string_lossy().into_owned(),
            thumb_path.to_string_lossy().into_owned(),
        ));

        Ok(())
    }
}

async fn generate_thumbnail_task(cover_path: String, thumb_path: String) {
    let cmd = process::Command::new("convert")
        .args([&cover_path, "-resize", "16x24", &thumb_path])
        .spawn();

    match cmd {
        Ok(mut imagemagick) => match imagemagick.wait().await {
            Ok(exit_code) => {
                if !exit_code.success() {
                    tracing::warn!(
                        ?cover_path,
                        ?thumb_path,
                        ?exit_code,
                        "imagemagick process error"
                    );
                }
            }
            Err(error) => tracing::warn!(
                ?cover_path,
                ?thumb_path,
                ?error,
                "imagemagick process error"
            ),
        },
        Err(error) => tracing::warn!(
            ?cover_path,
            ?thumb_path,
            ?error,
            "could not spawn imagemagick process"
        ),
    }
}

///////////////////////////////////////////////////////////////////////////////
// MULTIPART FORM HANDLING (add / edit)
//
// This is horrible, surely there's a better way to do this.

#[derive(Debug, MultipartForm)]
struct PutBook {
    code: Option<multipart::text::Text<String>>,
    #[multipart(limit = "10MiB")]
    cover: Option<multipart::tempfile::TempFile>,
    category: Option<multipart::text::Text<String>>,
    title: Option<multipart::text::Text<String>>,
    subtitle: Option<multipart::text::Text<String>>,
    volume_title: Option<multipart::text::Text<String>>,
    volume_number: Option<multipart::text::Text<String>>,
    fascicle_number: Option<multipart::text::Text<String>>,
    has_been_read: Option<multipart::text::Text<String>>,
    last_read_date: Option<multipart::text::Text<String>>,
    #[multipart(rename = "authors[]")]
    authors: Vec<multipart::text::Text<String>>,
    #[multipart(rename = "editors[]")]
    editors: Vec<multipart::text::Text<String>>,
    #[multipart(rename = "translators[]")]
    translators: Vec<multipart::text::Text<String>>,
    #[multipart(rename = "locations[]")]
    locations: Vec<multipart::text::Text<String>>,
    #[multipart(rename = "notes[]")]
    notes: Vec<multipart::text::Text<String>>,
    bucket: Option<multipart::text::Text<String>>,
}

// this has to return something that can be stuck into a template context, so
// that I don't need to type out all the details again just because I typo'd the
// ISBN.
fn validate_putbook(
    form: PutBook,
    category_fullname_map: &HashMap<Slug, Vec<String>>,
    location_name_map: &HashMap<Slug, String>,
) -> (Value, Option<multipart::tempfile::TempFile>, Vec<String>) {
    let mut errors = Vec::new();
    let code = multipart_str(form.code);
    let title = multipart_str(form.title);
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

    let book_json = json!({
        "code": code,
        "title": title,
        "subtitle": multipart_str(form.subtitle),
        "volume_title": multipart_str(form.volume_title),
        "volume_number": multipart_str(form.volume_number),
        "fascicle_number": multipart_str(form.fascicle_number),
        "authors": authors.unwrap_or_default(),
        "translators": multipart_vec(form.translators).unwrap_or_default(),
        "editors": multipart_vec(form.editors).unwrap_or_default(),
        "has_been_read": form.has_been_read.is_some(),
        "last_read_date": last_read_date,
        "category_slug": category_slug,
        "holdings": holdings,
        "bucket": multipart_str(form.bucket),
    });

    (book_json, form.cover, errors)
}

fn putbook_to_book(
    form: PutBook,
    category_fullname_map: &HashMap<Slug, Vec<String>>,
    location_name_map: &HashMap<Slug, String>,
) -> Result<(Book, Option<multipart::tempfile::TempFile>), (Value, Vec<String>)> {
    let (book_json, cover_image, errors) =
        validate_putbook(form, category_fullname_map, location_name_map);

    if !errors.is_empty() {
        return Err((book_json, errors));
    }

    let cover_image = if let Some(tempfile) = cover_image {
        if tempfile.size > 0 {
            match tempfile.content_type {
                Some(ref mime) if allowed_image_type(mime) => Ok(Some(tempfile)),
                Some(_) => Err("Cover image must be a JPEG or PNG.".to_string()),
                None => Ok(None),
            }
        } else {
            Ok(None)
        }
    } else {
        Ok(None)
    };

    match cover_image {
        Ok(tempfile) => {
            let nonempty_strvec = |s: Vec<String>| if s.is_empty() { None } else { Some(s) };
            let nonempty_str = |s: String| if s.is_empty() { None } else { Some(s) };
            // all the unsafe stuff is fine here because of the prior validation
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

            let code = get_json_str("code").unwrap().parse().unwrap();
            let title = get_json_str("title").unwrap();
            let authors: Vec<String> = get_json_strvec("authors");
            let has_been_read = book_json["has_been_read"].as_bool().unwrap();
            let last_read_date = get_json_str("last_read_date")
                .and_then(|s| Date::parse(&s, format_description!("[year]-[month]-[day]")).ok());
            let cover_image_mimetype = tempfile
                .as_ref()
                .map(|f| f.content_type.clone().unwrap().essence_str().to_string());
            let holdings = {
                let arr = book_json["holdings"].as_array().unwrap();
                let mut out = Vec::with_capacity(arr.len());
                for h in arr {
                    let location = Slug(json_str(h["location_slug"].clone()).unwrap());
                    let note = json_str(h["notes"].clone());
                    out.push(Holding { location, note });
                }
                out
            };
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
            let category = Slug(get_json_str("category_slug").unwrap());
            let book = Book {
                code,
                title,
                subtitle: get_json_str("subtitle").and_then(nonempty_str),
                volume_title: get_json_str("volume_title").and_then(nonempty_str),
                volume_number: get_json_str("volume_number").and_then(nonempty_str),
                fascicle_number: get_json_str("fascicle_number").and_then(nonempty_str),
                authors,
                editors: nonempty_strvec(get_json_strvec("editors")),
                translators: nonempty_strvec(get_json_strvec("translators")),
                has_been_read,
                last_read_date,
                cover_image_mimetype,
                holdings,
                bucket,
                category,
            };
            Ok((book, tempfile))
        }
        Err(error) => Err((book_json, vec![error])),
    }
}

fn allowed_image_type(mime: &Mime) -> bool {
    let ty = mime.type_();
    let subty = mime.subtype();
    ty == mime::IMAGE && (subty == mime::JPEG || subty == mime::PNG)
}

fn multipart_str(txt: Option<multipart::text::Text<String>>) -> Option<String> {
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

fn multipart_vec(vec: Vec<multipart::text::Text<String>>) -> Option<Vec<String>> {
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
