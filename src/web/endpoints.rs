use actix_files::NamedFile;
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

use crate::book::Book;
use crate::config::Slug;
use crate::es;
use crate::web::errors;
use crate::web::state::AppState;

lazy_static! {
    static ref TEMPLATES: Tera = {
        let mut tera = Tera::default();
        let res = tera.add_raw_templates(vec![
            ("edit.html", include_str!("_resources/edit.html.tera")),
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
    let (mut context, ok) = state.context().await;
    context.insert("query", &query);

    if !ok {
        return Err(errors::cannot_connect_to_search_server());
    }

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

    let rendered = TEMPLATES.render("search.html", &context).unwrap();
    Ok(HttpResponse::Ok()
        .content_type(mime::TEXT_HTML_UTF_8)
        .body(rendered))
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
        Err(errors::book_does_not_have_cover_image())
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
        Err(errors::book_does_not_have_cover_image())
    }
}

#[get("/new")]
pub async fn new_book(state: web::Data<AppState>) -> impl Responder {
    let (mut context, ok) = state.context().await;
    if !ok {
        return errors::cannot_connect_to_search_server().into();
    }

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

    let rendered = TEMPLATES.render("edit.html", &context).unwrap();
    HttpResponse::Ok()
        .content_type(mime::TEXT_HTML_UTF_8)
        .body(rendered)
}

#[post("/new")]
pub async fn new_book_commit(state: web::Data<AppState>) -> impl Responder {
    match state.elasticsearch() {
        Ok(_es) => HttpResponse::Ok().body("actually create a book"),
        Err(_) => errors::cannot_connect_to_search_server().into(),
    }
}

#[get("/book/{code}/delete")]
pub async fn delete_book(state: web::Data<AppState>, code: web::Path<String>) -> impl Responder {
    match state.elasticsearch() {
        Ok(_es) => HttpResponse::Ok().body(format!("delete book '{code}'")),
        Err(_) => errors::cannot_connect_to_search_server().into(),
    }
}

#[post("/book/{code}/delete")]
pub async fn delete_book_commit(
    state: web::Data<AppState>,
    code: web::Path<String>,
) -> impl Responder {
    match state.elasticsearch() {
        Ok(_es) => HttpResponse::Ok().body(format!("actually delete book '{code}'")),
        Err(_) => errors::cannot_connect_to_search_server().into(),
    }
}

#[get("/book/{code}/edit")]
pub async fn edit_book(state: web::Data<AppState>, code: web::Path<String>) -> impl Responder {
    let (mut context, ok) = state.context().await;
    if !ok {
        return errors::cannot_connect_to_search_server().into();
    }

    context.insert("action", &format!("/book/{code}/edit"));

    match state.get(code.into_inner()).await {
        Ok(book) => {
            context.insert(
                "book",
                &to_book_context(book, &state.category_fullname_map, &state.location_name_map),
            );
            let rendered = TEMPLATES.render("edit.html", &context).unwrap();
            HttpResponse::Ok()
                .content_type(mime::TEXT_HTML_UTF_8)
                .body(rendered)
        }
        Err(error) => error.into(),
    }
}

#[post("/book/{code}/edit")]
pub async fn edit_book_commit(
    state: web::Data<AppState>,
    code: web::Path<String>,
) -> impl Responder {
    match state.elasticsearch() {
        Ok(_es) => HttpResponse::Ok().body(format!("actually edit book '{code}'")),
        Err(_) => errors::cannot_connect_to_search_server().into(),
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
    async fn context(&self) -> (tera::Context, bool) {
        let mut context = tera::Context::new();
        context.insert("allow_writes", &self.allow_writes);
        context.insert("locations", &self.locations);
        context.insert("categories", &self.categories);

        if let Ok(result) = self.search(FormSearchQuery::default()).await {
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

            (context, true)
        } else {
            (context, false)
        }
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
        let code = code.parse().map_err(|_| errors::invalid_code())?;
        let client = self
            .elasticsearch()
            .map_err(|_| errors::cannot_connect_to_search_server())?;
        let result = es::get(&client, code)
            .await
            .map_err(|_| errors::cannot_connect_to_search_server())?;

        if let Some(book) = result {
            Ok(book)
        } else {
            Err(errors::book_not_found())
        }
    }
}
