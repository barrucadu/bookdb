use actix_files::NamedFile;
use actix_web::http::header::{ContentDisposition, DispositionType};
use actix_web::{
    error, get, post, web, App, Error, HttpRequest, HttpResponse, HttpServer, Responder,
};
use elasticsearch::http::transport::Transport;
use elasticsearch::Elasticsearch;
use lazy_static::lazy_static;
use mime::Mime;
use serde::{Deserialize, Serialize};
use serde_html_form;
use serde_json::{json, Value};
use std::collections::HashMap;
use std::default::Default;
use std::io;
use std::net::SocketAddr;
use std::path::PathBuf;
use tera::Tera;

use crate::book::{Book, Code};
use crate::config::{Config, NameSlug, Slug};
use crate::index as es;

lazy_static! {
    static ref TEMPLATES: Tera = {
        let mut tera = Tera::default();
        let res = tera.add_raw_templates(vec![
            ("edit.html", include_str!("resources/edit.html.tera")),
            ("search.html", include_str!("resources/search.html.tera")),
        ]);
        if let Err(error) = res {
            panic!("could not parse templates: {error}");
        }
        tera
    };
}

#[get("/")]
async fn index() -> impl Responder {
    web::Redirect::to("/search").permanent()
}

#[derive(Default, Deserialize, Serialize)]
pub struct FormSearchQuery {
    pub keywords: Option<String>,
    pub r#match: Option<Match>,
    pub location: Option<Slug>,
    pub category: Option<Slug>,
    pub person: Option<String>,
}

#[derive(PartialEq, Eq, Deserialize, Serialize)]
pub enum Match {
    #[serde(rename = "only-read")]
    OnlyRead,
    #[serde(rename = "only-unread")]
    OnlyUnread,
}

#[get("/search")]
async fn search(request: HttpRequest, data: web::Data<AppState>) -> impl Responder {
    let query =
        serde_html_form::from_str(request.query_string()).unwrap_or(FormSearchQuery::default());
    let (mut context, ok) = data.context().await;
    context.insert("query", &query);

    if !ok {
        return error::ErrorInternalServerError("could not connect to search server").into();
    }

    match data.search(query).await {
        Ok(result) => {
            let books: Vec<Value> = result
                .hits
                .into_iter()
                .map(|b| to_book_context(b, &data.category_fullname_map, &data.location_name_map))
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
            HttpResponse::Ok()
                .content_type(mime::TEXT_HTML_UTF_8)
                .body(rendered)
        }
        Err(error) => error::ErrorInternalServerError(error.to_string()).into(),
    }
}

#[get("/book/{code}/cover")]
async fn cover(data: web::Data<AppState>, code: web::Path<String>) -> Result<NamedFile, Error> {
    let book = find_book_by_code(&data, code).await?;

    if let Some(mime) = book.cover_image_mimetype {
        serve_book_image(
            data.cover_image_path(book.code.clone()),
            mime.parse().unwrap(),
        )
    } else {
        Err(error::ErrorNotFound("not found"))
    }
}

#[get("/book/{code}/thumb")]
async fn thumb(data: web::Data<AppState>, code: web::Path<String>) -> Result<NamedFile, Error> {
    let book = find_book_by_code(&data, code).await?;

    if let Some(mime) = book.cover_image_mimetype {
        let thumb_path = data.cover_thumb_path(book.code.clone());
        let cover_path = data.cover_image_path(book.code.clone());
        serve_book_image(thumb_path, mime::IMAGE_JPEG)
            .or(serve_book_image(cover_path, mime.parse().unwrap()))
    } else {
        Err(error::ErrorNotFound("not found"))
    }
}

#[get("/new")]
async fn new_book(data: web::Data<AppState>) -> impl Responder {
    let (mut context, ok) = data.context().await;
    if !ok {
        return error::ErrorInternalServerError("could not connect to search server").into();
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
async fn new_book_commit(data: web::Data<AppState>) -> impl Responder {
    match data.elasticsearch() {
        Ok(_es) => HttpResponse::Ok().body("actually create a book"),
        Err(error) => error::ErrorInternalServerError(error.to_string()).into(),
    }
}

#[get("/book/{code}/delete")]
async fn delete_book(data: web::Data<AppState>, code: web::Path<String>) -> impl Responder {
    match data.elasticsearch() {
        Ok(_es) => HttpResponse::Ok().body(format!("delete book '{code}'")),
        Err(error) => error::ErrorInternalServerError(error.to_string()).into(),
    }
}

#[post("/book/{code}/delete")]
async fn delete_book_commit(data: web::Data<AppState>, code: web::Path<String>) -> impl Responder {
    match data.elasticsearch() {
        Ok(_es) => HttpResponse::Ok().body(format!("actually delete book '{code}'")),
        Err(error) => error::ErrorInternalServerError(error.to_string()).into(),
    }
}

#[get("/book/{code}/edit")]
async fn edit_book(data: web::Data<AppState>, code: web::Path<String>) -> impl Responder {
    let (mut context, ok) = data.context().await;
    if !ok {
        return error::ErrorInternalServerError("could not connect to search server").into();
    }

    context.insert("action", &format!("/book/{code}/edit"));

    match find_book_by_code(&data, code).await {
        Ok(book) => {
            context.insert(
                "book",
                &to_book_context(book, &data.category_fullname_map, &data.location_name_map),
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
async fn edit_book_commit(data: web::Data<AppState>, code: web::Path<String>) -> impl Responder {
    match data.elasticsearch() {
        Ok(_es) => HttpResponse::Ok().body(format!("actually edit book '{code}'")),
        Err(error) => error::ErrorInternalServerError(error.to_string()).into(),
    }
}

#[get("/style.css")]
async fn stylesheet() -> HttpResponse {
    HttpResponse::Ok()
        .content_type(mime::TEXT_CSS_UTF_8)
        .body(include_str!("resources/style.css"))
}

async fn find_book_by_code(data: &AppState, code: web::Path<String>) -> Result<Book, Error> {
    match code.parse() {
        Ok(code) => match data.get(code).await {
            Ok(Some(book)) => Ok(book),
            _ => Err(error::ErrorNotFound("not found")),
        },
        _ => Err(error::ErrorBadRequest("invalid code")),
    }
}

fn serve_book_image(path: PathBuf, mime: Mime) -> Result<NamedFile, Error> {
    let file = NamedFile::open(path).map_err(|_| error::ErrorNotFound("not found"))?;

    Ok(file
        .set_content_type(mime)
        .set_content_disposition(ContentDisposition {
            disposition: DispositionType::Inline,
            parameters: vec![],
        }))
}

#[allow(dead_code)]
struct AppState {
    es_host: String,
    allow_writes: bool,
    upload_dir: PathBuf,
    category_fullname_map: HashMap<Slug, Vec<String>>,
    category_children_map: HashMap<Slug, Vec<Slug>>,
    categories: Vec<NameSlug>,
    location_name_map: HashMap<Slug, String>,
    locations: Vec<NameSlug>,
}

impl AppState {
    fn elasticsearch(&self) -> Result<Elasticsearch, elasticsearch::Error> {
        Transport::single_node(&self.es_host).map(Elasticsearch::new)
    }

    fn cover_image_path(&self, code: Code) -> PathBuf {
        let mut path = self.upload_dir.clone();
        path.push(code.to_string());
        path
    }

    fn cover_thumb_path(&self, code: Code) -> PathBuf {
        let mut path = self.upload_dir.clone();
        path.push("thumbs");
        path.push(format!("{code}.jpg"));
        path
    }

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

    async fn search(
        &self,
        query: FormSearchQuery,
    ) -> Result<es::SearchResult, elasticsearch::Error> {
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

        let client = self.elasticsearch()?;
        es::search(&client, es_query).await
    }

    async fn get(&self, code: Code) -> Result<Option<Book>, elasticsearch::Error> {
        let client = self.elasticsearch()?;
        es::get(&client, code).await
    }
}

pub async fn serve(
    es_host: String,
    address: SocketAddr,
    allow_writes: bool,
    upload_dir: PathBuf,
    config: Config,
) -> io::Result<()> {
    let category_fullname_map = category_fullname_map(
        &config.category_slugs(),
        &config.category_name_map(),
        &config.category_parents_map(),
    );
    let categories = config
        .category_slugs()
        .into_iter()
        .map(|s| NameSlug {
            name: category_fullname_map.get(&s).unwrap().join(" / "),
            slug: s.clone(),
        })
        .collect();
    let app_state = web::Data::new(AppState {
        es_host,
        allow_writes,
        upload_dir,
        category_fullname_map,
        category_children_map: config.category_children_map(),
        categories,
        location_name_map: config.location_name_map(),
        locations: config.locations.clone(),
    });

    if allow_writes {
        HttpServer::new(move || {
            App::new()
                .app_data(app_state.clone())
                .service(index)
                .service(search)
                .service(cover)
                .service(thumb)
                .service(new_book)
                .service(new_book_commit)
                .service(delete_book)
                .service(delete_book_commit)
                .service(edit_book)
                .service(edit_book_commit)
                .service(stylesheet)
        })
        .bind(address)?
        .run()
        .await
    } else {
        HttpServer::new(move || {
            App::new()
                .app_data(app_state.clone())
                .service(index)
                .service(search)
                .service(cover)
                .service(thumb)
                .service(stylesheet)
        })
        .bind(address)?
        .run()
        .await
    }
}

fn category_fullname_map(
    slugs: &[Slug],
    names: &HashMap<Slug, String>,
    parents: &HashMap<Slug, Vec<Slug>>,
) -> HashMap<Slug, Vec<String>> {
    let mut out = HashMap::with_capacity(slugs.len());
    for slug in slugs {
        let mut names: Vec<String> = parents
            .get(slug)
            .unwrap()
            .clone()
            .iter()
            .map(|slug| names.get(slug).unwrap().clone())
            .collect();
        names.reverse();
        out.insert(slug.clone(), names);
    }
    out
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
