use actix_files::NamedFile;
use actix_web::http::header::{ContentDisposition, DispositionType};
use actix_web::{
    error, get, post, web, App, Error, HttpRequest, HttpResponse, HttpServer, Responder,
};
use elasticsearch::http::transport::Transport;
use elasticsearch::Elasticsearch;
use mime::Mime;
use serde_html_form;
use std::io;
use std::net::SocketAddr;
use std::path::PathBuf;

use crate::book::{Book, Code};
use crate::config::Config;
use crate::index as es;

#[get("/")]
async fn index() -> impl Responder {
    web::Redirect::to("/search").permanent()
}

#[get("/search")]
async fn search(request: HttpRequest, data: web::Data<AppState>) -> impl Responder {
    match serde_html_form::from_str(request.query_string()) {
        Ok(query) => match data.search(query).await {
            Ok(response) => HttpResponse::Ok().json(response),
            Err(error) => error::ErrorInternalServerError(error.to_string()).into(),
        },
        Err(error) => error::ErrorBadRequest(error.to_string()).into(),
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
    match data.elasticsearch() {
        Ok(_es) => HttpResponse::Ok().body("create book form"),
        Err(error) => error::ErrorInternalServerError(error.to_string()).into(),
    }
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
    match data.elasticsearch() {
        Ok(_es) => HttpResponse::Ok().body(format!("edit book '{code}")),
        Err(error) => error::ErrorInternalServerError(error.to_string()).into(),
    }
}

#[post("/book/{code}/edit")]
async fn edit_book_commit(data: web::Data<AppState>, code: web::Path<String>) -> impl Responder {
    match data.elasticsearch() {
        Ok(_es) => HttpResponse::Ok().body(format!("actually edit book '{code}'")),
        Err(error) => error::ErrorInternalServerError(error.to_string()).into(),
    }
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
    config: Config,
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

    async fn search(
        &self,
        query: es::SearchQuery,
    ) -> Result<es::SearchResult, elasticsearch::Error> {
        let client = self.elasticsearch()?;
        es::search(&client, query).await
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
    let app_state = web::Data::new(AppState {
        es_host,
        allow_writes,
        upload_dir,
        config,
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
        })
        .bind(address)?
        .run()
        .await
    }
}
