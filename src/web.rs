use actix_web::{error, get, post, web, App, HttpResponse, HttpServer, Responder};
use elasticsearch::http::transport::Transport;
use elasticsearch::Elasticsearch;
use std::io;
use std::net::SocketAddr;
use std::path::PathBuf;

use crate::config::Config;

#[get("/")]
async fn index() -> impl Responder {
    HttpResponse::Ok().body("redirect to /search")
}

#[get("/search")]
async fn search(data: web::Data<AppState>) -> impl Responder {
    match data.elasticsearch() {
        Ok(_es) => HttpResponse::Ok().body("search"),
        Err(error) => error::ErrorInternalServerError(error.to_string()).into(),
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
        })
        .bind(address)?
        .run()
        .await
    }
}
