pub mod endpoints;
pub mod errors;
pub mod state;

use actix_web::{web, App, HttpServer};
use std::io;
use std::net::SocketAddr;
use std::path::PathBuf;

use crate::config::Config;

pub async fn serve(
    es_host: String,
    address: SocketAddr,
    allow_writes: bool,
    upload_dir: PathBuf,
    config: Config,
) -> io::Result<()> {
    let app_state = web::Data::new(state::AppState::new(
        es_host,
        allow_writes,
        upload_dir,
        config,
    ));

    HttpServer::new(move || {
        let ro_app = App::new()
            .app_data(app_state.clone())
            .service(endpoints::index)
            .service(endpoints::search)
            .service(endpoints::cover)
            .service(endpoints::thumb)
            .service(endpoints::stylesheet);
        if allow_writes {
            ro_app
                .service(endpoints::new_book)
                .service(endpoints::new_book_commit)
                .service(endpoints::delete_book)
                .service(endpoints::delete_book_commit)
                .service(endpoints::edit_book)
                .service(endpoints::edit_book_commit)
        } else {
            ro_app
        }
    })
    .bind(address)?
    .run()
    .await
}
