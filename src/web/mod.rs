pub mod endpoints;
pub mod errors;
pub mod model;
pub mod state;

use axum::extract::DefaultBodyLimit;
use axum::{routing, Router};
use std::net::SocketAddr;
use std::path::PathBuf;

use crate::config::Config;

pub async fn serve(
    es_host: String,
    address: SocketAddr,
    allow_writes: bool,
    upload_dir: PathBuf,
    config: Config,
) -> std::io::Result<()> {
    let app_state = state::AppState::new(es_host, allow_writes, upload_dir, &config);

    let app = routes(allow_writes)
        .fallback(fallback_404)
        .layer(DefaultBodyLimit::max(10 * 1024 * 1024))
        .with_state(app_state);
    let listener = tokio::net::TcpListener::bind(address).await?;
    axum::serve(listener, app).await?;

    Ok(())
}

fn routes(allow_writes: bool) -> Router<state::AppState> {
    let ro_app = Router::new()
        .route("/", routing::get(endpoints::index))
        .route("/search", routing::get(endpoints::search))
        .route("/book/{code}/cover", routing::get(endpoints::cover))
        .route("/book/{code}/thumb", routing::get(endpoints::thumb))
        .route("/style.css", routing::get(endpoints::stylesheet));

    if allow_writes {
        ro_app
            .route("/new", routing::get(endpoints::new_book))
            .route("/new", routing::post(endpoints::new_book_commit))
            .route("/book/{code}/delete", routing::get(endpoints::delete_book))
            .route(
                "/book/{code}/delete",
                routing::post(endpoints::delete_book_commit),
            )
            .route("/book/{code}/edit", routing::get(endpoints::edit_book))
            .route(
                "/book/{code}/edit",
                routing::post(endpoints::edit_book_commit),
            )
    } else {
        ro_app
    }
}

// this needs to be `async` for `fallback` above to work
async fn fallback_404() -> errors::Error {
    errors::file_not_found()
}
