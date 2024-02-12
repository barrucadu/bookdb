pub use actix_web::Error;

pub fn cannot_connect_to_search_server() -> Error {
    actix_web::error::ErrorServiceUnavailable("could not connect to search server")
}

pub fn book_does_not_have_cover_image() -> Error {
    actix_web::error::ErrorNotFound("book does not have cover image")
}

pub fn book_not_found() -> Error {
    actix_web::error::ErrorNotFound("book not found")
}

pub fn invalid_code() -> Error {
    actix_web::error::ErrorBadRequest("invalid code")
}

pub fn file_not_found() -> Error {
    actix_web::error::ErrorNotFound("file not found")
}
