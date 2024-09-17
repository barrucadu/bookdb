use axum::body::Body;
use axum::http::StatusCode;
use axum::response::{Html, IntoResponse, Response};
use std::fmt;

use crate::book::{Book, Code};

const TEMPLATE: &str = include_str!("_resources/error.html.tera");

#[derive(Debug)]
pub struct Error {
    status_code: StatusCode,
    message: String,
}

impl IntoResponse for Error {
    fn into_response(self) -> Response<Body> {
        let mut context = tera::Context::new();
        context.insert("status_code", &u16::from(self.status_code));
        context.insert("message", &self.message);

        if let Ok(rendered) = tera::Tera::one_off(TEMPLATE, &context, true) {
            (self.status_code, Html(rendered)).into_response()
        } else {
            (self.status_code, self.message.clone()).into_response()
        }
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}: {}", u16::from(self.status_code), self.message)
    }
}

pub fn cannot_connect_to_search_server() -> Error {
    Error {
        status_code: StatusCode::SERVICE_UNAVAILABLE,
        message: "The search server is unavailable.  Try again in a minute or two.".to_string(),
    }
}

pub fn book_does_not_have_cover_image(book: &Book) -> Error {
    Error {
        status_code: StatusCode::NOT_FOUND,
        message: format!(
            "The book '{}' does not have a cover image set.",
            book.inner.code
        ),
    }
}

pub fn book_not_found(code: &Code) -> Error {
    Error {
        status_code: StatusCode::NOT_FOUND,
        message: format!("The book '{code}' does not exist."),
    }
}

pub fn invalid_code(code: &str) -> Error {
    Error {
        status_code: StatusCode::BAD_REQUEST,
        message: format!("The book '{code}' does not exist, as the code is invalid."),
    }
}

pub fn file_not_found() -> Error {
    Error {
        status_code: StatusCode::NOT_FOUND,
        message: "The requested file does not exist.".to_string(),
    }
}

pub fn something_went_wrong() -> Error {
    Error {
        status_code: StatusCode::INTERNAL_SERVER_ERROR,
        message: "Something went wrong.".to_string(),
    }
}

pub fn bad_request() -> Error {
    Error {
        status_code: StatusCode::BAD_REQUEST,
        message: "Bad request.".to_string(),
    }
}
