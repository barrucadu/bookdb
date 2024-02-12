use actix_web::error as actix;
use actix_web::http::StatusCode;
use actix_web::HttpResponse;
use std::fmt;

use crate::book::{Book, Code};

const TEMPLATE: &str = include_str!("_resources/error.html.tera");

#[derive(Debug)]
pub struct Error {
    status_code: StatusCode,
    message: String,
}

impl Error {
    pub fn html_error_response(&self) -> Result<HttpResponse, tera::Error> {
        let mut context = tera::Context::new();
        context.insert("status_code", &u16::from(self.status_code));
        context.insert("message", &self.message);

        let rendered = tera::Tera::one_off(TEMPLATE, &context, true)?;
        Ok(HttpResponse::build(self.status_code)
            .content_type(mime::TEXT_HTML_UTF_8)
            .body(rendered))
    }

    pub fn fallback_error_response(&self) -> HttpResponse {
        HttpResponse::build(self.status_code)
            .content_type(mime::TEXT_PLAIN_UTF_8)
            .body(self.message.clone())
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}: {}", u16::from(self.status_code), self.message)
    }
}

impl actix::ResponseError for Error {
    fn status_code(&self) -> StatusCode {
        self.status_code
    }

    fn error_response(&self) -> HttpResponse {
        self.html_error_response()
            .unwrap_or(self.fallback_error_response())
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
        message: format!("The book '{}' does not have a cover image set.", book.code),
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
