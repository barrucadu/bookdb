use axum::body::Body;
use axum::extract::{Multipart, Path, RawQuery, State};
use axum::http::header;
use axum::response::{Html, IntoResponse, Redirect, Response};
use lazy_static::lazy_static;
use mime;
use serde::{Deserialize, Serialize};
use serde_json::Value;
use std::default::Default;
use std::path::PathBuf;
use tempfile::NamedTempFile;
use tera::Tera;
use tokio_util::io::ReaderStream;

use crate::book::{Book, Code};
use crate::config::Slug;
use crate::es;
use crate::web::errors;
use crate::web::model::BookForm;
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

pub async fn index() -> Redirect {
    Redirect::permanent("/search")
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

pub async fn search(
    RawQuery(query_string): RawQuery,
    State(state): State<AppState>,
) -> Result<Html<String>, errors::Error> {
    let query = query_string
        .and_then(|s| serde_html_form::from_str(&s).ok())
        .unwrap_or(FormSearchQuery::default());

    let mut context = state.context().await?;
    context.insert("query", &query);

    let result = state.search(query).await?;
    let books: Vec<Value> = result
        .hits
        .into_iter()
        .map(|b| state.book_to_context(b))
        .collect();
    context.insert("num_books", &result.count);
    context.insert("books", &books);
    context.insert("num_authors", &result.aggs.author.len());
    context.insert("num_read", &result.aggs.read);
    context.insert("percent_read", &percent(result.aggs.read, result.count));

    render_html("search.html", &context)
}

pub async fn cover(Path(code): Path<String>, State(state): State<AppState>) -> impl IntoResponse {
    let book = state.get(code).await?;
    if let Some(mime) = book.cover_image_mimetype {
        serve_static_file(state.cover_image_path(&book.code), mime.parse().unwrap()).await
    } else {
        Err(errors::book_does_not_have_cover_image(&book))
    }
}

pub async fn thumb(Path(code): Path<String>, State(state): State<AppState>) -> impl IntoResponse {
    let book = state.get(code).await?;

    if let Some(mime) = book.cover_image_mimetype {
        serve_static_file(state.cover_thumb_path(&book.code), mime::IMAGE_JPEG)
            .await
            .or(serve_static_file(state.cover_image_path(&book.code), mime.parse().unwrap()).await)
    } else {
        Err(errors::book_does_not_have_cover_image(&book))
    }
}

pub async fn new_book(State(state): State<AppState>) -> Result<Html<String>, errors::Error> {
    render_book_form(
        &state,
        "/new",
        &state.bookform_to_context(BookForm::default()),
        &[],
    )
    .await
}

pub async fn new_book_commit(
    State(state): State<AppState>,
    form: Multipart,
) -> Result<Html<String>, errors::Error> {
    match BookForm::from_multipart(form).await {
        Some((bookform, tempfile)) => {
            match bookform.to_book(&state.category_fullname_map, &state.location_name_map) {
                Ok(book) => {
                    let code = book.code.clone();
                    state.put(book).await?;
                    if let Some(tempfile) = tempfile {
                        state.save_cover_image(code, tempfile).await?;
                    }

                    let mut context = tera::Context::new();
                    context.insert("message", "The book has been created.");
                    render_html("notice.html", &context)
                }
                Err((book_context, errors)) => {
                    render_book_form(&state, "/new", &book_context, &errors).await
                }
            }
        }
        None => Err(errors::bad_request()),
    }
}

pub async fn delete_book(
    Path(code): Path<String>,
    State(state): State<AppState>,
) -> Result<Html<String>, errors::Error> {
    let book = state.get(code).await?;

    let mut context = tera::Context::new();
    context.insert("book", &state.book_to_context(book));

    render_html("delete.html", &context)
}

pub async fn delete_book_commit(
    Path(code): Path<String>,
    State(state): State<AppState>,
) -> Result<Html<String>, errors::Error> {
    let book = state.get(code).await?;
    state.delete(book, true).await?;

    let mut context = tera::Context::new();
    context.insert("message", "The book has been deleted.");

    render_html("notice.html", &context)
}

pub async fn edit_book(
    Path(code): Path<String>,
    State(state): State<AppState>,
) -> Result<Html<String>, errors::Error> {
    let book = state.get(code.clone()).await?;
    render_book_form(
        &state,
        &format!("/book/{code}/edit"),
        &state.book_to_context(book),
        &[],
    )
    .await
}

pub async fn edit_book_commit(
    Path(code): Path<String>,
    State(state): State<AppState>,
    form: Multipart,
) -> Result<Html<String>, errors::Error> {
    let original = state.get(code.clone()).await?;

    match BookForm::from_multipart(form).await {
        Some((bookform, tempfile)) => {
            match bookform.to_book(&state.category_fullname_map, &state.location_name_map) {
                Ok(mut book) => {
                    book.cover_image_mimetype = book
                        .cover_image_mimetype
                        .or(original.cover_image_mimetype.clone());
                    state.replace(original.clone(), book.clone()).await?;
                    if let Some(tempfile) = tempfile {
                        state.save_cover_image(book.code.clone(), tempfile).await?;
                    }

                    let mut context = tera::Context::new();
                    context.insert("message", "The book has been updated.");
                    render_html("notice.html", &context)
                }
                Err((book_context, errors)) => {
                    render_book_form(
                        &state,
                        &format!("/book/{code}/edit"),
                        &book_context,
                        &errors,
                    )
                    .await
                }
            }
        }
        None => Err(errors::bad_request()),
    }
}

pub async fn stylesheet() -> impl IntoResponse {
    (
        [(header::CONTENT_TYPE, mime::TEXT_CSS_UTF_8.essence_str())],
        include_str!("_resources/style.css"),
    )
}

async fn serve_static_file(
    path: PathBuf,
    mime: mime::Mime,
) -> Result<Response<Body>, errors::Error> {
    let file = tokio::fs::File::open(path)
        .await
        .map_err(|_| errors::file_not_found())?;
    let body = Body::from_stream(ReaderStream::new(file));
    Ok(([(header::CONTENT_TYPE, mime.essence_str())], body).into_response())
}

async fn render_book_form(
    state: &AppState,
    action: &str,
    book_context: &Value,
    errors: &[String],
) -> Result<Html<String>, errors::Error> {
    let mut context = state.context().await?;
    context.insert("action", action);
    context.insert("errors", errors);
    context.insert("book", book_context);
    render_html("edit.html", &context)
}

fn render_html(template: &str, context: &tera::Context) -> Result<Html<String>, errors::Error> {
    match TEMPLATES.render(template, context) {
        Ok(rendered) => Ok(Html(rendered)),
        Err(_) => Err(errors::something_went_wrong()),
    }
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

    fn book_to_context(&self, b: Book) -> Value {
        self.bookform_to_context(BookForm::from(b))
    }

    fn bookform_to_context(&self, bf: BookForm) -> Value {
        bf.to_context(&self.category_fullname_map, &self.location_name_map)
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

        self.es_search(es_query)
            .await
            .map_err(|_| errors::cannot_connect_to_search_server())
    }

    async fn get(&self, code: String) -> Result<Book, errors::Error> {
        let code: Code = code.parse().map_err(|_| errors::invalid_code(&code))?;
        let result = self
            .es_get(code.clone())
            .await
            .map_err(|_| errors::cannot_connect_to_search_server())?;

        if let Some(book) = result {
            Ok(book)
        } else {
            Err(errors::book_not_found(&code))
        }
    }

    async fn put(&self, book: Book) -> Result<(), errors::Error> {
        self.es_put(book)
            .await
            .map_err(|_| errors::cannot_connect_to_search_server())
    }

    async fn delete(&self, book: Book, delete_files: bool) -> Result<(), errors::Error> {
        if book.cover_image_mimetype.is_some() && delete_files {
            self.remove_files(&book.code)
                .await
                .map_err(|_| errors::something_went_wrong())?;
        }
        self.es_delete(book.code)
            .await
            .map_err(|_| errors::cannot_connect_to_search_server())?;
        Ok(())
    }

    async fn replace(&self, from: Book, to: Book) -> Result<(), errors::Error> {
        self.delete(from.clone(), false).await?;
        self.put(to.clone()).await?;

        if from.code != to.code && from.cover_image_mimetype.is_some() {
            self.rename_files(&from.code, &to.code)
                .await
                .map_err(|_| errors::something_went_wrong())?;
        }

        Ok(())
    }

    async fn save_cover_image(&self, code: Code, file: NamedTempFile) -> Result<(), errors::Error> {
        let tmp_path = file.path();
        self.save_cover_file_and_generate_thumb(&code, tmp_path)
            .await
            .map_err(|_| errors::something_went_wrong())?;

        tokio::fs::remove_file(tmp_path)
            .await
            .map_err(|_| errors::something_went_wrong())?;

        Ok(())
    }
}

#[allow(clippy::cast_possible_truncation)]
#[allow(clippy::cast_precision_loss)]
#[allow(clippy::cast_sign_loss)]
fn percent(nom: u64, denom: usize) -> u64 {
    (nom as f64 / denom as f64 * 100.0) as u64
}
