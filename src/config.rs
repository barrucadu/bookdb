use serde::{Deserialize, Serialize};

#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct Config {
    pub locations: Vec<Location>,
    pub categories: Vec<Category>,
}

#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct Location {
    pub name: String,
    pub slug: Slug,
}

#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct Category {
    pub name: String,
    pub slug: Slug,
    pub children: Option<Vec<Category>>,
}

#[derive(PartialEq, PartialOrd, Eq, Ord, Clone, Debug, Deserialize, Serialize)]
pub struct Slug(pub String);
