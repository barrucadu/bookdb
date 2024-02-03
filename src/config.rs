use serde::Deserialize;

#[derive(Clone, Debug, Deserialize)]
pub struct Config {
    pub locations: Vec<Location>,
    pub categories: Vec<Category>,
}

#[derive(Clone, Debug, Deserialize)]
pub struct Location {
    pub name: String,
    pub slug: Slug,
}

#[derive(Clone, Debug, Deserialize)]
pub struct Category {
    pub name: String,
    pub slug: Slug,
    pub children: Option<Vec<Category>>,
}

#[derive(PartialEq, PartialOrd, Eq, Ord, Clone, Debug, Deserialize)]
pub struct Slug(pub String);
