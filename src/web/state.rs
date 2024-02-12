use elasticsearch::http::transport::Transport;
use elasticsearch::Elasticsearch;
use std::collections::HashMap;
use std::path::PathBuf;

use crate::book::Code;
use crate::config::{Config, NameSlug, Slug};

pub struct AppState {
    pub es_host: String,
    pub allow_writes: bool,
    pub upload_dir: PathBuf,
    pub category_fullname_map: HashMap<Slug, Vec<String>>,
    pub category_children_map: HashMap<Slug, Vec<Slug>>,
    pub categories: Vec<NameSlug>,
    pub location_name_map: HashMap<Slug, String>,
    pub locations: Vec<NameSlug>,
}

impl AppState {
    pub fn new(es_host: String, allow_writes: bool, upload_dir: PathBuf, config: Config) -> Self {
        let category_fullname_map = category_fullname_map(
            &config.category_slugs(),
            &config.category_name_map(),
            &config.category_parents_map(),
        );
        let categories = config
            .category_slugs()
            .into_iter()
            .map(|s| NameSlug {
                name: category_fullname_map.get(&s).unwrap().join(" / "),
                slug: s.clone(),
            })
            .collect();
        Self {
            es_host,
            allow_writes,
            upload_dir,
            category_fullname_map,
            category_children_map: config.category_children_map(),
            categories,
            location_name_map: config.location_name_map(),
            locations: config.locations.clone(),
        }
    }

    pub fn elasticsearch(&self) -> Result<Elasticsearch, elasticsearch::Error> {
        Transport::single_node(&self.es_host).map(Elasticsearch::new)
    }

    pub fn cover_image_path(&self, code: Code) -> PathBuf {
        let mut path = self.upload_dir.clone();
        path.push(code.to_string());
        path
    }

    pub fn cover_thumb_path(&self, code: Code) -> PathBuf {
        let mut path = self.upload_dir.clone();
        path.push("thumbs");
        path.push(format!("{code}.jpg"));
        path
    }
}

fn category_fullname_map(
    slugs: &[Slug],
    names: &HashMap<Slug, String>,
    parents: &HashMap<Slug, Vec<Slug>>,
) -> HashMap<Slug, Vec<String>> {
    let mut out = HashMap::with_capacity(slugs.len());
    for slug in slugs {
        let mut names: Vec<String> = parents
            .get(slug)
            .unwrap()
            .clone()
            .iter()
            .map(|slug| names.get(slug).unwrap().clone())
            .collect();
        names.reverse();
        out.insert(slug.clone(), names);
    }
    out
}
