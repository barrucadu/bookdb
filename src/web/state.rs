use elasticsearch::http::transport::Transport;
use elasticsearch::Elasticsearch;
use std::collections::HashMap;
use std::path::{Path, PathBuf};

use crate::book::{Book, Code};
use crate::config::{Config, NameSlug, Slug};
use crate::es;

#[derive(Clone)]
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
    pub fn new(es_host: String, allow_writes: bool, upload_dir: PathBuf, config: &Config) -> Self {
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

    pub fn cover_image_path(&self, code: &Code) -> PathBuf {
        let mut path = self.upload_dir.clone();
        path.push(code.to_string());
        path
    }

    pub fn cover_thumb_path(&self, code: &Code) -> PathBuf {
        let mut path = self.upload_dir.clone();
        path.push("thumbs");
        path.push(format!("{code}.jpg"));
        path
    }

    pub async fn es_search(
        &self,
        es_query: es::SearchQuery,
    ) -> Result<es::SearchResult, elasticsearch::Error> {
        let client = self.elasticsearch()?;
        es::search(&client, es_query).await
    }

    pub async fn es_get(&self, code: Code) -> Result<Option<Book>, elasticsearch::Error> {
        let client = self.elasticsearch()?;
        es::get(&client, code).await
    }

    pub async fn es_put(&self, book: Book) -> Result<(), elasticsearch::Error> {
        let client = self.elasticsearch()?;
        es::put(&client, book).await?;
        Ok(())
    }

    pub async fn es_delete(&self, code: Code) -> Result<(), elasticsearch::Error> {
        let client = self.elasticsearch()?;
        es::delete(&client, code).await?;
        Ok(())
    }

    pub async fn remove_files(&self, code: &Code) -> Result<(), std::io::Error> {
        tokio::fs::remove_file(self.cover_image_path(code)).await?;
        tokio::fs::remove_file(self.cover_thumb_path(code)).await?;
        Ok(())
    }

    pub async fn rename_files(&self, from: &Code, to: &Code) -> Result<(), std::io::Error> {
        tokio::fs::rename(self.cover_image_path(from), self.cover_image_path(to)).await?;
        tokio::fs::rename(self.cover_thumb_path(from), self.cover_thumb_path(to)).await?;
        Ok(())
    }

    pub async fn save_cover_file_and_generate_thumb(
        &self,
        code: &Code,
        tmp_path: &Path,
    ) -> Result<(), std::io::Error> {
        let cover_path = self.cover_image_path(code);
        let thumb_path = self.cover_thumb_path(code);

        // copy to handle the case where the temporary directory is on a
        // different mountpoint (e.g. a tmpfs)
        tokio::fs::copy(tmp_path, cover_path.clone()).await?;
        tokio::fs::set_permissions(
            cover_path.clone(),
            std::os::unix::fs::PermissionsExt::from_mode(0o644),
        )
        .await?;

        tokio::spawn(generate_thumbnail_task(
            cover_path.to_string_lossy().into_owned(),
            thumb_path.to_string_lossy().into_owned(),
        ));

        Ok(())
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

async fn generate_thumbnail_task(cover_path: String, thumb_path: String) {
    let cmd = tokio::process::Command::new("magick")
        .args([&cover_path, "-resize", "16x24", &thumb_path])
        .spawn();

    match cmd {
        Ok(mut imagemagick) => match imagemagick.wait().await {
            Ok(exit_code) => {
                if !exit_code.success() {
                    tracing::warn!(
                        ?cover_path,
                        ?thumb_path,
                        ?exit_code,
                        "imagemagick process error"
                    );
                }
            }
            Err(error) => tracing::warn!(
                ?cover_path,
                ?thumb_path,
                ?error,
                "imagemagick process error"
            ),
        },
        Err(error) => tracing::warn!(
            ?cover_path,
            ?thumb_path,
            ?error,
            "could not spawn imagemagick process"
        ),
    }
}
