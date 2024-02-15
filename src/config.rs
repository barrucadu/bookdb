use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::hash::Hash;

#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct Config {
    pub locations: Vec<NameSlug>,
    pub categories: Vec<Category>,
}

#[derive(Clone, Debug, PartialEq, Eq, Deserialize, Serialize)]
pub struct NameSlug {
    pub name: String,
    pub slug: Slug,
}

#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct Category {
    pub name: String,
    pub slug: Slug,
    pub children: Option<Vec<Category>>,
}

#[derive(PartialEq, PartialOrd, Eq, Ord, Clone, Debug, Hash, Deserialize, Serialize)]
pub struct Slug(pub String);

impl Config {
    pub fn category_name_map(&self) -> HashMap<Slug, String> {
        let category_map = category_map_recursive(&self.categories);
        let mut out = HashMap::with_capacity(category_map.len());
        for (slug, cats) in category_map {
            out.insert(slug, cats[0].name.clone());
        }
        out
    }

    pub fn category_parents_map(&self) -> HashMap<Slug, Vec<Slug>> {
        let category_map = category_map_recursive(&self.categories);
        let mut out = HashMap::with_capacity(category_map.len());
        for (slug, cats) in category_map {
            out.insert(slug, cats.into_iter().map(|c| c.slug).collect());
        }
        out
    }

    pub fn category_children_map(&self) -> HashMap<Slug, Vec<Slug>> {
        HashMap::from_iter(category_child_map_recursive(&self.categories))
    }

    pub fn category_slugs(&self) -> Vec<Slug> {
        category_map_recursive(&self.categories)
            .into_iter()
            .map(|kv| kv.0)
            .collect()
    }

    pub fn location_name_map(&self) -> HashMap<Slug, String> {
        let mut out = HashMap::with_capacity(self.locations.len());
        for location in &self.locations {
            out.insert(location.slug.clone(), location.name.clone());
        }
        out
    }

    pub fn location_slugs(&self) -> Vec<Slug> {
        self.locations.iter().map(|loc| loc.slug.clone()).collect()
    }
}

fn category_map_recursive(nested_categories: &[Category]) -> Vec<(Slug, Vec<Category>)> {
    let mut out = Vec::with_capacity(nested_categories.len());
    for ncat in nested_categories {
        out.push((ncat.slug.clone(), vec![ncat.clone()]));
        if let Some(children) = &ncat.children {
            for (ck, mut cv) in category_map_recursive(children) {
                cv.push(ncat.clone());
                out.push((ck, cv));
            }
        }
    }
    out
}

fn category_child_map_recursive(nested_categories: &[Category]) -> Vec<(Slug, Vec<Slug>)> {
    let mut out = Vec::with_capacity(nested_categories.len());
    for ncat in nested_categories {
        let mut mine = if let Some(children) = &ncat.children {
            let mut mine = Vec::with_capacity(children.len());
            for (ck, cv) in category_child_map_recursive(children) {
                mine.push(ck.clone());
                mine.append(&mut cv.clone());
                out.push((ck, cv));
            }
            mine
        } else {
            Vec::new()
        };
        mine.sort();
        mine.dedup();
        out.push((ncat.slug.clone(), mine));
    }
    out
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn category_name_map() {
        assert_eq!(
            fixture().category_name_map(),
            HashMap::from([
                (Slug("top".to_string()), "Kurald Galain".to_string()),
                (Slug("middle".to_string()), "Kurald Thyrllan".to_string()),
                (Slug("leaf-a".to_string()), "Serc".to_string()),
                (Slug("leaf-b".to_string()), "Rashan".to_string()),
            ]),
        );
    }

    #[test]
    fn category_parents_map() {
        let leaf_a = Slug("leaf-a".to_string());
        let leaf_b = Slug("leaf-b".to_string());
        let middle = Slug("middle".to_string());
        let top = Slug("top".to_string());

        assert_eq!(
            fixture().category_parents_map(),
            HashMap::from([
                (
                    leaf_a.clone(),
                    vec![leaf_a.clone(), middle.clone(), top.clone()]
                ),
                (leaf_b.clone(), vec![leaf_b.clone(), top.clone()]),
                (middle.clone(), vec![middle.clone(), top.clone()]),
                (top.clone(), vec![top.clone()]),
            ]),
        );
    }

    #[test]
    fn category_children_map() {
        let leaf_a = Slug("leaf-a".to_string());
        let leaf_b = Slug("leaf-b".to_string());
        let middle = Slug("middle".to_string());
        let top = Slug("top".to_string());

        assert_eq!(
            fixture().category_children_map(),
            HashMap::from([
                (leaf_a.clone(), vec![]),
                (leaf_b.clone(), vec![]),
                (middle.clone(), vec![leaf_a.clone()]),
                (
                    top.clone(),
                    vec![leaf_a.clone(), leaf_b.clone(), middle.clone()]
                ),
            ]),
        );
    }

    #[test]
    fn category_slugs() {
        assert_eq!(
            fixture().category_slugs(),
            vec![
                Slug("top".to_string()),
                Slug("middle".to_string()),
                Slug("leaf-a".to_string()),
                Slug("leaf-b".to_string()),
            ],
        );
    }

    #[test]
    fn location_name_map() {
        assert_eq!(
            fixture().location_name_map(),
            HashMap::from([
                (Slug("malaz".to_string()), "Malaz Island".to_string()),
                (Slug("quon-tali".to_string()), "Quon Tali".to_string()),
                (Slug("seven-cities".to_string()), "Seven Cities".to_string()),
                (Slug("genabackis".to_string()), "Genabackis".to_string()),
            ]),
        );
    }

    #[test]
    fn location_slugs() {
        assert_eq!(
            fixture().location_slugs(),
            vec![
                Slug("malaz".to_string()),
                Slug("quon-tali".to_string()),
                Slug("seven-cities".to_string()),
                Slug("genabackis".to_string()),
            ],
        );
    }

    fn fixture() -> Config {
        let leaf_a = Category {
            name: "Serc".to_string(),
            slug: Slug("leaf-a".to_string()),
            children: None,
        };
        let leaf_b = Category {
            name: "Rashan".to_string(),
            slug: Slug("leaf-b".to_string()),
            children: None,
        };
        let middle = Category {
            name: "Kurald Thyrllan".to_string(),
            slug: Slug("middle".to_string()),
            children: Some(vec![leaf_a.clone()]),
        };
        let top = Category {
            name: "Kurald Galain".to_string(),
            slug: Slug("top".to_string()),
            children: Some(vec![middle.clone(), leaf_b.clone()]),
        };

        Config {
            locations: vec![
                NameSlug {
                    name: "Malaz Island".to_string(),
                    slug: Slug("malaz".to_string()),
                },
                NameSlug {
                    name: "Quon Tali".to_string(),
                    slug: Slug("quon-tali".to_string()),
                },
                NameSlug {
                    name: "Seven Cities".to_string(),
                    slug: Slug("seven-cities".to_string()),
                },
                NameSlug {
                    name: "Genabackis".to_string(),
                    slug: Slug("genabackis".to_string()),
                },
            ],
            categories: vec![top],
        }
    }
}
