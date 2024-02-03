use elasticsearch::Elasticsearch;

pub async fn create(_client: &Elasticsearch) {
    println!("bookdb::index::create");
}

pub async fn drop(_client: &Elasticsearch) {
    println!("bookdb::index::drop");
}

pub async fn export(_client: &Elasticsearch) {
    println!("bookdb::index::export");
}

pub async fn import(_client: &Elasticsearch) {
    println!("bookdb::index::import");
}
