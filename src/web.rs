use elasticsearch::Elasticsearch;
use std::net::SocketAddr;
use std::path::PathBuf;

use crate::config::Config;

pub async fn serve(
    _client: &Elasticsearch,
    _address: SocketAddr,
    _allow_writes: bool,
    _upload_dir: PathBuf,
    config: Config,
) {
    println!("bookdb::web::serve");
    dbg!(config);
}
