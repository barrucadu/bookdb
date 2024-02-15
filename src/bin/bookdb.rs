use clap::Parser;
use std::env;
use std::net::SocketAddr;
use std::path::PathBuf;
use std::process;
use tokio::fs;
use tracing_subscriber::EnvFilter;

/// A database and web app to keep track of my books.
#[derive(Clone, Debug, Parser)]
struct Args {
    /// Address of the Elasticsearch server (in URL form)
    #[clap(
        long,
        value_parser,
        default_value = "http://localhost:9200",
        env = "ES_HOST"
    )]
    es_host: String,

    /// Address to listen on (in `ip:port` form)
    #[clap(
        long,
        value_parser,
        default_value = "127.0.0.1:4444",
        env = "BOOKDB_ADDRESS"
    )]
    address: SocketAddr,

    /// Whether to allow writes.
    #[clap(
        long,
        value_parser,
        default_value_t = false,
        env = "BOOKDB_ALLOW_WRITES"
    )]
    allow_writes: bool,

    /// Directory to store uploaded image files in.
    #[clap(
        long,
        value_parser,
        default_value = "./uploads",
        env = "BOOKDB_UPLOADS_DIR"
    )]
    upload_dir: PathBuf,

    /// Configuration file.
    #[clap(value_parser, value_name = "FILE")]
    config: PathBuf,
}

#[tokio::main]
async fn main() {
    begin_logging();

    let args = Args::parse();

    match fs::read_to_string(args.config).await {
        Ok(config_str) => match serde_yaml::from_str(&config_str) {
            Ok(config) => {
                if let Err(error) = bookdb::web::serve(
                    args.es_host,
                    args.address,
                    args.allow_writes,
                    args.upload_dir,
                    config,
                )
                .await
                {
                    tracing::error!(?error, "could not serve");
                    process::exit(1);
                }
            }
            Err(error) => {
                tracing::error!(?error, "could not parse config file");
                process::exit(1);
            }
        },
        Err(error) => {
            tracing::error!(?error, "could not read config file");
            process::exit(1);
        }
    }
}

/// Configure the log level and format from the environment.
///
/// `RUST_LOG` controls the level.
///
/// `RUST_LOG_FORMAT` is a sequence of comma-separated values:
///
/// - One of `full` (default), `compact`, `prewtty`, or `json`
/// - One of `ansi` (default), `no-ansi`
/// - One of `time` (default), `no-time`
fn begin_logging() {
    let mut with_ansi = true;
    let mut with_time = true;
    let mut log_format = "full";

    if let Ok(var) = env::var("RUST_LOG_FORMAT") {
        for option in var.split(',') {
            match option {
                "no-ansi" => with_ansi = false,
                "no-time" => with_time = false,
                "json" => log_format = "json",
                "pretty" => log_format = "pretty",
                "compact" => log_format = "compact",
                _ => {}
            }
        }
    }

    let logger = tracing_subscriber::fmt()
        .with_env_filter(EnvFilter::from_default_env())
        .with_ansi(with_ansi);

    // surely there is a better way to do this
    match (log_format, with_time) {
        ("json", true) => logger.json().init(),
        ("json", false) => logger.json().without_time().init(),
        ("pretty", true) => logger.pretty().init(),
        ("pretty", false) => logger.pretty().without_time().init(),
        ("compact", true) => logger.compact().init(),
        ("compact", false) => logger.compact().without_time().init(),
        (_, true) => logger.init(),
        (_, false) => logger.without_time().init(),
    }
}
