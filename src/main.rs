use clap::Parser;
use elasticsearch::http::transport::Transport;
use elasticsearch::Elasticsearch;
use std::env;
use std::io;
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

    #[command(subcommand)]
    command: ArgsCommand,
}

#[derive(Clone, Debug, clap::Subcommand)]
enum ArgsCommand {
    /// Create the Elasticsearch index
    CreateIndex(CreateIndexArgs),
    /// Dump the Elasticsearch index as json to stdout
    ExportIndex,
    /// Import a dump of the Elasticsearch index, as json from stdin
    ImportIndex(CreateIndexArgs),
    /// Serve the web app
    Serve(ServeArgs),
}

#[derive(Clone, Debug, clap::Args)]
struct CreateIndexArgs {
    /// Drop the existing index.  This will delete data!
    #[clap(long, value_parser, default_value_t = false)]
    drop_existing: bool,
}

#[derive(Clone, Debug, clap::Args)]
struct ServeArgs {
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
    match Transport::single_node(&args.es_host) {
        Ok(transport) => {
            let client = &Elasticsearch::new(transport);
            match args.command {
                ArgsCommand::CreateIndex(cmd_args) => create_index(client, cmd_args).await,
                ArgsCommand::ExportIndex => export_index(client).await,
                ArgsCommand::ImportIndex(cmd_args) => import_index(client, cmd_args).await,
                ArgsCommand::Serve(cmd_args) => serve(args.es_host, cmd_args).await,
            }
        }
        Err(error) => {
            tracing::error!(?error, "could not initialise elasticsearch transport");
            process::exit(1);
        }
    }
}

async fn create_index(client: &Elasticsearch, args: CreateIndexArgs) {
    if args.drop_existing {
        match bookdb::es::drop(client).await {
            Ok(_) => {
                println!("dropped index");
            }
            Err(error) => {
                tracing::error!(?error, "could not drop index");
                process::exit(1);
            }
        }
    }

    match bookdb::es::create(client).await {
        Ok(_) => {
            println!("created index");
        }
        Err(error) => {
            tracing::error!(?error, "could not create index");
            process::exit(1);
        }
    }
}

async fn export_index(client: &Elasticsearch) {
    match bookdb::es::export(client).await {
        Ok(books) => serde_json::to_writer(io::stdout(), &books).unwrap(),
        Err(error) => {
            tracing::error!(?error, "could not export index");
            process::exit(1);
        }
    }
}

async fn import_index(client: &Elasticsearch, args: CreateIndexArgs) {
    if args.drop_existing {
        // drop and then recreate
        create_index(client, args).await;
    }
    match io::read_to_string(io::stdin()) {
        Ok(stdin) => match serde_json::from_str(&stdin) {
            Ok(books) => match bookdb::es::import(client, books).await {
                Ok(Some(num)) => println!("imported {num} records"),
                Ok(None) => {
                    tracing::error!(error = "bulk index failed", "could not import records");
                    process::exit(1);
                }
                Err(error) => {
                    tracing::error!(?error, "could not import records");
                    process::exit(1);
                }
            },
            Err(error) => {
                tracing::error!(?error, "could not deserialise records");
                process::exit(1);
            }
        },
        Err(error) => {
            tracing::error!(?error, "could not read stdin");
            process::exit(1);
        }
    }
}

async fn serve(es_host: String, args: ServeArgs) {
    match fs::read_to_string(args.config).await {
        Ok(config_str) => match serde_yaml::from_str(&config_str) {
            Ok(config) => {
                if let Err(error) = bookdb::web::serve(
                    es_host,
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
