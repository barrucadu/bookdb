use clap::Parser;
use elasticsearch::http::transport::Transport;
use elasticsearch::Elasticsearch;
use std::io;
use std::process;

use bookdb::book::Book;

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

#[allow(clippy::enum_variant_names)]
#[derive(Clone, Debug, clap::Subcommand)]
enum ArgsCommand {
    /// Create the Elasticsearch index
    CreateIndex(CreateIndexArgs),
    /// Dump the Elasticsearch index as json to stdout
    ExportIndex,
    /// Import a dump of the Elasticsearch index, as json from stdin
    ImportIndex(CreateIndexArgs),
}

#[derive(Clone, Debug, clap::Args)]
struct CreateIndexArgs {
    /// Drop the existing index first, if it exists.  This will delete data!
    #[clap(long, value_parser, default_value_t = false)]
    drop_existing: bool,
}

#[tokio::main]
async fn main() {
    let args = Args::parse();
    match Transport::single_node(&args.es_host) {
        Ok(transport) => {
            let client = &Elasticsearch::new(transport);
            match args.command {
                ArgsCommand::CreateIndex(cmd_args) => create_index(client, cmd_args).await,
                ArgsCommand::ExportIndex => export_index(client).await,
                ArgsCommand::ImportIndex(cmd_args) => import_index(client, cmd_args).await,
            }
        }
        Err(error) => {
            eprintln!("could not initialise elasticsearch transport: {error}");
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
                eprintln!("could not drop index: {error}");
                process::exit(1);
            }
        }
    }

    match bookdb::es::create(client).await {
        Ok(_) => {
            println!("created index");
        }
        Err(error) => {
            eprintln!("could not create index: {error}");
            process::exit(1);
        }
    }
}

async fn export_index(client: &Elasticsearch) {
    match bookdb::es::export(client).await {
        Ok(books) => serde_json::to_writer(io::stdout(), &books).unwrap(),
        Err(error) => {
            eprintln!("could not export index: {error}");
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
            Ok(books) => match bookdb::es::import(client, values_to_books(books)).await {
                Ok(Some(num)) => println!("imported {num} records"),
                Ok(None) => {
                    eprintln!("could not import records: bulk index failed");
                    process::exit(1);
                }
                Err(error) => {
                    eprintln!("could not import records: {error}");
                    process::exit(1);
                }
            },
            Err(error) => {
                eprintln!("could not deserialise records from stdin: {error}");
                process::exit(1);
            }
        },
        Err(error) => {
            eprintln!("could not read stdin: {error}");
            process::exit(1);
        }
    }
}

fn values_to_books(values: Vec<serde_json::Value>) -> Vec<Book> {
    let mut books = Vec::with_capacity(values.len());
    let mut errs = Vec::new();

    for value in &values {
        match Book::deserialize(value) {
            Ok(book) => books.push(book),
            Err(err) => errs.push(err),
        }
    }

    if errs.is_empty() {
        books
    } else {
        eprintln!("could not deserialise records from stdin:");
        for err in errs {
            eprintln!("  {err}");
        }
        process::exit(1);
    }
}
