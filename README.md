bookdb
======

A database and web app to keep track of all my books, deployed to
[bookdb.barrucadu.co.uk](https://bookdb.barrucadu.co.uk/).


Development
-----------

Install `rustup` and `openssl`, and then install the default toolchain:

```bash
rustup show
```

Then, compile in release mode:

```bash
cargo build --release
```

Run the unit tests with:

```bash
cargo test
```

### With nix

Open a development shell:

```bash
nix develop
```

And run cargo commands in there.


Usage
-----

Start up an Elasticsearch server and store the URL in the `ES_HOST` environment
variable.

Initialise the Elasticsearch index and start the server in read-write mode:

```bash
export ES_HOST="..."
./target/release/bookdb_ctl create-index
./target/release/bookdb --allow-writes --upload-dir="<...>" <config file>
```

Omit the `--alow-writes` to launch in read-only mode.

Dump the Elasticsearch index as json to stdout with:

```bash
./target/release/bookdb_ctl export-index > bookdb.json
```

Restore it, overwriting the existing index:

```bash
./targets/release/bookdb_ctl import-index --drop-existing < bookdb.json
```

See the `--help` text for more.
