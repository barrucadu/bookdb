BookDB
======

A database and web app to keep track of all my books.

Building
--------

Build with stack:

    stack build

Running
-------

Configure with environment variables:

- `BOOKDB_HOST`: host to listen on (default `*`)
- `BOOKDB_PORT`: port to listen on (default `3000`)
- `BOOKDB_WEB_ROOT`: base path for external URLs (default `http://localhost:3000`)
- `BOOKDB_FILE_ROOT`: directory to store covers in (default `/tmp`)
- `BOOKDB_PG_HOST`: postgres host (default `localhost`)
- `BOOKDB_PG_PORT`: postgres port (default `5432`)
- `BOOKDB_PG_DB`: postgres database name (default `bookdb`)
- `BOOKDB_PG_SCHEME`: postgres schema (default unset)
- `BOOKDB_PG_USERNAME`: postgres username (default unset)
- `BOOKDB_PG_PASSWORD`: postgres password (default unset)
- `BOOKDB_READ_ONLY`: whether the database is read only (default `false`)

Before first use:

    bookdb makedb

Starting the server:

    bookdb run

Migration
---------

When upgrading major version, run the migration sql file on your
database (don't forget to make a backup first!)
