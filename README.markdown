bookdb
======

A database and web app to keep track of all my books, deployed to [bookdb.barrucadu.co.uk](https://bookdb.barrucadu.co.uk/search).

![bookdb](bookdb.png)

## Running

There's a docker-compose file provided, which will fire up a read-write instance
of bookdb accessible at `http://localhost:8888`:

```bash
docker-compose up
```

You will then need to create the search index:

```bash
docker-compose exec bookdb python -m bookdb.index.create
```

I run bookdb [via nix](https://github.com/barrucadu/nixfiles/blob/master/shared/bookdb/default.nix).
