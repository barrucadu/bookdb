BookDB
======

A database and web app to keep track of all my books.

Running
-------

Before first use:

    bookdb makedb examples/bookdb.conf

Starting the server:

    bookdb run examples/bookdb.conf

Migration
---------

When upgrading major version, run the migration sql file on your
database (don't forget to make a backup first!)
