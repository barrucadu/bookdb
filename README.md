BookDB
======

A database and web app to keep track of all my books.

Note: this depends on Seacat, which hasn't got a stable release
yet. See the repo (https://github.com/Barrucadu/lambdadelta).

Running
-------

Before first use, and after all database changes:

    bookdb migrate examples/bookdb.conf

Starting the server:

    bookdb runserver examples/bookdb.conf
