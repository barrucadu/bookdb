"""Book list views: for handling (optionally filtered) lists of
books.

list:   Display all books.
search: Display all books matching the criteria.
filter: Display all books matching the filter.

Book lists are ordered by author name and then by title.
"""

from pyramid.view import view_config
from models import DBSession
from models.book import Book


@view_config(route_name='list', renderer='booklist.mako')
def list_view(request):
    """Display a list of all books.

    :param request: The request object.
    """

    # The template takes a page title, list of books, the number of
    # unique authors and the number of books which are read.
    books = booklist()
    return {'pagetitle': 'BookDB',
            'books':     books,
            'authors':   count_authors(books),
            'read':      count_read(books)}


@view_config(route_name='search', renderer='search.mako')
def search_view(request):
    """Display a list of all books matching the criteria.

    :param request: The request object.
    """

    # The search criteria are encoded in the GET parameters, where
    # every key is a search field. The read field is matched by matchread
    # and matchunread.
    criteria = request.GET
    books = None

    matchread   = criteria.get("matchread",   "")
    matchunread = criteria.get("matchunread", "")
    if "matchread" in criteria.keys() \
        and "matchunread" in criteria.keys():
        # All books are either read or unread, so don't add any
        # constraints.
        criteria.pop("matchread")
        criteria.pop("matchunread")

    elif "matchread" in criteria.keys():
        # Only want read books
        criteria.pop("matchread")
        criteria["read"] = "yes"

    elif "matchunread" in criteria.keys():
        # Only want unread books
        criteria.pop("matchunread")
        criteria["read"] = "no"

    else:
        # There are no books which are both read and unread, so take a
        # shortcut.
        books = []

    books = booklist(criteria.items()) if books is None else books
    return {'pagetitle': 'BookDB :: Search',
            'books':     books,
            'authors':   count_authors(books),
            'read':      count_read(books),
            # Search-specific template vars
            'isbn':        criteria.get("isbn",     ""),
            'title':       criteria.get("title",    ""),
            'author':      criteria.get("author",   ""),
            'matchread':   matchread,
            'matchunread': matchunread,
            'location':    criteria.get("location", ""),
            'borrower':    criteria.get("borrower", "")
            }


@view_config(route_name='filter', renderer='booklist.mako')
def filter_view(request):
    """Display a list of all books matching the filter.

    :param request: The request object.
    """

    # We need to extract the single field and value, and pass those in
    # as the one filter criteria.
    field = request.matchdict['field']
    value = request.matchdict['value']

    books = booklist([(field, value)])
    return {'pagetitle': 'BookDB :: Filter',
            'books':     books,
            'authors':   count_authors(books),
            'read':      count_read(books)}


def booklist(filter=[]):
    """All of the views on this page are the same, they only differ in
    which criteria are used to filter the list, from none, to only a
    single field, to potentially all the fields. However, these
    operations are basically all the same: retrieve the books which
    match some criteria.

    :param filter: A list of (field, value) pairs. Only books for
        which the given field contains the given value will be
        returned, and this must be the case for all pairs.
    """

    # Get all the books
    books = DBSession.query(Book)

    # Then apply every valid filter
    for field, val in filter:
        # Now get the actual field object and filter by it.
        dbfield = Book.unstring(field)

        # The "read" field needs special treatment as it's a
        # boolean.
        if field == "read":
            books = books.filter(dbfield == (val == "yes"))
        else:
            if dbfield is not None:
                books = books.filter(dbfield.like('%{}%'.format(val)))

    # And return them
    return books.order_by(Book.author, Book.title).all()


def count_authors(books):
    """Count the number of unique authors in a list of books.

    :param books: A (possibly empty) list of books.
    """

    return len(set([author
                    for book in books
                    for author in book.authors()]))


def count_read(books):
    """Count the number of read books in a list of books.

    :param books: A (possibly empty) list of books.
    """

    return len([book
                for book in books if book.read])
