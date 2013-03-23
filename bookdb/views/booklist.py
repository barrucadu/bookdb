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
from utils.errors import handle_exception


@view_config(route_name='list', renderer='booklist.mako')
def list_view(request):
    """Display a list of all books.

    :param request: The request object.
    """

    # The template takes a page title, list of books, the number of
    # unique authors and the number of books which are read.
    books = booklist()
    return {'title':   'BookDB',
            'books':   books,
            'authors': count_authors(books),
            'read':    count_read(books)}


@view_config(route_name='search', renderer='search.mako')
def search_view(request):
    """Display a list of all books matching the criteria.

    :param request: The request object.
    """

    # The search criteria are encoded in the GET parameters, where
    # every key is a search field.
    criteria = [(Book.unstring(field), value)
                for field, value in request.GET.items()
                if Book.unstring(field) is not None]

    # Finally, we need to manually filter by matchread and
    # matchunread, as those don't directly correspond to a field in
    # the database.
    matchread = "matchread" in request.GET.keys()
    matchunread = "matchunread" in request.GET.keys()
    books = [book
             for book in booklist(criteria)
             if (book.read and matchread) or (not book.read and matchunread)]

    return {'title':       'BookDB :: Search',
            'books':       books,
            'authors':     count_authors(books),
            'read':        count_read(books),
            'isbn':        request.GET.get("isbn",        ""),
            'booktitle':   request.GET.get("title",       ""),
            'author':      request.GET.get("author",      ""),
            'matchread':   request.GET.get("matchread",   ""),
            'matchunread': request.GET.get("matchunread", ""),
            'location':    request.GET.get("location",    ""),
            'borrower':    request.GET.get("borrower",    "")}


@view_config(route_name='filter', renderer='booklist.mako')
def filter_view(request):
    """Display a list of all books matching the filter.

    :param request: The request object.
    """

    field = request.matchdict['field']
    value = request.matchdict['value']

    # Display an error if the field doesn't exist
    if Book.unstring(field) is None:
        return handle_exception(
            request,
            message='The field you attempted to filter by does not exist.')

    if field == "read":
        books = [book
                 for book in booklist()
                 if book.read == (value == "yes")]
    else:
        books = booklist([(Book.unstring(field), value)])

    return {'title':   'BookDB :: Filter',
            'books':   books,
            'authors': count_authors(books),
            'read':    count_read(books)}


def booklist(filter=[]):
    """All of the views on this page are the same, they only differ in
    which criteria are used to filter the list, from none, to only a
    single field, to potentially all the fields. However, these
    operations are basically all the same: retrieve the books which
    match some criteria.

    :param filter: A list of (field, value) pairs. Only books for
    which the given field matches the given value will be returned,
    and this must be the case for all pairs.

        "field" is a field of a Book object
        "value" is the value to compare against
    """

    books = DBSession.query(Book)

    for field, val in filter:
        books = books.filter(field.like('%{}%'.format(val)))

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
                for book in books
                if book.read])
