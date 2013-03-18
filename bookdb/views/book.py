"""Book views: for handling individual books.

info:   Display all the information about one book.
"""

from pyramid.view import view_config
from models import DBSession
from models.book import Book


@view_config(route_name='info', renderer='bookinfo.mako')
def info_view(request):
    """Display all the information about one book.

    :param request: The request object.
    """

    # TODO: Display an error page if the book does not exist.
    isbn = request.matchdict["isbn"]
    book = DBSession.query(Book).filter(Book.isbn == isbn).one()

    return {'pagetitle': '{}'.format(book.title),
            'book':      book}
