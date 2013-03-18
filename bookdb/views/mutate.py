"""Mutator views: for handling updates to the database.

add:    Add a new book to the database.
edit:   Update an existing book in the database.
delete: Delete a book from the database.
"""

from pyramid.view import view_config
from models import DBSession
from models.book import Book
from datetime import date


@view_config(route_name='add', renderer='bookform.mako')
def add_view(request):
    """Display a form to add a new book.

    :param request: The request object.
    """

    return {'pagetitle': 'BookDB :: Add',
            'target':    '/add',
            'submit':    'Add',
            'isbn':      '',
            'title':     '',
            'author':    '',
            'read':      '',
            'lastread':  '',
            'location':  '',
            'borrower':  ''}


@view_config(route_name='edit', renderer='bookform.mako')
def edit_view(request):
    """Display a form to edit a book.

    :param request: The request object.
    """

    # TODO: Display an error page if the book does not exist.
    isbn = request.matchdict["isbn"]
    book = DBSession.query(Book).filter(Book.isbn == isbn).one()

    return {'pagetitle': 'BookDB :: Edit',
            'target':    '/{}/edit'.format(isbn),
            'submit':    'Edit',
            'isbn':      isbn,
            'title':     book.title,
            'author':    ' & '.join(book.authors),
            'read':      'checked'
                if book.read else '',
            'lastread':  str(book.lastread)
                if not book.lastread == date.MIN else '',
            'location':  book.location,
            'borrower':  book.borrower}


@view_config(route_name='delete', renderer='confirmdelete.mako')
def delete_view(request):
    """Ask for confirmation to delete a book.

    :param request: The request object.
    """

    # TODO: Display an error page if the book does not exist.
    isbn = request.matchdict["isbn"]
    book = DBSession.query(Book).filter(Book.isbn == isbn).one()

    return {'pagetitle': 'Confirm Delete',
            'isbn': isbn,
            'title': book.title,
            'author': ' & '.join(book.authors)}


@view_config(route_name='addp', renderer='information.mako')
def add_post_view(request):
    """Process a book addition and display a confirmation.

    :param request: The request object.
    """

    pass


@view_config(route_name='editp', renderer='information.mako')
def edit_post_view(request):
    """Process a book edit and display a confirmation.

    :param request: The request object.
    """

    pass


@view_config(route_name='deletep', renderer='information.mako')
def delete_post_view(request):
    """Process a book deletion and display a confirmation.

    :param request: The request object.
    """

    pass
