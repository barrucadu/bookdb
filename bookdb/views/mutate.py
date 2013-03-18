"""Mutator views: for handling updates to the database.

add:    Add a new book to the database.
edit:   Update an existing book in the database.
delete: Delete a book from the database.
"""

from pyramid.response import Response
from pyramid.view import view_config


@view_config(route_name='add', renderer='booklist.mako')
def add_view(request):
    """Display a form to add a new book.

    :param request: The request object.
    """

    pass


@view_config(route_name='edit', renderer='booklist.mako')
def edit_view(request):
    """Display a form to edit a book.

    :param request: The request object.
    """

    pass


@view_config(route_name='delete', renderer='booklist.mako')
def delete_view(request):
    """Ask for confirmation to delete a book.

    :param request: The request object.
    """

    pass


@view_config(route_name='addp', renderer='booklist.mako')
def add_post_view(request):
    """Process a book addition and display a confirmation.

    :param request: The request object.
    """

    pass


@view_config(route_name='editp', renderer='booklist.mako')
def edit_post_view(request):
    """Process a book edit and display a confirmation.

    :param request: The request object.
    """

    pass


@view_config(route_name='deletep', renderer='booklist.mako')
def delete_post_view(request):
    """Process a book deletion and display a confirmation.

    :param request: The request object.
    """

    pass
