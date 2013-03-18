"""Book list views: for handling (optionally filtered) lists of
books.

list:   Display all books.
search: Display all books matching the criteria.
filter: Display all books matching the filter.

Book lists are ordered by author name and then by title.
"""

from pyramid.response import Response
from pyramid.view import view_config


@view_config(route_name='list', renderer='booklist.mako')
def list_view(request):
    """Display a list of all books.

    :param request: The request object.
    """

    pass


@view_config(route_name='search', renderer='booklist.mako')
def search_view(request):
    """Display a list of all books matching the criteria.

    :param request: The request object.
    """

    pass


@view_config(route_name='filter', renderer='booklist.mako')
def filter_view(request):
    """Display a list of all books matching the filter.

    :param request: The request object.
    """

    pass
