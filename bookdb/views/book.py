"""Book views: for handling individual books.

info:   Display all the information about one book.
"""

from pyramid.response import Response
from pyramid.view import view_config


@view_config(route_name='info', renderer='book.mako')
def info_view(request):
    """Display all the information about one book.

    :param request: The request object.
    """

    pass
