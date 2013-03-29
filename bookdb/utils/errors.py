"""utils.errors: Error reporting functions.

error:          Rollback the database and display an error message.
exception_view: Catch any exceptions that bubble to the top of the
program and display an error message.
"""

from models import DBSession
from pyramid.renderers import render_to_response
from pyramid.view import view_config


def error(request, message):
    """Do some error recovery and return an appropriate response to
    the user.

    :param request: The request object.
    :param message: Message to display.
    """

    DBSession.rollback()

    return render_to_response(
        'information.mako',
        {
            'message': message,
            'error':   True
        },
        request=request)


@view_config(context=Exception)
def exception_view(e, request):
    """A view handler for uncaught exceptions and returning an error
    message as appropriate

    :param e: The uncaught exception
    :param request: The request object.
    """

    messages = {
        'NoResultFound':   'The specified book does not exist.',
        'KeyError':        'Please fill in all required fields.',
        'PermissionError': 'Could not upload cover image.'}

    return error(request, messages.get(
        type(e).__name__,
        'An unhandled error occurred. This is a bug.'))
