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
    """A context manager for handling exceptions and returning an
    error message as appropriate

    :param e: The uncaught exception
    :param request: The request object.
    """

    messages = {
        'NoResultFound':   'The specified book does not exist.',
        'KeyError':        'Please fill in all required fields.',
        'PermissionError': 'Could not upload cover image.'}

    exception = type(e).__name__

    if exception in messages:
        return error(request, messages[exception])
    else:
        return error(request, 'An unhandled error occurred. This is a bug.')
