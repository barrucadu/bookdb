from models import DBSession
from pyramid.renderers import render_to_response


def handle_exception(request, e=None, message=None):
    """Do some error recovery and return an appropriate response to
    the user.

    :param request: The request object.
    :param e:       The exception object.
    :param message: Custom message to display.

    At least one of 'e' or 'message' must be given, and if both are
    given, the message takes priority.
    """

    DBSession.rollback()

    messages = {
        'NoResultFound':   'The specified book does not exist.',
        'KeyError':        'Please fill in all required fields.',
        'PermissionError': 'Could not upload cover image.'}

    if message is None:
        # If e is None here we get an uncaught exception thrown, but
        # that's fine as it's an internal logic error.
        message = messages[type(e).__name__]

    return render_to_response(
        'information.mako',
        {
            'title':   'Error Occurred',
            'message': message,
            'error':   True
        },
        request=request)
