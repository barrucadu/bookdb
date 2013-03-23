from utils.errors import handle_exception
from functools import wraps


def mutates(f):
    """Indicate that a view function mutates the database. An error
    will be returned to the user if the system is running in read-only
    mode.

    :param f: The view function
    """

    @wraps(f)
    def wrapper(*args, **kwargs):
        request = args[1]
        if request.registry.settings['readonly']:
            return handle_exception(request,
                                    message='The database is read-only.')

        return f(request, **kwargs)

    return wrapper
