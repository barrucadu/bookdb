"""BookDB.

Usage:
  bookdb [--readonly] [--host=<host>] [--port=<port>] [--database=<database>] [--webpath=<webpath>]
  bookdb -h | --help

Options:
  -h --help              Show this screen

  --readonly             Do not allow updates to the database.

  --host=<host>          Hostname or IP to listen on
                           [default: localhost]

  --port=<port>          Port to listen on
                           [default: 3000]

  --database=<database>  The filename of the SQLite database
                           [default: bookdb.sqlite3]

  --webpath=<webpath>    Prepend this to all URLs in the application.
"""

# Import views and models
import views
import models

# Import everything else
from docopt import docopt
from pyramid.config import Configurator
from wsgiref.simple_server import make_server
import sqlalchemy
from sys import exit
import dirs
from pyramid.events import subscriber, BeforeRender

# Parse the command-line arguments
arguments = docopt(__doc__)


@subscriber(BeforeRender)
def add_web_path(event):
    """Add the web path to all template contexts before they are
    rendered.

    :param event: The template render event.
    """

    if arguments['--webpath'] is None:
        event['webpath'] = ''
    else:
        event['webpath'] = arguments['--webpath']

# Start the server
if __name__ == '__main__':
    # Build the configuration and the WSGI app
    config = Configurator(settings={
        'mako.directories': dirs.templates,
        'readonly':         arguments['--readonly'],
    })

    config.add_static_view(name='static', path=dirs.static)

    config.add_route('list',    '/',                request_method='GET')
    config.add_route('search',  '/search',          request_method='GET')
    config.add_route('add',     '/add',             request_method='GET')
    config.add_route('edit',    '/{isbn}/edit',     request_method='GET')
    config.add_route('delete',  '/{isbn}/delete',   request_method='GET')
    config.add_route('addp',    '/add',             request_method='POST')
    config.add_route('editp',   '/{isbn}/edit',     request_method='POST')
    config.add_route('deletep', '/{isbn}/delete',   request_method='POST')
    config.add_route('filter',  '/{field}/{value}', request_method='GET')

    config.scan(views)

    # Load any remaining local configuration
    config.scan()

    app = config.make_wsgi_app()

    # Initialise the SQL database
    try:
        engine = sqlalchemy.create_engine(
            'sqlite:///{}'.format(arguments['--database']))
        models.initialise_sql(engine)
    except sqlalchemy.exc.OperationalError:
        print("Cannot initialise database.")
        exit(1)

    # Start the server
    try:
        server = make_server(arguments['--host'],
                             int(arguments['--port']),
                             app)
        server.serve_forever()
    except ValueError:
        print("Port must be a numeric value.")
        exit(1)
    except OSError:
        print("Cannot listen on given address.")
        exit(1)
