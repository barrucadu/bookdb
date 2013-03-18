"""BookDB.

Usage:
  bookdb [--host=<host>] [--port=<port>]
  bookdb -h | --help

Options:
  -h --help      Show this screen
  --host=<host>  Hostname or IP to listen on [default: localhost]
  --port=<port>  Port to listen on [default: 3000]
"""

# Import views and models
import views
import models

# Import everything else
import os
from docopt import docopt
from pyramid.config import Configurator
from wsgiref.simple_server import make_server
from sqlalchemy import create_engine

# Figure out where we are
here = os.path.dirname(os.path.abspath(__file__))

# Parse the command-line arguments
arguments = docopt(__doc__)

# Start the server
if __name__ == '__main__':
    # Build the configuration and the WSGI app
    config = Configurator(settings={
        'mako.directories': os.path.join(here, 'templates')
    })

    config.add_route('list',    '/',                request_method='GET')
    config.add_route('search',  '/search',          request_method='GET')
    config.add_route('filter',  '/{field}/{value}', request_method='GET')
    config.add_route('info',    '/{isbn}',          request_method='GET')
    config.add_route('add',     '/add',             request_method='GET')
    config.add_route('edit',    '/{isbn}/edit',     request_method='GET')
    config.add_route('delete',  '/{isbn}/delete',   request_method='GET')
    config.add_route('addp',    '/add',             request_method='POST')
    config.add_route('editp',   '/{isbn}/edit',     request_method='POST')
    config.add_route('deletep', '/{isbn}/delete',   request_method='POST')

    config.add_static_view(name='static', path=os.path.join(here, 'static'))

    config.scan(views)

    app = config.make_wsgi_app()

    # Initialise the SQL database
    engine = create_engine('sqlite:///bookdb.sqlite3')
    models.initialise_sql(engine)
    config.scan(models)

    # Start the server
    server = make_server(arguments['--host'],
                         int(arguments['--port']),
                         app)

    server.serve_forever()
