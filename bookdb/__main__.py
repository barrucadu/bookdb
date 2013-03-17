"""BookDB.

Usage:
  bookdb [--host=<host>] [--port=<port>]
  bookdb -h | --help

Options:
  -h --help      Show this screen
  --host=<host>  Hostname or IP to listen on [default: localhost]
  --port=<port>  Port to listen on [default: 3000]
"""

import os
from docopt import docopt
from pyramid.config import Configurator
from wsgiref.simple_server import make_server

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

    config.add_static_view(name='static', path=os.path.join(here, 'static'))

    app = config.make_wsgi_app()

    # Start the server
    server = make_server(arguments['--host'],
                         int(arguments['--port']),
                         app)

    server.serve_forever()
