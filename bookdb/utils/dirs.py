"""utils.dirs: Commonly-used directories.
"""

import os

# Figure out where we are
here = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))

# Templates
templates = os.path.join(here, 'templates')

# Static files
static = os.path.join(here, 'static')

# Uploads
uploads = os.path.join(static, 'uploads')
