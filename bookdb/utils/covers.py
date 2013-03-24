"""utils.covers: Utility functions for dealing with book covers.

upload: Upload a new cover image, deleting any prior ones.
delete: Delete a book's cover image (if any).
"""

import os
import utils.dirs as dirs


def upload(book, request):
    """Books may have covers associated with them. We need to check to
    see if an image is provided and save it to the appropriate place.

    :param book: The book to update
    :param request: The request object

    The database is not updated, after this the book should either be
    added (if new) or the changes committed (if old).
    """

    # Firstly check if an image has been sent
    if 'cover' not in request.POST.keys() or request.POST['cover'] == b'':
        return

    # Extract the data and file extension
    ext = os.path.splitext(request.POST['cover'].filename)[1]
    data = request.POST['cover'].file

    # Delete the current cover
    delete(book)
    book.image = book.isbn + ext

    # Save the new one
    with open(os.path.join(dirs.uploads, book.isbn + ext), 'wb') as f:
        f.write(data.read())


def delete(book):
    """Deletes the cover image, if there is one, associated with the
    given book.

    :param book: Book to remove the cover of
    """

    if book.image != '':
        os.remove(os.path.join(dirs.uploads, book.image))
