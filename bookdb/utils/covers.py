import os
import utils.dirs as dirs
from models import DBSession
from models.book import Book


def upload(request):
    """Books may have covers associated with them. We need to check to
    see if an image is provided and save it to the appropriate place.

    The book must be already in the database before calling this
    function, as it retrieves and updates it.

    :param request: The request object
    """

    isbn = request.POST['isbn']

    # Firstly check if an image has been sent
    if 'cover' not in request.POST.keys() or request.POST['cover'] == b'':
        return

    # Extract the data and file extension
    ext = os.path.splitext(request.POST['cover'].filename)[1]
    data = request.POST['cover'].file

    # Delete the current cover
    book = Book.lookup(isbn)
    delete(book)
    book.image = isbn + ext
    DBSession.commit()

    # Save the new one
    with open(os.path.join(dirs.uploads, isbn + ext), 'wb') as f:
        f.write(data.read())


def delete(book):
    """Deletes the cover image, if there is one, associated with the
    given book.

    :param book: Book to remove the cover of
    """

    if book.image != '':
        os.remove(os.path.join(dirs.uploads, book.image))
