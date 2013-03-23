import os
import utils.dirs as dirs

extensions = ['.png', '.gif', '.jpg', '.jpeg']


def upload(request):
    """Books may have covers associated with them, but only a few
    types of file are supported. We need to check to see if an image
    is provided and, if it the correct type, save it to the
    appropriate place.

    :param request: The request object
    """

    isbn = request.POST['isbn']

    # Firstly check if an image has been sent
    if 'cover' not in request.POST.keys() or request.POST['cover'] == b'':
        return

    # Extract the data and file extension
    ext = os.path.splitext(request.POST['cover'].filename)[1]
    data = request.POST['cover'].file

    # Check the file extension
    if ext not in extensions:
        return

    # Delete the current cover
    delete(isbn)

    # Save the new one
    with open(os.path.join(dirs.uploads, isbn + ext), 'wb') as f:
        f.write(data.read())


def delete(isbn):
    """Deletes the cover image, if there is one, associated with the
    given ISBN.

    :param isbn: Book to remove the cover of
    """

    for ext in extensions:
        img = os.path.join(dirs.uploads, isbn + ext)
        if os.path.exists(img):
            os.remove(img)


def find(isbn):
    """Find the cover associated with the given ISBN, and return its
    filename. If there is no such cover, we return None.

    :param isbn: The ISBN of the book.
    """

    for ext in extensions:
        if os.path.exists(os.path.join(dirs.uploads, isbn + ext)):
            return isbn + ext
