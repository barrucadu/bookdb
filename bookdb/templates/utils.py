import os
import dirs


def plural(count, singular):
    """Turn a singular word into a plural if need be.

    :param count: The number of things (1 yields a singular, everything
        else a plural)
    :param singular: The singular form of the word.
    """

    if count == 1:
        return singular
    else:
        # Woo, irregulars!
        irregulars = {'has': 'have'}
        if singular in irregulars.keys():
            return irregulars[singular]
        else:
            return singular + "s"


def find_book_image(isbn):
    """Find the image file associated with a book, and return its path.

    :param isbn: The ISBN of the book.
    """

    imagefile = '/static/nocover.png'

    exts = ['png', 'jpg', 'gif']
    for ext in exts:
        if os.path.exists(os.path.join(dirs.uploads, isbn + '.' + ext)):
            imagefile = '/static/uploads/{}.{}'.format(isbn, ext)

    return imagefile
