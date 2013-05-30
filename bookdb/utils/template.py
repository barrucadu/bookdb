"""utils.templates: Template utilities.

find_book_image:  Get the relative path from the web root to a book's
cover image.
are_unread_books: Check if there are any unread books
next_book:        Pick an unread book at random
"""

from models import DBSession
from models.book import Book
from random import randrange


def find_book_image(book):
    """Find the image file associated with a book, and return its path.

    :param book: The book
    """

    if book.image == '':
        return 'static/nocover.png'
    else:
        return 'static/uploads/' + book.image


def are_unread_books():
    """Check if there are any unread books in the database.
    """

    return DBSession.query(Book).filter(~Book.read).count() > 0


def next_book():
    """Pick a random unread book
    """

    books = DBSession.query(Book).filter(~Book.read)

    return books[randrange(0, books.count())]


def prettyprint_book(book):
    """Pretty-print the title of a book.
    """

    out = book.title

    if book.subtitle:
        out += ': {}'.format(book.subtitle)

    if book.volume and book.fascicle:
        out += ' (vol. {}; fas. {})'.format(book.volume, book.fascicle)
    elif book.volume:
        out += ' (vol. {})'.format(book.volume)
    elif book.fascicle:
        out += ' (fas. {})'.format(book.fascicle)

    if book.voltitle:
        out += ' {}'.format(book.voltitle)

    return out
