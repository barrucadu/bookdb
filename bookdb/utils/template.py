from models import DBSession
from models.book import Book
from random import randrange
import utils.covers as covers


def find_book_image(isbn):
    """Find the image file associated with a book, and return its path.

    :param isbn: The ISBN of the book.
    """

    imagefile = covers.find(isbn)

    if imagefile is None:
        return 'static/nocover.png'
    else:
        return 'static/uploads/{}'.format(imagefile)


def are_unread_books():
    """Check if there are any unread books in the database.
    """

    return DBSession.query(Book).filter(~Book.read).count() > 0


def next_book():
    """Pick a random unread book
    """

    books = DBSession.query(Book).filter(~Book.read)

    return books[randrange(0, books.count())]
