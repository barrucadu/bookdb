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
