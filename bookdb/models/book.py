from models import Base
from sqlalchemy import BigInteger, Boolean, Column, Date, PickleType, String


class Book(Base):
    """A class to represent a book. A book has an ISBN, title, list of
    authors, read flag, last read date, location, and borrower.

    Unfortunately sqlalchemy has no list column type, and so the
    authors are pickled.
    """

    __tablename__ = 'books'

    # PEP8 doesn't like having multiple spaces after the operator, but
    # I don't care. It is so much more readible when aligned like
    # this.
    id       = Column(BigInteger, primary_key=True)
    isbn     = Column(String, unique=True)
    title    = Column(String)
    authors  = Column(PickleType)
    read     = Column(Boolean)
    lastread = Column(Date)
    location = Column(String)
    borrower = Column(String)

    def __init__(self, isbn, title, authors, read, lastread,
                 location, borrower):
        """Create a new book from the given data.

        :param isbn: The ISBN number of the book, this cannot already
            be in the database.
        :param title: The title of the book.
        :param authors: List of authors of the book, in the format
            "Surname, Forename Initials"
        :param read: Whether the book has been read or not
        :param lastread: The date on which the book was last read
            (invalid if read = False)
        :param location: Location of the book.
        :param borrower: Borrower of the book (may be empty)
        """

        self.isbn     = isbn
        self.title    = title
        self.authors  = authors
        self.read     = read
        self.lastread = lastread
        self.location = location
        self.borrower = borrower
