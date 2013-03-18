from models import Base
from sqlalchemy import Boolean, Column, Date, Integer, String


class Book(Base):
    """A class to represent a book. A book has an ISBN, title, list of
    authors, read flag, last read date, location, and borrower.

    Unfortunately sqlalchemy has no list column type, and so the
    authors stored as an ampersand-sepaarted string.
    """

    __tablename__ = 'books'

    # PEP8 doesn't like having multiple spaces after the operator, but
    # I don't care. It is so much more readible when aligned like
    # this.
    id       = Column(Integer, primary_key=True)
    isbn     = Column(String, unique=True)
    title    = Column(String)
    author   = Column(String)
    read     = Column(Boolean)
    lastread = Column(Date)
    location = Column(String)
    borrower = Column(String)

    def __init__(self, isbn, title, author, read, lastread,
                 location, borrower):
        """Create a new book from the given data.

        :param isbn: The ISBN number of the book, this cannot already
            be in the database.
        :param title: The title of the book.
        :param author: List of authors of the book, in the format
            "Surname, Forename Initials", with ampersands separating
            different authors.
        :param read: Whether the book has been read or not
        :param lastread: The date on which the book was last read
            (invalid if read = False)
        :param location: Location of the book.
        :param borrower: Borrower of the book (may be empty)
        """

        self.isbn     = isbn
        self.title    = title
        self.author   = author
        self.read     = read
        self.lastread = lastread
        self.location = location
        self.borrower = borrower

    def authors(self):
        """Get the authors of a book in list form.
        """

        return [author.strip()
                for author in self.author.split('&')]

    @staticmethod
    def unstring(field):
        """Take the name of a field and return the field. Return None
        if the name is bad.

        :param field: The name of the field.
        """

        try:
            return {'isbn':     Book.isbn,
                    'title':    Book.title,
                    'author':   Book.author,
                    'read':     Book.read,
                    'lastread': Book.lastread,
                    'location': Book.location,
                    'borrower': Book.borrower}[field]
        except:
            return None
