"""Mutator views: for handling updates to the database.

add:    Add a new book to the database.
edit:   Update an existing book in the database.
delete: Delete a book from the database.
"""

from pyramid.view import view_config
from models import DBSession
from models.book import Book
from datetime import date
import dirs
import os


@view_config(route_name='add', renderer='bookform.mako')
def add_view(request):
    """Display a form to add a new book.

    :param request: The request object.
    """

    return {'pagetitle': 'BookDB :: Add',
            'target':    '/add',
            'submit':    'Add',
            'book':      Book()}


@view_config(route_name='edit', renderer='bookform.mako')
def edit_view(request):
    """Display a form to edit a book.

    :param request: The request object.
    """

    # TODO: Display an error page if the book does not exist.
    isbn = request.matchdict["isbn"]
    book = DBSession.query(Book).filter(Book.isbn == isbn).one()

    return {'pagetitle': 'BookDB :: Edit',
            'target':    '/{}/edit'.format(isbn),
            'submit':    'Edit',
            'book':      book}


@view_config(route_name='delete', renderer='confirmdelete.mako')
def delete_view(request):
    """Ask for confirmation to delete a book.

    :param request: The request object.
    """

    # TODO: Display an error page if the book does not exist.
    isbn = request.matchdict["isbn"]
    book = DBSession.query(Book).filter(Book.isbn == isbn).one()

    return {'pagetitle': 'Confirm Delete',
            'isbn': isbn,
            'title': book.title,
            'author': ' & '.join(book.authors())}


@view_config(route_name='addp', renderer='information.mako')
def add_post_view(request):
    """Process a book addition and display a confirmation.

    :param request: The request object.
    """

    # TODO: Better error messages.
    try:
        newbook = mutate(Book(), request)
        upload_cover(request)

        DBSession.add(newbook)
        DBSession.commit()

        return {'pagetitle': 'Add Successful',
                'message':   'The book has been added to the database.'}
    except:
        DBSession.rollback()
        return {'pagetitle': 'Add Failed',
                'message':   'An error occurred whilst adding the book.'}


@view_config(route_name='editp', renderer='information.mako')
def edit_post_view(request):
    """Process a book edit and display a confirmation.

    :param request: The request object.
    """

    # TODO: Better error messages.
    try:
        isbn = request.matchdict['isbn']
        upload_cover(request)

        mutate(DBSession.query(Book).filter(Book.isbn == isbn).one(),
               request)

        DBSession.commit()

        return {'pagetitle': 'Edit Successful',
                'message':   'The book has been updated in the database.'}
    except:
        DBSession.rollback()
        return {'pagetitle': 'Edit Failed',
                'message':   'An error occurred whilst updating the book.'}


@view_config(route_name='deletep', renderer='information.mako')
def delete_post_view(request):
    """Process a book deletion and display a confirmation.

    :param request: The request object.
    """

    # TODO: Better error messages.
    try:
        isbn = request.matchdict['isbn']
        book = DBSession.query(Book).filter(Book.isbn == isbn).one()
        delete_cover(isbn)
        DBSession.delete(book)
        DBSession.commit()

        return {'pagetitle': 'Delete Successful',
                'message':   'The book has been deleted from the database.'}
    except:
        DBSession.rollback()
        return {'pagetitle': 'Delete Failed',
                'message':   'An error occurred whilst deleting the book.'}


def mutate(book, request):
    """Adding and editing a book are basically the same operation: you
    take a new or already existing book, replace all its data with the
    new data, and update the database. This can be abstracted.

    :param book: The book object to update
    :param request: The request to update the book from
    """

    book.mutate(request.POST['isbn'],
                request.POST['title'],
                sort_authors(request.POST['author']),
                'read' in request.POST,
                last_read_date(request),
                request.POST['location'],
                request.POST['borrower'],
                request.POST['quote'],
                request.POST['notes'])

    return book


def sort_authors(authors):
    """The user can enter a list of authors separated by ampersands in
    any order, but we really want them to be sorted internally, as
    this makes sorting by author a really trivial operation. So, this
    function takes an author string and sorts it.

    >>> sort_authors("Zidane, Zealot Z. & Aardvark, Alvis A.")
    "Aardvark, Alvis A. & Zidane, Zealot Z."

    :param authors: The unsorted author string
    """

    authorlist = [author.strip()
                  for author in authors.split('&')]
    authorlist.sort()
    return ' & '.join(authorlist)


def last_read_date(request):
    """The last read date of a book is mandatory in the database, but
    this is hidden from the user. Why require a value for this if the
    book isn't read? Why even require one if it is read? It may be
    unknown. This function looks at the request for a last read date,
    and makes use of it if found. Otherwise, a default value is
    used. To further complicate things, the last read date is sent as
    a string, rather than a better data structure, so we need to parse
    it as well.

    :param request: The request object, it is assumed the last read
        date is request.POST['lastread']
    """

    try:
        datelist = list(map(int, request.POST['lastread'].split('-')))
        return date(datelist[0], datelist[1], datelist[2])
    except:
        return date.min


def upload_cover(request):
    """Books may have covers associated with them, but only a few
    types of file are supported. We need to check to see if an image
    is provided and, if it the correct type, save it to the
    appropriate place.

    :param request: The request object
    """

    # Firstly check if an image has been sent
    if 'cover' not in request.POST.keys() or request.POST['cover'] == b'':
        return

    # Extract the data and file extension
    ext = request.POST['cover'].filename[-3:]
    data = request.POST['cover'].file

    # Check the file extension
    if ext not in ['png', 'gif', 'jpg']:
        return

    # Delete the current cover
    delete_cover(request.POST['isbn'])

    # Save the new one
    with open(os.path.join(
            dirs.uploads,
            '{}.{}'.format(request.POST['isbn'], ext)), 'wb') as f:
        f.write(data.read())


def delete_cover(isbn):
    """Deletes the cover image, if there is one, associated with the
    given ISBN.

    :param isbn: Book to remove the cover of
    """

    for ext in ['png', 'gif', 'jpg']:
        img = os.path.join(dirs.uploads, isbn + '.' + ext)
        if os.path.exists(img):
            os.remove(img)
