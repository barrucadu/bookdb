"""models: A module to handle connections to and operations on the
database.
"""

from sqlalchemy.ext.declarative import declarative_base
from sqlalchemy.orm import scoped_session, sessionmaker

# Make a database session factory and a base for our models to inherit
# from.
DBSession = scoped_session(sessionmaker())
Base = declarative_base()


def initialise_sql(engine):
    """Initialise a new SQL database.

    :param engine: The database engine to use.
    """

    DBSession.configure(bind=engine)
    Base.metadata.bind = engine
    Base.metadata.create_all(engine)
