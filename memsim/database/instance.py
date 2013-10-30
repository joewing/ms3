
import sys

from . import couch
from . import simple


_db_instance = None


def get_instance(model='', url=None):
    """Get a database instance."""
    global _db_instance
    if _db_instance is not None:
        return _db_instance

    # First try to connect to couch.
    db = couch.CouchDatabase(model, url)
    if db.load():
        print("Connected to CouchDB")
        _db_instance = db
        return db

    # If a database URL was provided, but we were unable to connect, we exit.
    if url is not None:
        print("ERROR: could not connect to database: " + str(url))
        sys.exit(-1)

    # Fall back to the local database.
    print("Using local database")
    db = simple.SimpleDatabase(model)
    _db_instance = db
    return db


def set_instance(db):
    """Set the database instance to use (for debugging)."""
    global _db_instance
    _db_instance = db
