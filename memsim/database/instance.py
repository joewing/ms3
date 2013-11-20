
import sys

from memsim.database import couch, simple


_instance = None


def get_instance(url=None):
    """Get a database instance."""
    global _instance
    if _instance is not None:
        return _instance

    # First try to connect to couch.
    db = couch.CouchDatabase(url)
    if db.connect():
        sys.stderr.write('Connected to CouchDB\n')
        _instance = db
        return db

    # If a database URL was provided, but we were unable to connect, we exit.
    if url:
        print('ERROR: could not connect to database:', url)
        sys.exit(-1)

    # Fall back to the local database.
    sys.stderr.write('Using local database\n')
    db = simple.SimpleDatabase()
    _instance = db
    return db


def set_instance(db):
    """Set the database instance to use (for debugging)."""
    global _instance
    _instance = db
