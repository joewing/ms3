
import sys

from memsim.database import couch, simple


_instances = dict()
_last_model = ''


def get_instance(model=None, url=None):
    """Get a database instance."""
    global _instances, _last_model
    if model:
        smodel = str(model)
    else:
        smodel = _last_model
    if smodel in _instances:
        return _instances[smodel]

    # First try to connect to couch.
    db = couch.CouchDatabase(model, url)
    if db.load():
        sys.stderr.write('Connected to CouchDB\n')
        _instances[smodel] = db
        _last_model = smodel
        return db

    # If a database URL was provided, but we were unable to connect, we exit.
    if url:
        print('ERROR: could not connect to database:', url)
        sys.exit(-1)

    # Fall back to the local database.
    sys.stderr.write('Using local database\n')
    db = simple.SimpleDatabase(model)
    _instances[smodel] = db
    _last_model = smodel
    return db


def set_instance(db, model=''):
    """Set the database instance to use (for debugging)."""
    global _instances
    smodel = str(model)
    _instances[smodel] = db
