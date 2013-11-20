
import os
import sys

from memsim.database import couch, simple, pg


_instance = None


def connect_couch(url):
    global _instance
    db = couch.CouchDatabase(url)
    if db.connect():
        sys.stderr.write('Connected to CouchDB\n')
        _instance = db
        return db
    print('ERROR: could not connect to CouchDB\n')
    sys.exit(-1)


def connect_pg(url):
    global _instance
    db = pg.PGDatabase(url)
    if db.connect():
        sys.stderr.write('Connected to PostgreSQL\n')
        _instance = db
        return db
    print('ERROR: could not connect to PostgreSQL\n')
    sys.exit(-1)


def get_instance(url=None):
    """Get a database instance."""
    global _instance
    if _instance is not None:
        return _instance

    # Handle an explicit URL.
    if url:
        if url[0:9] == 'postgresql':
            return connect_pg(url)
        else:
            return connect_couch(url)

    # Check for Postgres.
    if 'PSQL_URL' in os.environ:
        return connect_pg(os.environ['PSQL_URL'])

    # Check for CouchDB.
    if 'COUCHDB_URL' in os.environ:
        return connect_couch(os.environ['COUCHDB_URL'])

    # Fall back to the local database.
    sys.stderr.write('Using local database\n')
    _instance = simple.SimpleDatabase()
    return _instance


def set_instance(db):
    """Set the database instance to use (for debugging)."""
    global _instance
    _instance = db
