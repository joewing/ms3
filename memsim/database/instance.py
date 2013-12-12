
import os
import sys

from memsim.database import simple, pg


_instance = None


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
        return connect_pg(url)

    # Check for Postgres.
    if 'PSQL_URL' in os.environ:
        return connect_pg(os.environ['PSQL_URL'])

    # Fall back to the local database.
    sys.stderr.write('Using local database\n')
    _instance = simple.SimpleDatabase()
    return _instance


def set_instance(db):
    """Set the database instance to use (for debugging)."""
    global _instance
    _instance = db
