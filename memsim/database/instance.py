
import os
import sys
import threading

from memsim.database import simple, pg


db_instance = threading.local()


def connect_pg(url):
    global db_instance
    db = pg.PGDatabase(url)
    if db.connect():
        sys.stderr.write('Connected to PostgreSQL\n')
        db_instance.db = db
        return db
    print('ERROR: could not connect to PostgreSQL\n')
    sys.exit(-1)


def get_instance(url=None):
    """Get a database instance."""
    global db_instance

    db = getattr(db_instance, 'db', None)
    if db:
        return db

    # Handle an explicit URL.
    if url:
        return connect_pg(url)

    # Check for Postgres.
    if 'PSQL_URL' in os.environ:
        return connect_pg(os.environ['PSQL_URL'])

    # Fall back to the local database.
    sys.stderr.write('Using local database\n')
    db_instance.db = simple.SimpleDatabase()
    return db_instance.db


def set_instance(db):
    """Set the database instance to use (for debugging)."""
    global db_instance
    db_instance.db = db
