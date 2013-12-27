
import os
import sys
import threading

from memsim.database import simple, pg

db_lock = threading.Lock()
db_instance = None


def connect_pg(url):
    global db_instance
    db = pg.PGDatabase(url)
    if db.connect():
        sys.stderr.write('Connected to PostgreSQL\n')
        db_instance = db
        return db
    print('ERROR: could not connect to PostgreSQL\n')
    sys.exit(-1)


def get_instance(url=None):
    """Get a database instance."""
    global db_instance

    with db_lock:

        if db_instance is not None:
            return db_instance

        # Handle an explicit URL.
        if url:
            return connect_pg(url)

        # Check for Postgres.
        if 'PSQL_URL' in os.environ:
            return connect_pg(os.environ['PSQL_URL'])

        # Fall back to the local database.
        sys.stderr.write('Using local database\n')
        db_instance = simple.SimpleDatabase()
        return db_instance


def set_instance(db):
    """Set the database instance to use (for debugging)."""
    global db_instance
    with db_lock:
        db_instance = db
