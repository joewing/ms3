
from __future__ import print_function
import os
import sys

from memsim.database import sql

db_instance = None


def connect_sql(url, dbname):
    global db_instance
    db = sql.SQLDatabase(url, dbname)
    if db.connect():
        print('Connected to', url, file=sys.stderr)
        db_instance = db
        return db
    print('ERROR: could not connect to PostgreSQL\n')
    sys.exit(-1)


def get_instance(url=None):
    """Get a database instance."""
    global db_instance

    if db_instance is not None:
        return db_instance

    # Handle an explicit URL.
    if url:
        return connect_sql(url, 'ms3')

    # Check for PostgreSQL URL.
    if 'PSQL_URL' in os.environ:
        return connect_sql(os.environ['PSQL_URL'], 'ms3')

    # Fall back to the local database.
    db_instance = connect_sql('sqlite://', None)
    return db_instance


def set_instance(db):
    """Set the database instance to use."""
    global db_instance
    db_instance = db
