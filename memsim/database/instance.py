
from __future__ import print_function
import re
import os
import sys

from memsim.database import sql

db_instance = None


def clean_url(url):
    """Return a copy of the URL with the username/password removed."""
    return re.sub(r'^(.+://)[^:]+:[^@]+@(.*)$', r'\1*:*@\2', url)


def connect_sql(url):
    global db_instance
    db = sql.SQLDatabase(url)
    if db.connect():
        disp_url = clean_url(url)
        print('Connected to', disp_url, file=sys.stderr)
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
        return connect_sql(url)

    # Check for PostgreSQL URL.
    if 'PSQL_URL' in os.environ:
        return connect_sql(os.environ['PSQL_URL'])

    # Fall back to the local database.
    return connect_sql('sqlite://')


def set_instance(db):
    """Set the database instance to use."""
    global db_instance
    db_instance = db
