
import sys

import couch
import simple

_db_instance = None

def get_instance(model='', url=None):
   """Get a database instance."""
   global _db_instance
   if _db_instance != None:
      return _db_instance

   # First try to connect to couch.
   db = couch.CouchDatabase(model, url)
   if db.load():
      print("Connected to CouchDB")
      _db_instance = db
      return db

   # If a database URL was provided, but we were unable to connect, we exit.
   if url != None:
      print("ERROR: could not connect to database: " + str(url))
      sys.exit(-1)

   # Fall back to the local database.
   print("Using local database")
   db = simple.SimpleDatabase(model)
   _db_instance = db
   return db

