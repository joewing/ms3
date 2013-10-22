
import couch
import simple

_db_instance = None

def get_instance(model = '', url = 'http://127.0.0.1:5984'):
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

   # Fall back to the local database.
   print("Could not connect to database")
   db = simple.SimpleDatabase(model)
   _db_instance = db
   return db

