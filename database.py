
import json
import os
import sys

sys.path.insert(0,'/usr/local/lib/python2.7/dist-packages')
sys.path.insert(0,'/usr/lib/python2.7/dist-packages')
from boto.s3.connection import S3Connection
from boto.s3.key import Key

class Database:
   """Key-value store for saving state."""

   def __init__(self, name):
      """Load state from the named file."""
      try:
         self.name = name
         akey = os.environ['AWS_ACCESS_KEY']
         skey = os.environ['AWS_SECRET_KEY']
         self.conn = S3Connection(akey, skey)
         self.bucket = self.conn.get_bucket('ms3.out')
         self.data = dict()
         k = Key(self.bucket)
         k.key = self.name
         self.data = json.loads(k.get_contents_as_string())
      except Exception as e:
         print("no state loaded: " + str(e))
         pass

   def save(self):
      """Save state."""
      try:
         k = Key(self.bucket)
         k.key = self.name
         k.set_contents_from_string(json.dumps(self.data))
      except Exception as e:
         print("ERROR: could not save state: " + str(e))

   def has_value(self, key):
      """Determine if the specified key exists."""
      return key in self.data

   def get_value(self, key):
      """Get the value associated with a key."""
      return self.data[key]

   def set_value(self, key, value):
      """Set the value associated with a key."""
      self.data[key] = value

