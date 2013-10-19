
import uuid
import hashlib
import couchdb.client
import base

class CouchDatabase(base.Database):

   def __init__(self, m):
      base.Database.__init__(self, m)
      self.dbname = 'ms3'
      self.model_hash = hashlib.sha1(self.model).hexdigest()

   def _create_views(self):
      doc = {
         "language": "javascript",
         "views": {
            "results": {
               "map": """function(d) {
                  if(d.type == 'result') {
                     emit([d.model, d.memory], d.value);
                  }
               }""",
            },
            "state": {
               "map": """function(d) {
                  if(d.type == 'state') {
                     emit([d.model, d.key],
                          {'value':d.value, 'id':d._id, 'rev':d._rev});
                  }
               }"""
            }
         }
      }
      self.db['_design/ms3'] = doc

   def load(self):
      self.server = couchdb.client.Server()
      try:
         if self.dbname in self.server:
            self.db = self.server[self.dbname]
         else:
            self.db = self.server.create(self.dbname)
            self._create_views()
         return True
      except:
         return False

   def save(self):
      pass

   def get_result(self, mem):
      """Get a result from the database."""
      mem_hash = hashlib.sha1(str(mem)).hexdigest()
      for r in self.db.view('ms3/results', key=[self.model_hash, mem_hash]):
         return r.value
      return None

   def add_result(self, mem, value):
      """Insert a result to the database."""
      doc_id = uuid.uuid4().hex
      mem_hash = hashlib.sha1(str(mem)).hexdigest()
      doc = {
         'type': 'result',
         'model': self.model_hash,
         'memory': mem_hash,
         'value': value
      }
      self.db[doc_id] = doc

   def set_value(self, key, value):
      """Set a value for the current state."""
      doc = {
         '_id': uuid.uuid4().hex,
         'type': 'state',
         'model': self.model_hash,
         'key': key,
         'value': value,
      }
      for r in self.db.view('ms3/state', key=[self.model_hash, key]):
         doc['_id'] = r.value['id']
         doc['_rev'] = r.value['rev']
         break
      self.db.save(doc)

   def get_value(self, key, default = None):
      """Get a value from the current state."""
      for r in self.db.view('ms3/state', key=[self.model_hash, key]):
         return r.value['value']
      return default

