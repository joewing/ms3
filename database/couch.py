
import uuid
import json
import couchdb.client
import base

class CouchDatabase(base.Database):
   """CouchDB database connector."""

   def __init__(self, m = ''):
      base.Database.__init__(self, m)
      self.dbname = 'ms3'
      self.model_hash = self.get_hash(self.model)
      self.state = dict()
      self.results = dict()

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
                     emit(d.model, {'value':d.value, 'id':d._id, 'rev':d._rev});
                  }
               }"""
            },
            "model_list": {
               "map": """function(d) {
                  emit(d.model, d._id);
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
         for r in self.db.view('ms3/state', key=self.model_hash):
            self.state = json.loads(r.value['value'])
            break
         return True
      except:
         return False

   def save(self):
      doc = {
         '_id': uuid.uuid4().hex,
         'type': 'state',
         'model': self.model_hash,
         'value': json.dumps(self.state),
      }
      for r in self.db.view('ms3/state', key=self.model_hash):
         doc['_id'] = r.value['id']
         doc['_rev'] = r.value['rev']
         break
      self.db.save(doc)

   def get_result(self, mem):
      """Get a result from the database."""
      mem_hash = self.get_hash(mem)
      if mem_hash in self.results:
         return self.results[mem_hash]
      for r in self.db.view('ms3/results', key=[self.model_hash, mem_hash]):
         self.results[mem_hash] = r.value
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
      self.results[mem_hash] = value

   def get_states(self):
      """Generator to return all persisted states."""
      for r in self.db.view('ms3/state'):
         yield json.loads(r.value['value'])

   def remove(self, h):
      """Remove data for the specified hash."""
      for r in self.db.view('ms3/model_list', key=h):
         del self.db[r.value]

   def compact(self):
      """Perform database compaction."""
      self.db.compact()

