
from __future__ import print_function
import uuid
import json
import sys
import couchdb.client

from memsim.database import base


BATCH_SIZE = 1024


class CouchDatabase(base.Database):
    """CouchDB database connector."""

    def __init__(self, m='', url=None):
        base.Database.__init__(self, m)
        self.dbname = 'ms3'
        self.url = url
        self.model_hash = self.get_hash(self.model)
        self.state = dict()
        self.results = dict()
        self.fpga_results = dict()
        self.cacti_results = dict()
        self.url = url if url else 'http://127.0.0.1:5984'

    def _create_views(self):
        """Create the necessary map-reduce views."""
        doc = {
            "language": "javascript",
            "views": {
                "results": {
                    "map": """function(d) {
                        if(d.type == 'result') {
                            emit([d.model, d.memory], d.value);
                        }
                    }"""
                },
                "state": {
                    "map": """function(d) {
                        if(d.type == 'state') {
                            emit(d.model, {'value':d.value,
                                           'id':d._id,
                                           'rev':d._rev});
                        }
                    }"""
                },
                "fpga_results": {
                    "map": """function(d) {
                        if(d.type == 'fpga') {
                            emit(d.key, [d.frequency, d.bram_count]);
                        }
                    }"""
                },
                "cacti_results": {
                    "map": """function(d) {
                        if(d.type == 'cacti') {
                            emit(d.key, [d.access_time, d.cycle_time, d.area]);
                        }
                    }"""
                },
                "model_list": {
                    "map": """function(d) {
                        if(d.model) {
                            emit(d.model, d._id);
                        }
                    }"""
                }
            }
        }
        self.db['_design/ms3'] = doc

    def load(self):
        """Load the current model state from the database."""
        if couchdb.client is None:
            sys.stderr.write('Could not load couchdb.client\n')
            return False
        sys.stderr.write('Trying ' + str(self.url) + '\n')
        self.server = couchdb.client.Server(url=self.url)
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
        """Save the current model state to the database."""
        doc = {
            '_id': uuid.uuid4().hex,
            'type': 'state',
            'model': self.model_hash,
            'value': json.dumps(self.state),
        }
        for r in self.db.iterview('ms3/state', BATCH_SIZE,
                                  key=self.model_hash):
            doc['_id'] = r.value['id']
            doc['_rev'] = r.value['rev']
            break
        self.db.save(doc)

    def get_result(self, mem):
        """Get a result from the database."""
        mem_hash = self.get_hash(mem)
        if mem_hash in self.results:
            return self.results[mem_hash]
        for r in self.db.iterview('ms3/results', BATCH_SIZE,
                                  key=[self.model_hash, mem_hash]):
            self.results[mem_hash] = r.value
            return r.value
        return None

    def add_result(self, mem, value):
        """Insert a result to the database."""
        doc_id = uuid.uuid4().hex
        mem_hash = self.get_hash(mem)
        doc = {
            'type': 'result',
            'model': self.model_hash,
            'memory': mem_hash,
            'value': value
        }
        self.db[doc_id] = doc
        self.results[mem_hash] = value

    def get_fpga_result(self, name):
        """Get FPGA timing data from the database."""
        key_hash = self.get_hash(name)
        if key_hash in self.fpga_results:
            return self.fpga_results[key_hash]
        for r in self.db.iterview('ms3/fpga_results', BATCH_SIZE,
                                  key=key_hash):
            self.fpga_results[key_hash] = r.value
            return r.value
        return None

    def add_fpga_result(self, name, frequency, bram_count):
        """Add FPGA timing data to the database."""
        key_hash = self.get_hash(name)
        doc_id = uuid.uuid4().hex
        doc = {
            'type': 'fpga',
            'key': key_hash,
            'frequency': frequency,
            'bram_count': bram_count,
            'memory': name
        }
        self.db[doc_id] = doc
        self.fpga_results[key_hash] = (frequency, bram_count)

    def get_cacti_result(self, name):
        """Get CACTI timing information from the database."""
        key_hash = self.get_hash(name)
        if key_hash in self.cacti_results:
            return self.cacti_results[key_hash]
        for r in self.db.iterview('ms3/cacti_results', BATCH_SIZE,
                                  key=key_hash):
            self.cacti_results[key_hash] = r.value
            return r.value
        return None

    def add_cacti_result(self, name, access_time, cycle_time, area):
        """Add CACTI timing information to the database."""
        key_hash = self.get_hash(name)
        doc_id = uuid.uuid4().hex
        doc = {
            'type': 'cacti',
            'key': key_hash,
            'access_time': access_time,
            'cycle_time': cycle_time,
            'area': area,
            'parameters': str(name)
        }
        self.db[doc_id] = doc
        self.cacti_results[key_hash] = (access_time, cycle_time, area)

    def get_states(self):
        """Generator to return all persisted states."""
        for r in self.db.iterview('ms3/state', BATCH_SIZE):
            yield json.loads(r.value['value'])

    def get_results(self, model_hash):
        """Generator to return all results for the specified model hash."""
        for r in self.db.iterview('ms3/results', BATCH_SIZE,
                                  key1=model_hash):
            yield r.key[1], r.value

    def get_fpga_results(self):
        """Generator to return all FPGA results."""
        for r in self.db.iterview('ms3/fpga_results', BATCH_SIZE):
            yield r.key, r.value

    def get_cacti_results(self):
        """Generator to return all CACTI results."""
        for r in self.db.iterview('ms3/cacti_results', BATCH_SIZE):
            yield r.key, r.value

    def remove(self, h):
        """Remove data for the specified hash."""
        for r in self.db.iterview('ms3/model_list', BATCH_SIZE, key=h):
            del self.db[r.value]

    def compact(self):
        """Perform database compaction."""
        self.db.compact()

    def remove_fpga(self):
        """Remove invalid FPGA results."""
        last_key = ''
        last_frequency = 0
        last_bram_count = 0
        for r in self.db.view('ms3/fpga_results', BATCH_SIZE):
            if r.value[0] == 1:
                print('Removing invalid:', r.id)
                del self.db[r.id]
            elif r.key == last_key:
                print('Removing duplicate:', r.id)
                if last_frequency != r.value[0]:
                    print('WARN: frequency mismatch; key:', r.key)
                elif last_bram_count != r.value[1]:
                    print('WARN: bram_count mismatch; key:', r.key)
                else:
                    del self.db[r.id]
            else:
                last_key = r.key
                last_frequency = r.value[0]
                last_bram_count = r.value[1]
