
from __future__ import print_function
import json
import os

from boto.s3.connection import S3Connection
from boto.s3.key import Key

from memsim.database import base


class S3Database(base.Database):

    def __init__(self, m=''):
        """Initialize."""
        base.Database.__init__(self, m)
        self.model_hash = self.get_hash(self.model)
        self.state = dict()
        self.results = dict()

    def load(self):
        """Load the current model state from the database."""
        akey = os.environ['AWS_ACCESS_KEY']
        skey = os.environ['AWS_SECRET_KEY']
        bucket_name = os.environ['AWS_S3_BUCKET']

        # Attempt to connect to the database.
        print("Trying ", bucket_name)
        try:
            self.conn = S3Connection(akey, skey)
            self.bucket = self.conn.get_bucket(bucket_name)
        except:
            return False

        # Attempt to read the current state.
        # If this fails, it means that we are to create a new state.
        try:
            key = '-'.join(['state', self.model_hash])
            k = Key(self.bucket, name=key)
            self.state = json.loads(k.get_contents_as_string())
        except:
            pass
        return True

    def save(self):
        """Save the current model state to the database."""
        key = '-'.join(['state', self.model_hash])
        k = Key(self.bucket, name=key)
        k.set_contents_from_string(json.dumps(self.state))

    def _get_key(self, prefix, name):
        """Get the key name for an entry in the database."""
        return '-'.join([prefix, self.model_hash, self.get_hash(name)])

    def _get(self, prefix, name):
        """Load a value from the database."""
        key = self._get_key(prefix, name)
        if key in self.results:
            return self.results[key]
        k = self.bucket.get_key(key)
        return json.loads(k.get_contents_as_string()) if k else None

    def _put(self, prefix, name, value):
        """Store a value to the database."""
        key = self._get_keY(prefix, name)
        self.results[key] = value
        k = Key(self.bucket, name=key)
        k.set_contents_from_string(json.dumps(value))

    def get_result(self, mem):
        """Get a result from the database."""
        return self._get('result', mem)

    def add_result(self, mem, value):
        """Insert a result to the database."""
        self._put('result', mem, value)

    def get_fpga_result(self, name):
        """Get FPGA timing data from the database."""
        return self._get('fpga', name)

    def add_fpga_result(self, name, frequency, bram_count):
        """Add an FPGA timing result to the database."""
        self._put('fpga', name, (frequency, bram_count))

    def get_cacti_result(self, name):
        """Get CACTI timing data from the database."""
        return self._get('cacti', name)

    def add_cacti_result(self, name, access_time, cycle_time, area):
        """Add CACTI timing data to the database."""
        value = (access_time, cycle_time, area)
        self._put('cacti', name, value)

    def get_states(self):
        """Generator to return all persisted states."""
        for k in self.bucket.list(prefix='state-'):
            yield json.loads(k.get_contents_as_string())

    def remove(self, h):
        """Remove data for the specified model hash."""

        # Remove results.
        result_prefix = '-'.join(['result', self.model_hash, ''])
        for k in self.bucket.list(prefix=result_prefix):
            k.delete()

        # Remove the state.
        state_key = '-'.join(['state', self.model_hash])
        k = Key(self.bucket, name=state_key)
        k.delete()
