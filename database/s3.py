
import json
import os

from boto.utils import get_instance_metadata
from boto.s3.connection import S3Connection
from boto.s3.key import Key

import base


class S3Database(base.Database):

    def __init__(self, m):
        """Initialize."""
        self.data = dict()
        self.name = None
        try:
            akey = os.environ['AWS_ACCESS_KEY']
            skey = os.environ['AWS_SECRET_KEY']
            self.conn = S3Connection(akey, skey)
            self.bucket = self.conn.get_bucket(os.environ['AWS_S3_BUCKET'])
        except Exception as e:
            print("ERROR: could not connect to the database: " + str(e))

    def list(self):
        """Get a list of named states."""
        for n in self.bucket.list():
            yield n

    def load(self, name):
        """Load the specified named state."""
        self.name = name
        try:
            k = Key(self.bucket)
            k.key = name
            self.data = json.loads(k.get_contents_as_string())
        except Exception as e:
            print("state not loaded: " + str(e))

    def set_instance(self):
        if 'instance' in self.data:
            return False
        else:
            meta = get_instance_metadata()
            instance = meta['instance-id']
            self.set_value('instance', instance)
            return True

    def clear_instance(self):
        if 'instance' in self.data:
            del self.data['instance']

    def get_instance(self):
        return self.get_value('instance')

    def save(self):
        """Save state."""
        if self.name is None:
            print("ERROR: no state loaded")
            return
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
        return self.data.get(key)

    def set_value(self, key, value):
        """Set the value associated with a key."""
        self.data[key] = value
