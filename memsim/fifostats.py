import json

class FIFOStats(object):

    def __init__(self, s=None):
        if s is not None:
            self.stats = json.loads(s)
        else:
            self.stats = dict()

    def __str__(self):
        return json.dumps(self.stats)

    def get_stats(self, index):
        index = str(index)
        return self.stats[index]

    def update(self, index, data):
        index = str(index)
        self.stats[index] = data;

    def combine(self, other):
        for index, data in other.stats.iteritems():
            self.update(index, data)
