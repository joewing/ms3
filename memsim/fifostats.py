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
        data = self.stats[index]
        pdata = data.get('pdata', [])
        cdata = data.get('cdata', [])
        return pdata, cdata

    def update(self, index, pdata, cdata):
        index = str(index)
        if index in self.stats:
            data = self.stats[index]
        else:
            data = dict()
            self.stats[index] = data
        if len(pdata) > 0:
            data['pdata'] = pdata
        if len(cdata) > 0:
            data['cdata'] = cdata

    def combine(self, other):
        for index, values in other.stats.iteritems():
            pdata = values.get('pdata', [])
            cdata = values.get('cdata', [])
            self.update(index, pdata, cdata)
