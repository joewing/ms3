import json

class FIFOStats(object):

    def __init__(self, s='{}'):
        self.stats = json.loads(s)  # FIFO index -> name -> value

    def __str__(self):
        return json.dumps(self.stats)

    def get_stats(self, index):
        index = str(index)
        data = self.stats[index]
        items = data.get('items', 0)
        pvar = data.get('pvar', 0)
        ptime = data.get('ptime', 0)
        cvar = data.get('cvar', 0)
        ctime = data.get('ctime', 0)
        return items, ptime, pvar, ctime, cvar

    def update(self, index, item_count, ptime, pvar, ctime, cvar):
        index = str(index)
        if index in self.stats:
            data = self.stats[index]
        else:
            data = dict()
            self.stats[index] = data
        if item_count > 0:
            data['items'] = item_count
        if ptime > 0:
            data['ptime'] = ptime
            data['pvar'] = pvar
        if ctime > 0:
            data['ctime'] = ctime
            data['cvar'] = cvar

    def combine(self, other):
        for index, values in other.stats.iteritems():
            item_count = values.get('items', 0)
            ptime = values.get('ptime', 0)
            pvar = values.get('pvar', 0)
            ctime = values.get('ctime', 0)
            cvar = values.get('cvar', 0)
            self.update(index, item_count, ptime, pvar, ctime, cvar)
