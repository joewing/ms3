import json

class FIFOStats(object):

    def __init__(self):
        self.stats = dict()     # FIFO index -> name -> value

    def __str__(self):
        return json.dumps(self.stats)

    def get_stats(self, index):
        data = self.stats[index]
        items = data['items']
        pvar = data['pvar']
        ptime = data['ptime']
        cvar = data['cvar']
        ctime = data['ctime']
        return items, ptime, pvar, ctime, cvar

    def update(self, index, item_count, ptime, pvar, ctime, cvar):
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
        for index, values in other.iteritems():
            item_count = values['items']
            ptime = values['ptime']
            pvar = values['pvar']
            ctime = values['ctime']
            cvar = values['cvar']
            self.update(index, item_count, ptime, pvar, ctime, cvar)
