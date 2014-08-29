import json

class FIFOStats(object):

    def __init__(self):
        self.stats = dict()     # FIFO index -> name -> value

    def __str__(self):
        return json.dumps(self.stats)

    def update(self, index, item_count, prod_var, cons_var):
        if index in self.stats:
            data = self.stats[index]
        else:
            data = dict()
            self.stats[index] = data
        if item_count > 0:
            data['items'] = item_count
        if prod_var > 0.0:
            data['pvar'] = prod_var
        if cons_var > 0.0:
            data['cvar'] = cons_var

    def combine(self, other):
        for index, values in other.iteritems():
            item_count = values['items']
            pvar = values['pvar']
            cvar = values['cvar']
            self.update(index, item_count, pvar, cvar)
