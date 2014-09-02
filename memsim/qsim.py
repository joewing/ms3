import math
from random import Random

from priorityqueue import PriorityQueue


MAX_ITER_DEFAULT = 10000
MAX_COV_DEFAULT = 1e-4


class Queue(object):

    def __init__(self, rand, count, depth,
                 prod_time, prod_var, cons_time, cons_var):
        assert(depth > 0)
        assert(count > 0)
        assert(prod_time > 0)
        assert(cons_time > 0)
        self.rand = rand
        self.count = count
        self.depth = depth
        self.prod_mean = float(prod_time) / count
        self.prod_std = math.sqrt(prod_var)
        self.cons_mean = float(cons_time) / count
        self.cons_std = math.sqrt(cons_var)
        self.reset()

    def _get_rand(self, mean, std):
        value = self.rand.gauss(mean, std)
        return max(1, int(value + 0.5))

    def _get_next_prod(self, t):
        return t + self._get_rand(self.prod_mean, self.prod_std)

    def _get_next_cons(self, t):
        return t + self._get_rand(self.cons_mean, self.cons_std)

    def reset(self):
        self.size = 0
        self.total = 0
        self.next_prod = self._get_next_prod(0)
        self.next_cons = self._get_next_cons(0)
        return min(self.next_prod, self.next_cons)

    def process(self, t):
        if t >= self.next_prod:
            if self.size < self.depth:
                self.size += 1
                self.next_prod = self._get_next_prod(t)
            else:
                self.next_prod = self.next_cons
        if t >= self.next_cons:
            if self.size > 0:
                self.size -= 1
                self.total += 1
                self.next_cons = self._get_next_cons(t)
            else:
                self.next_cons = self.next_prod
        if self.total >= self.count:
            return -1
        else:
            return min(self.next_prod, self.next_cons)


class Simulator(object):

    def __init__(self, seed=5):
        self.t = 0
        self.queues = []
        self.rand = Random(seed)

    def add_queue(self, count, depth,
                  prod_time, prod_var, cons_time, cons_var):
        q = Queue(self.rand, count, depth,
                  prod_time, prod_var, cons_time, cons_var)
        self.queues.append(q)

    def run(self):
        pq = PriorityQueue()
        for q in self.queues:
            t = q.reset()
            pq.push(t, q)
        while not pq.empty():
            t = max(t, pq.key())
            q = pq.value()
            pq.pop()
            next_t = q.process(t)
            if next_t >= 0:
                pq.push(next_t, q)
        return t

    def run_multiple(self,
                     max_cov=MAX_COV_DEFAULT,
                     max_iterations=MAX_ITER_DEFAULT):
        total = 0
        total2 = 0.0
        for i in xrange(max_iterations):
            t = self.run()
            total += t
            total2 += t * t
            if i > 0:
                n = i + 1
                mean = total / n
                var = total2 / n - mean * mean
                cov = math.sqrt(var) / mean
                if cov <= max_cov:
                    return mean, cov
        return mean, cov


def simulate(mod, ml, value, fstats):
    sim = Simulator()
    for fifo in ml.all_fifos():
        index = fifo.index
        depth = fifo.depth
        items, ptime, pvar, ctime, cvar = fstats.get_stats(index)
        sim.add_queue(items, depth, ptime, pvar, ctime, cvar)
    mean, cov = sim.run_multiple()
    print mean, cov
    return mean


if __name__ == '__main__':
    sim = Simulator()
    sim.add_queue(100, 1, 4000, 1, 2000, 1000)
    print sim.run_multiple(1e-4)
