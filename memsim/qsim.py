import math
from random import Random
from subprocess import Popen, PIPE
import json

import cost
from priorityqueue import PriorityQueue


EPSILON = 1e-4
BRAM_BYTES = (512 * 36) // 8


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
        if prod_time > 0 and cons_time > 0:
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

    def run_multiple(self, confidence=0.95):
        n, mean, m2 = 0, 0, 0
        while True:
            t = self.run()
            n += 1
            delta = t - mean
            mean += delta / n
            m2 += delta * (t - mean)
            if n > 2:
                var = m2 / (n - 1)
                std = math.sqrt(var)
                interval = confidence * std / math.sqrt(n)
                if interval / mean < EPSILON:
                    break
        print mean, n
        return mean


def simulate(mod, ml, value, fstats):

    # Determine the maximum number of items used.
    value = 0
    for fifo in ml.all_fifos():
        index = fifo.index
        items, _, _, _, _ = fstats.get_stats(index)
        value = max(value, items)

    # Scale the number of items to process.
    max_value = 1000
    scale = value // max_value if value > max_value else 1

    # Add the FIFOs to the simulation.
    sim = Simulator()
    for fifo in ml.all_fifos():
        index = fifo.index
        depth = fifo.depth
        items, ptime, pvar, ctime, cvar = fstats.get_stats(index)
        items = (items + scale - 1) // scale
        sim.add_queue(items, depth, ptime, pvar, ctime, cvar)

    # Run the simulation.
    return sim.run_multiple()


def increase_size(mod, ml, value, fstats, bytes_left):

    # Attempt to increase the size of each queue and increase
    # the size of the queue that improves the performance the most.
    best_value = simulate(mod, ml, value, fstats)
    best_fifo = None
    for fifo in ml.all_fifos():
        increment = BRAM_BYTES // fifo.get_word_size()
        assert(increment > 0)
        bytes_left -= BRAM_BYTES
        increment = (increment - 1) if fifo.depth == 1 else increment
        fifo.depth += increment
        temp = simulate(mod, ml, value, fstats)
        fifo.depth -= increment
        if (temp - best_value) / temp < -EPSILON:
            best_value = temp
            best_fifo = fifo
    if best_fifo is not None:
        increment = BRAM_BYTES // best_fifo.get_word_size()
        increment = (increment - 1) if best_fifo.depth == 1 else increment
        best_fifo.depth += increment
        return True
    else:
        # Unable to improve the performance.
        return False


def get_score(mod, ml, value, fstats):

    # Reset the sizes of all FIFOs.
    for fifo in ml.all_fifos():
        fifo.depth = 1

    # Determine how many BRAMs we can give to FIFOs.
    remaining = mod.machine.get_max_cost() - ml.get_cost(mod.machine)

    sim_data = {}
    sim_data['bram_count'] = remaining.cost
    fifo_data = []
    for fifo in ml.all_fifos():
        item = {}
        item['depth'] = fifo.depth
        count, ptime, pvar, ctime, cvar = fstats.get_stats(fifo.index)
        item['count'] = count
        item['ptime'] = ptime
        item['pvar'] = pvar
        item['ctime'] = ctime
        item['cvar'] = cvar
        fifo_data.append(item)
    sim_data['queues'] = fifo_data

    args = ['qsim/qsim']
    p = Popen(args, stdin=PIPE, stdout=PIPE)
    result, _ = p.communicate(input=json.dumps(sim_data))
    result = json.loads(result)

    t = result['total']
    for fifo, d in zip(ml.all_fifos(), result['depths']):
        fifo.depth = d
    print t
    return t

if __name__ == '__main__':
    sim = Simulator()
    sim.add_queue(100, 1, 4000, 1, 2000, 1000)
    print sim.run_multiple(1e-4)
