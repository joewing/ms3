
from __future__ import print_function
import random
from StringIO import StringIO

from memsim import lex, memory
from memsim.memory import cache
from memsim.memory import join
from memsim.memory import offset
from memsim.memory import prefetch
from memsim.memory import spm
from memsim.memory import shift
from memsim.memory import split
from memsim.memory import xor


class Optimizer(object):

    steps = 0
    threshold = 1024
    age = 0
    last = None
    last_value = 0
    current = None
    max_tries = 64

    constructors = [
        cache.random_cache,
        offset.random_offset,
        shift.random_shift,
        split.random_split,
        split.random_split,
        spm.random_spm,
        xor.random_xor
    ]

    def __init__(self, machine, ml, seed,
                 permute_only=False,
                 use_prefetch=False):
        self.current = ml
        self.machine = machine
        self.rand = random.Random(seed)
        self.permute_only = permute_only
        self.current.reset(machine)
        if use_prefetch:
            self.constructors.append(prefetch.random_prefetch)

    def load(self, db):
        """Load state from the database."""
        use_prefetch = db.get_value('use_prefetch', False)
        if use_prefetch and prefetch.random_prefetch not in self.constructors:
            self.constructors.append(prefetch.random_prefetch)

    def save(self, db):
        """Save the current state to a database."""
        use_prefetch = prefetch.random_prefetch in self.constructors
        db.set_value('use_prefetch', use_prefetch)
        for i in xrange(len(self.current.distributions)):
            dist = self.current.distributions[i]
            dist.save(db, i)
        db.save()

    def create_memory(self, dist, nxt, cost, in_bank):
        index = self.rand.randint(0, len(self.constructors) - 1)
        constructor = self.constructors[index]
        result = constructor(self.machine, nxt, dist, cost)
        if result is not None:
            result.reset(self.machine)
            return result
        else:
            return nxt

    def permute(self, dist, mem, index, max_cost):
        """Permute a specific memory component.
            Returns True if successful.
        """
        assert(index >= 0)
        if index == 0:
            mc = max_cost + mem.get_cost()
            return mem.permute(dist, mc)
        n = mem.get_next()
        nc = n.count()
        if index <= nc:
            mem.push_transform(-1, dist)
            result = self.permute(dist, n, index - 1, max_cost)
            mem.pop_transform(dist)
            return result
        t = nc + 1
        banks = mem.get_banks()
        for i in xrange(len(banks)):
            c = banks[i].count()
            if index < t + c:
                mem.push_transform(i, dist)
                result = self.permute(dist, banks[i], index - t, max_cost)
                mem.pop_transform(dist)
                return result
            t += c
        assert(False)

    def insert(self, dist, mem, index, max_cost):
        """Insert a memory component before index.
            Returns the updated memory.
        """
        assert(index >= 0)
        if index == 0:
            return self.create_memory(dist, mem, max_cost, False)
        n = mem.get_next()
        nc = n.count()
        if index <= nc:
            mem.push_transform(-1, dist)
            mem.set_next(self.insert(dist, n, index - 1, max_cost))
            mem.pop_transform(dist)
            return mem
        banks = mem.get_banks()
        t = nc + 1
        for i in xrange(len(banks)):
            c = banks[i].count()
            if index < t + c:
                mem.push_transform(i, dist)
                mem.set_bank(i, self.insert(dist, banks[i],
                                            index - t, max_cost))
                mem.pop_transform(dist)
                return mem
            t += c
        assert(False)

    def remove(self, dist, mem, index):
        """ Remove a memory component at index.
             Returns the updated memory.
        """
        assert(index >= 0)
        n = mem.get_next()
        if index == 0:
            if n is None:
                return mem
            for b in mem.get_banks():
                if not isinstance(b, join.Join):
                    return mem
            return n
        nc = n.count()
        if index <= nc:
            mem.push_transform(-1, dist)
            mem.set_next(self.remove(dist, n, index - 1))
            mem.pop_transform(dist)
            return mem
        t = nc + 1
        banks = mem.get_banks()
        for i in xrange(len(banks)):
            c = banks[i].count()
            if index < t + c:
                mem.push_transform(i, dist)
                updated = self.remove(dist, banks[i], index - t)
                mem.set_bank(i, updated)
                mem.pop_transform(dist)
                return mem
            t += c
        assert(False)

    def modify(self):
        """Modify the memory subsystem."""

        # Loop until we successfully modify the memory subsystem.
        max_cost = self.machine.max_cost - self.current.get_cost()
        stat = False
        while not stat:

            # Select a memory to modify.
            # Note that we do not attempt to modify a memory subsystem
            # unless it is actually used, which we determine using
            # Distribution.is_empty.
            while True:
                mindex = self.rand.randint(0, len(self.current) - 1)
                mem = self.current.memories[mindex]
                dist = self.current.distributions[mindex]
                if not dist.is_empty():
                    break
            count = mem.count()

            # Select an action to perform.
            action = self.rand.randint(0, 7)
            if action == 0:  # Insert
                for i in xrange(100):
                    before = str(mem)
                    index = self.rand.randint(0, count - 1)
                    temp = self.insert(dist, mem, index, max_cost)
                    stat = temp is not None and str(temp) != before
                    if stat:
                        self.current.memories[mindex] = temp
                        break
            elif action <= 2 and count > 1:  # Remove
                for i in xrange(100):
                    before = str(mem)
                    index = self.rand.randint(0, count - 1)
                    temp = self.remove(dist, mem, index)
                    stat = temp is not None and str(temp) != before
                    if stat:
                        self.current.memories[mindex] = temp
                        break
            else:   # Permute
                index = self.rand.randint(0, count - 1)
                stat = self.permute(dist, mem, index, max_cost)

    def load_best(self, db):
        best_name, _, _ = db.get_best()
        lexer = lex.Lexer(StringIO(best_name))
        distributions = self.current.distributions
        self.current = memory.parse_memory_list(lexer, distributions)
        self.current.reset(self.machine)

    def generate_next(self, db, time):
        """Generate the next memory to try."""
        tries = 1
        while True:
            self.steps += 1
            print('Step {} (threshold: {}, age: {}, try: {}/{})'
                  .format(self.steps, self.threshold, self.age,
                          tries, self.max_tries))
            if self.last is None:
                self.last = self.current.clone()
            else:
                denom = self.max_tries
                diff = time - self.last_value
                if diff <= self.threshold:
                    # Keep the current memory.
                    self.last_value = time
                    self.last = self.current.clone()
                    self.threshold -= (self.threshold + denom - 1) // denom
                    self.age = 0
                else:
                    # Revert to the last memory.
                    self.current = self.last.clone()
                    self.threshold += (self.age * self.threshold) // denom
                    self.age += 1
                before = self.current.clone()
                while True:
                    self.modify()
                    simplified = self.current.simplified()
                    ml = simplified.get_max_path_length()
                    if ml <= self.machine.max_path_length:
                        break
                    self.current = before.clone()
                time = db.get_result(str(simplified))
                if time is None:
                    return self.current
                else:
                    # If we get stuck, restart from the best.
                    tries += 1
                    if tries > self.max_tries:
                        self.max_tries += 1
                        self.threshold *= self.max_tries
                        tries = 0
                        self.load_best(db)

    def optimize(self, db, time):
        """This function is to be called after each evaluation.
            It returns the next memory list to evaluate, None when complete.
        """

        # Cache the simplified memory.
        simplified = self.current.simplified()
        db.add_result(str(simplified), time, simplified.get_cost())

        # Generate the next subsystem and display stats.
        result = self.generate_next(db, time)
        return result.simplified()
