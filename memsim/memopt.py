from __future__ import print_function
import random
from StringIO import StringIO
import sys

from optimizer import Optimizer
from memsim import lex, memory
from memsim.memory import (
    cache,
    offset,
    prefetch,
    spm,
    shift,
    split,
    xor
)


class MemoryOptimizer(Optimizer):

    constructors = [
        cache.random_cache,
        offset.random_offset,
        shift.random_shift,
        split.random_split,
        split.random_split,
        spm.random_spm,
        xor.random_xor
    ]

    def __init__(self, mod, ml, seed, dist, use_prefetch=False):
        Optimizer.__init__(self, ml)
        self.rand = random.Random(seed)
        self.model = mod
        self.dist = dist
        ml.reset(mod.machine)
        if use_prefetch:
            self.constructors.append(prefetch.random_prefetch)

    def store_result(self, db, current, value):
        """Store a result."""
        simplified = current.simplified()
        db.add_result(self.model, str(simplified), value,
                      simplified.get_cost())

    def load_result(self, db, current):
        """Load a result.
        Returns None if the state has not yet been evaluated.
        """
        return db.get_result(self.model, str(current.simplified()))

    def create_memory(self, dist, nxt, cost, in_bank):
        index = self.rand.randint(0, len(self.constructors) - 1)
        constructor = self.constructors[index]
        result = constructor(self.model.machine, nxt, dist, cost)
        if result is not None:
            result.reset(self.model.machine)
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
            if mem.can_insert():
                return self.create_memory(dist, mem, max_cost, False)
            else:
                return mem
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
            if n is not None and mem.can_remove():
                return n
            else:
                return mem
        nc = n.count()
        if index <= nc:
            mem.push_transform(-1, dist)
            mem.set_next(self.remove(dist, n, index - 1))
            mem.pop_transform(dist)
            return mem
        t = nc + 1
        banks = mem.get_banks()
        for i, bank, in enumerate(banks):
            c = bank.count()
            if index < t + c:
                mem.push_transform(i, dist)
                updated = self.remove(dist, bank, index - t)
                mem.set_bank(i, updated)
                mem.pop_transform(dist)
                return mem
            t += c
        assert(False)

    def modify(self, last):
        """Modify the memory subsystem."""

        # Loop until we successfully modify the memory subsystem.
        max_path = self.model.machine.max_path_length
        max_cost = self.model.machine.max_cost - last.get_cost()
        while True:

            # Select an action to perform.  We make multiple
            # attempts to use the selected action.
            action = self.rand.randint(0, 7)
            for i in xrange(10):

                # Select a memory to modify.
                # Note that we do not attempt to modify a memory subsystem
                # unless it is actually used, which we determine using
                # Distribution.is_empty.
                current = last.clone()
                while True:
                    mindex = current.choice(self.rand)
                    mem = current.get(mindex)
                    dist = self.dist.get_distribution(mem)
                    if not dist.is_empty():
                        break
                count = mem.count()

                # Modify the memory.
                if action == 0:  # Insert
                    before = str(mem)
                    index = self.rand.randint(0, count - 1)
                    temp = self.insert(dist, mem, index, max_cost)
                    if temp is not None and str(temp) != before:
                        current.set(mindex, temp)
                        if current.get_max_path_length() <= max_path:
                            return current
                elif action <= 2 and count > 1:  # Remove
                    before = str(mem)
                    index = self.rand.randint(0, count - 1)
                    temp = self.remove(dist, mem, index)
                    if temp is not None and str(temp) != before:
                        current.set(mindex, temp)
                        if current.get_max_path_length() <= max_path:
                            return current
                else:   # Permute
                    index = self.rand.randint(0, count - 1)
                    if self.permute(dist, mem, index, max_cost):
                        if current.get_max_path_length() <= max_path:
                            return current

    def restart(self, db):
        best_name, best_value, _ = db.get_best(self.model)
        lexer = lex.Lexer(StringIO(best_name))
        current = memory.parse_memory_list(lexer)
        current.reset(self.model.machine)
        return current, best_value
