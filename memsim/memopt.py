from __future__ import print_function
import random
from StringIO import StringIO

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
        #cache.random_cache,
        #offset.random_offset,
        #prefetch.random_prefetch,
        #shift.random_shift,
        #split.random_split,
        #split.random_split,
        spm.random_spm,
        #xor.random_xor,
    ]

    def __init__(self, mod, value, seed, dist, directory):
        Optimizer.__init__(self, value)
        self.rand = random.Random(seed)
        self.model = mod
        self.dist = dist
        self.directory = directory

    def create_memory(self, dist, nxt, cost, in_bank):

        # Attempt to create the new subsystem component.
        constructor = self.rand.choice(self.constructors)
        result = constructor(self.model.machine, nxt, dist, cost)
        if result is None:
            return nxt

        # A new component was created.
        # If the new subsystem has banks, select parts of the
        # existing subsystem to go into the banks.
        bank_count = len(result.get_banks())
        if bank_count == 0:
            result.reset(self.model.machine)
            return result

        # Determine the extent of the existing subsystem that can
        # go into the bank(s).
        following = []
        last = nxt
        while last.can_remove():
            following.append(last)
            last = last.get_next()

        # Divide the components into banks.
        last_following = len(following) - 1
        start_index = 0
        for b in range(bank_count):
            if start_index >= last_following:
                break
            end_index = self.rand.randint(start_index, last_following)
            if end_index > start_index:
                following[end_index].set_next(result.get_bank(b))
                result.set_bank(b, following[start_index])
            start_index = end_index + 1
        if start_index < len(following):
            result.set_next(following[start_index])
        else:
            result.set_next(last)

        # Reset and return the new component.
        result.reset(self.model.machine)
        return result

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

        # Check if this is the component to remove.
        n = mem.get_next()
        if index == 0:
            if n is not None and mem.can_remove():
                return n
            else:
                return mem

        # Check subcomponents.
        index -= 1
        banks = mem.get_banks()
        for i, bank, in enumerate(banks):
            c = bank.count()
            if index < c:   # In this bank.
                mem.push_transform(i, dist)
                updated = self.remove(dist, bank, index)
                mem.set_bank(i, updated)
                mem.pop_transform(dist)
                return mem
            index -= c

        # In the next component.
        if n is not None:
            mem.push_transform(-1, dist)
            mem.set_next(self.remove(dist, n, index))
            mem.pop_transform(dist)
        return mem

    def modify(self, last):
        """Modify the memory subsystem."""

        # Loop until we successfully modify the memory subsystem.
        max_path = self.model.machine.max_path_length
        mach_cost = self.model.machine.get_max_cost()
        max_cost = mach_cost - last.get_cost()
        for f in last.all_fifos():
            max_cost.size -= f.total_size()
            max_cost.size -= f.get_word_size()
        for b in self.model.benchmarks:
            m = last.get_subsystem(b.index)
            max_cost.size -= m.get_word_size()
            max_cost.size -= b.get_size(self.directory)
        while True:

            # Select a memory to modify.
            # Note that we do not attempt to modify a memory subsystem
            # unless it is actually used, which we determine using
            # Distribution.is_empty.
            current = last.clone()
            while True:
                mem = current.choice(self.rand)
                dist = self.dist.get_distribution(mem)
                if not dist.is_empty():
                    break
            count = mem.count()
            parameter_count = mem.get_parameter_count() * 8
            subsystem = mem.index

            # Select an action to perform.
            # We make multiple attempts for the selected action
            # and memory subsystem before trying something new.
            action = self.rand.randint(0, 1 + parameter_count + count)
            for i in xrange(count):

                # Modify the memory.
                updated = False
                if action == 0:             # Insert
                    before = str(mem)
                    index = self.rand.randint(0, count - 1)
                    temp = self.insert(dist, mem, index, max_cost)
                    if temp is not None and str(temp) != before:
                        current.update(temp)
                        updated = True
                elif action <= count:       # Remove
                    before = str(mem)
                    index = self.rand.randint(0, count - 1)
                    temp = self.remove(dist, mem, index)
                    if temp is not None and str(temp) != before:
                        current.update(temp)
                        updated = True
                else:                       # Permute
                    index = self.rand.randint(0, count - 1)
                    updated = self.permute(dist, mem, index, max_cost)
                if updated:
                    if not current.get_cost().fits(mach_cost):
                        current = last.clone()
                        continue
                    if current.get_max_path_length() > max_path:
                        current = last.clone()
                        continue
                    return current, subsystem
