from __future__ import print_function
from collections import defaultdict
import random


class PendingException(Exception):
    pass


class Optimizer(object):

    def __init__(self, current):
        self.current = current
        self.current_index = 0
        self.last = None
        self.last_value = None
        self.subsystem = -1
        self.threshold = 1024
        self.delta = 1024

    def __str__(self):
        """Get a string to represent the current status."""
        return '{}'.format(self.threshold)

    def load(self, db):
        """Load state from the database."""
        pass

    def restart(self, db):
        """Start a new chain."""
        return self.current, 1

    def store_result(self, db, current, subsystem, value):
        """Store a result."""
        pass

    def load_result(self, db, current, subsystem):
        """Load a stored result."""
        return None

    def _load_results(self, db, current):
        results = dict()
        for subsystem in self.last_value.iterkeys():
            temp = self.load_result(db, current, subsystem)
            if temp is None:
                return None
            elif temp < 0:
                raise PendingException()
            results[subsystem] = temp
        return results

    def modify(self, current):
        """Modify the current state."""
        assert(False)

    def _get_difference(self, value):
        """Get the difference between `value` and the last value."""
        # Note: we are using 'sum' as the aggregation function.
        total = 0
        if self.last_value is not None:
            for k, v in value.iteritems():
                total += v - self.last_value[k]
        return total

    def optimize(self, db, value):
        """This function is to be called after each evaluation.
            It returns the next memory list to evaluate or None if
            this is a duplicate chain.
        """

        # Store the current result.
        if self.subsystem >= 0:
            self.store_result(db, self.current, self.subsystem, value)

        # Generate the next state.
        self.last = self.current if self.last is None else self.last
        while True:
            denom = self.delta
            diff = self._get_difference(value)
            if diff <= self.threshold:
                # Keep the current state.
                self.last_value = value
                self.last = self.current
                self.threshold -= (self.threshold + denom - 1) // denom
                self.threshold = max(1, self.threshold)
                self.delta += 1
            elif random.randint(0, 15) == 0:
                # Restart from the best state.
                self.current, _ = self.restart(db)
                self.last = self.current
                self.threshold = diff
            else:
                # Revert to the last state.
                self.current = self.last
                self.threshold += (self.threshold + denom - 1) // denom
                self.delta = max(1, self.delta - 1)
            self.current, self.subsystem = self.modify(self.current)
            value = self._load_results(db, self.current)
            if value is None:
                # Current state needs to be evaluated.
                return self.current, self.subsystem
            else:
                # Current state has already been evaulated.
                # Probabilistically restart from the best.
                if random.randint(0, 15) == 0:
                    self.current, _ = self.restart(db)
                    self.last = self.current
                    self.threshold = max(1, abs(diff))
