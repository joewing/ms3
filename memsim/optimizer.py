from __future__ import print_function
from collections import defaultdict
import random


class PendingException(Exception):
    pass


class Optimizer(object):

    def __init__(self, current):
        self.current = current
        self.last = None
        self.last_value = 0
        self.threshold = 1024
        self.delta = 8

    def __str__(self):
        """Get a string to represent the current status."""
        return '{}'.format(self.threshold)

    def modify(self, current):
        """Modify the current state."""
        assert(False)

    def optimize(self, db, value):
        """This function is to be called after each evaluation.
           It returns the next memory list to evaluate.
        """

        # If we don't have a last state, just save the current
        # state as the last state and generate a new subsystem.
        if self.last is None:
            # We don't have a last state.
            self.last = self.current
            self.last_value = value
            self.current, subsystem = self.modify(self.current)
            return self.current, subsystem

        # Determine if we should keep the current state or
        # revert to the last state.
        diff = value - self.last_value
        denom = self.delta
        if diff <= self.threshold:
            # Keep the current state.
            self.last = self.current
            self.last_value = value
            self.threshold -= (self.threshold + denom - 1) // denom
            self.threshold = max(1, self.threshold)
            self.delta += 1
        elif random.randint(0, 31) == 0:
            # Randomly restart from the best.
            raise PendingException()
        else:
            # Revert to the last state.
            self.current = self.last
            self.threshold += (self.threshold + denom - 1) // denom
            self.delta = max(1, self.delta - 1)

        # Generate a new subsystem to try.
        self.current, subsystem = self.modify(self.current)
        return self.current, subsystem
