from __future__ import print_function
from collections import defaultdict
import random


class PendingException(Exception):
    pass


class Optimizer(object):

    def __init__(self, value):
        self.last_value = value
        self.threshold = 1 + value // 8
        self.delta = 1024

    def __str__(self):
        """Get a string to represent the current status."""
        return '{}'.format(self.threshold)

    def modify(self, current):
        """Modify the current state."""
        assert(False)

    def get_next(self, current):
        """Get the next state to evaluate."""
        return self.modify(current)

    def update(self, value):
        """Set the value from the last modification."""
        diff = value - self.last_value
        denom = self.delta
        if diff <= self.threshold:
            # Accept this state.
            self.last_value = value
            self.threshold -= (self.threshold + denom - 1) // denom
            self.threshold = max(1, self.threshold)
            self.delta += 1
            return True
        else:
            # Reject this state.
            self.threshold += (self.threshold + denom - 1) // denom
            self.delta -= 1
            if random.randint(0, self.delta) == 0:
                raise PendingException()
            return False
