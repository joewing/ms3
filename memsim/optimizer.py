from __future__ import print_function
import random


class Optimizer(object):

    steps = 0
    threshold = 1
    last = None
    last_value = 0
    current = None
    max_tries = 1

    def __init__(self, current):
        self.current = current

    def __str__(self):
        """Get a string to represent the current status."""
        return '{}'.format(self.threshold)

    def load(self, db):
        """Load state from the database."""
        pass

    def restart(self, db):
        """Start a new chain."""
        return self.current, 1

    def store_result(self, db, current, value):
        """Store a result."""
        pass

    def load_result(self, db, current):
        """Load a stored result."""
        return None

    def modify(self, current):
        """Modify the current state."""
        assert(False)

    def optimize(self, db, value):
        """This function is to be called after each evaluation.
            It returns the next memory list to evaluate or None if
            this is a duplicate chain.
        """

        # Store the current result.
        self.store_result(db, self.current, value)

        # Generate the next state.
        self.last = self.current if self.last is None else self.last
        tries = 1
        while True:
            self.steps += 1
            denom = self.max_tries
            diff = value - self.last_value
            if diff <= self.threshold:
                # Keep the current state.
                self.last_value = value
                self.last = self.current
                self.threshold -= (self.threshold + denom - 1) // denom
            else:
                # Revert to the last state.
                self.current = self.last
                self.threshold += (self.threshold + denom - 1) // denom
            self.current = self.modify(self.current)
            value = self.load_result(db, self.current)
            if value is None:
                # Current state needs to be evaluated.
                return self.current
            elif value < 0:
                # Current state is pending evaluation.
                return None
            else:
                # Current state has already been evaulated.
                # Probabilistically restart from the best.
                tries += 1
                if random.randint(0, 1) == 0:
                    self.max_tries += 1
                    tries = 0
                    self.current, self.threshold = self.restart(db)
