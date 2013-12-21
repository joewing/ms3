
from __future__ import print_function


class Optimizer(object):

    steps = 0
    threshold = 1
    last = None
    last_value = 0
    current = None
    max_tries = 1
    max_value = 0

    def __init__(self, current):
        self.current = current

    def load(self, db):
        """Load state from the database."""
        pass

    def save(self, db):
        """Save the current state to the database."""
        pass

    def restart(self, db):
        """Start a new chain."""
        pass

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
            It returns the next memory list to evaluate, None when complete.
        """

        # Store the current result.
        self.store_result(db, self.current, value)
        self.max_value = max(self.max_value, value)

        # Generate the next state.
        self.last = self.current if self.last is None else self.last
        tries = 1
        while True:
            self.steps += 1
            print('Step {} (threshold: {}, try: {}/{})'
                  .format(self.steps, self.threshold, tries, self.max_tries))
            denom = self.max_tries
            diff = value - self.last_value
            if diff <= self.threshold:
                # Keep the current memory.
                self.last_value = value
                self.last = self.current
                self.threshold -= (self.threshold + denom - 1) // denom
            else:
                # Revert to the last memory.
                self.current = self.last
                self.threshold += (self.threshold + denom - 1) // denom
            self.current = self.modify(self.current)
            value = self.load_result(db, self.current)
            if value is None:
                return self.current
            else:
                # If we get stuck, restart from the best.
                self.max_value = max(self.max_value, value)
                tries += 1
                if tries > self.max_tries:
                    self.max_tries *= 2
                    self.threshold = self.max_value
                    self.max_value = 0
                    tries = 0
                    self.restart(db)

        return self.current
