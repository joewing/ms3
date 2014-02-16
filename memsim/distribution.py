from abc import ABCMeta, abstractmethod
import random


class Distribution(random.Random):
    __metaclass__ = ABCMeta

    # Minimum range size in bytes.
    min_size = 1024

    def __init__(self, seed):
        random.Random.__init__(self, seed)
        self.start_seed = seed

        # Stack of address range limits of the form:
        # (False, lower limit, upper limit) and address transforms, which
        # are functions from address to # transformed address.
        # Transform entries are pairs: (True, function).
        self.limits = []

    @abstractmethod
    def load(self, state, index):
        """Load the state of this distribution object."""
        pass

    @abstractmethod
    def save(self, state, index):
        """Save the state of this distribution object."""
        pass

    def reset(self):
        """Reset the random number generator using the original seed."""
        self.seed(self.start_seed)

    @abstractmethod
    def is_empty(self):
        """Determine if there are any address ranges in this distribution."""
        pass

    @abstractmethod
    def insert_range(self, addr, size):
        """Insert a new range into the set of address ranges."""
        pass

    def push_limit(self, lower, upper):
        """Push an address limit on to the stack.
            This is done when evaluating a part of a split or scratchpad
            to limit the addresses that are selected.
        """
        self.limits.append((False, lower, upper))

    def pop_limit(self):
        """Pop an address limit off of the stack."""
        self.limits.pop()

    def push_transform(self, transform):
        """Push an address transform on to the stack."""
        self.limits.append((True, transform))

    def pop_transform(self):
        """Pop an address transform off of the stack."""
        self.limits.pop()

    @abstractmethod
    def random_address(self, alignment):
        """Get a random address with the specified alignment."""
        pass

    def randbool(self):
        """Generate a random boolean value."""
        return self.randint(0, 1) == 1
