
from abc import ABCMeta, abstractmethod
import hashlib


class BaseDatabase(object):
    """Database functionality used for saving state."""
    __metaclass__ = ABCMeta

    def __init__(self):
        """Initialize."""
        pass

    @abstractmethod
    def load(self, mod):
        """Load state."""
        return None

    @abstractmethod
    def save(self, mod, state):
        """Save state."""
        pass

    @abstractmethod
    def get_result(self, mod, mem):
        """Load a cached result."""
        return None

    @abstractmethod
    def add_result(self, mod, mem, value, cost):
        """Insert a result to the database."""
        return True

    @abstractmethod
    def get_best(self, mod):
        """Get the best for the specified model."""
        return None, 0, 0

    @abstractmethod
    def get_result_count(self, mod):
        """Get the number of results for specified model."""
        return 0

    @abstractmethod
    def get_fpga_result(self, name):
        """Load FPGA timing data."""
        return None

    @abstractmethod
    def add_fpga_result(self, key, frequency, bram_count):
        """Save FPGA timing data."""
        return True

    @abstractmethod
    def get_cacti_result(self, key):
        """Load CACTI timing data."""
        return None

    @abstractmethod
    def add_cacti_result(self, key, access_time, cycle_time, area):
        """Save CACTI timing data."""
        return True

    def get_hash(self, value):
        """Get the hash for a value."""
        return hashlib.sha1(str(value)).hexdigest()
