import copy
from functools import reduce
from abc import ABCMeta, abstractmethod

from memsim import parser


# Constructors used by the parser to construct memories.
constructors = dict()


# Unique ID assigned to memories.
next_memory_id = 1


class MemoryPort(object):
    def __init__(self, name, word_size, addr_width):
        self.name = name
        self.word_size = word_size
        self.addr_width = addr_width


class Memory(object):
    """The abstract base class for all memory components."""
    __metaclass__ = ABCMeta

    def __init__(self):
        global next_memory_id
        self.machine = None
        self.memory_id = next_memory_id
        next_memory_id += 1

    def get_name(self):
        """Get the name of this memory subsystem."""
        return str(self)

    def get_parameter_count(self):
        """Get the number of parameters."""
        return 0

    def get_size(self):
        """Get the size of this memory subsystem."""
        return 1

    @abstractmethod
    def can_remove(self):
        """Determine if this memory component can be removed."""
        return False

    @abstractmethod
    def can_insert(self):
        """Determine if we can insert before this component."""
        return False

    @abstractmethod
    def generate(self, gen, source):
        """Generate the HDL model for this memory.

        gen is the HDL generator.
        source is the upstream memory component.

        Returns the base name for the memory port."""
        return ''

    @abstractmethod
    def get_word_size(self):
        """Get the word size for this memory component."""
        return 0

    def clone(self):
        """Create a deep copy of this memory."""
        return copy.deepcopy(self)

    def get_main(self):
        """Return the main memory."""
        n = self.get_next()
        if n is not None:
            return n.get_main()
        else:
            return None

    def set_main(self, mem):
        """Set the main memory.
        Returns the updated memory subsystem.
        """
        n = self.get_next()
        if n is not None:
            self.set_next(n.set_main(mem))
        else:
            self.set_next(mem)
        return self

    def get_id(self):
        """Get this memory's identifier."""
        return 'm' + str(self.memory_id)

    def get_next(self):
        """Get the next memory component."""
        return None

    def get_banks(self):
        """Get memory subcomponents."""
        return []

    def get_bank(self, i):
        """Return the specified memory bank."""
        banks = self.get_banks()
        return banks[i]

    def set_next(self, n):
        """Set the next memory."""
        assert(False)

    def set_bank(self, i, c):
        """Update a memory sub-component."""
        assert(False)

    def count(self):
        """Count the total number of components that make up this memory."""
        counts = map(lambda m: m.count(), self.get_banks())
        result = reduce(lambda a, b: a + b, counts, 1)
        n = self.get_next()
        if n:
            result += n.count()
        return result

    def get_cost(self):
        """Get the cost of this memory component (shallow)."""
        return 0

    def get_total_cost(self):
        """Get the total cost of the memory component and its children."""
        costs = map(lambda m: m.get_total_cost(), self.get_banks())
        result = reduce(lambda a, b: a + b, costs, self.get_cost())
        if self.get_next():
            result += self.get_next().get_total_cost()
        return result

    def get_path_length(self, incoming):
        """Get the total path length before the next register."""
        return incoming

    def simplify(self):
        """Return a simplified memory subsystem."""
        banks = self.get_banks()
        for i in xrange(len(banks)):
            self.set_bank(i, banks[i].simplify())
        n = self.get_next()
        if n:
            self.set_next(n.simplify())
        return self

    def push_transform(self, index, rand):
        """Push any address transforms or limits for bank index.
            index=-1 is used for the next memory.
        """
        pass

    def pop_transform(self, rand):
        """Pop any address transforms or limits."""
        pass

    def permute(self, rand, max_cost, max_size):
        """Permute a memory component.
            This function will permute the memory component without
            exceeded max_cost for that component.  This returns
            True if the component was modified and False otherwise.
        """
        return False

    def reset(self, machine):
        """Reset the memory for a new run.
            This is called before every trace execution.
        """
        self.machine = machine
        for b in self.get_banks():
            b.reset(machine)
        if self.get_next():
            self.get_next().reset(machine)

    @abstractmethod
    def process(self, start, write, addr, size):
        """Process a memory access operation.
            This function will return the number of cycles until the
            access completes.
            The start parameter is the relative start time of the access.
            That is, adding start to self.machine.time will give the
            absolute time of the access.
            write is True for writes and False for reads.
            addr is the byte address of the access.
            size is the size of the access in bytes.
        """
        return start

    def done(self):
        """Finish a trace execution.
            This will return the number of cycles to add to the
            result for the memory subsystem.
        """
        return 0


def send_request(mem, start, write, addr, size):
    """Send a memory request to the specified memory subsystem."""
    assert(size > 0)
    word_size = mem.get_word_size()
    word_mask = word_size - 1
    addr_mask = mem.machine.addr_mask
    offset = addr & word_mask
    if offset:
        first_size = min(word_size - offset, size)
        start = mem.process(start, write, addr, first_size)
        addr = (addr + first_size) & addr_mask
        size -= first_size
    while size:
        temp_size = min(word_size, size)
        start = mem.process(start, write, addr, temp_size)
        addr = (addr + temp_size) & addr_mask
        size -= temp_size
    return start


def parse_memory(lexer):
    return parser.parse(lexer, constructors)
