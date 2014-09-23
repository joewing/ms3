import copy
from functools import reduce
from abc import ABCMeta, abstractmethod

from memsim import parser, cost


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

    def get_full_name(self):
        """Get the full name of this memory subsystem."""
        return self.get_name()

    def get_name(self):
        """Get the name of this memory subsystem."""
        return str(self)

    @abstractmethod
    def get_parameter_count(self):
        """Get the number of parameters."""
        return 0

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

    def get_bytes(self):
        """Get the number of bytes this component uses."""
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
        result = 1 + sum([m.count() for m in self.get_banks()])
        n = self.get_next()
        if n:
            result += n.count()
        return result

    @abstractmethod
    def get_cost(self):
        """Get the cost of this memory component (shallow)."""
        return self.machine.get_zero_cost()

    def get_total_cost(self):
        """Get the total cost of the memory component and its children."""
        costs = [m.get_total_cost() for m in self.get_banks()]
        result = reduce(lambda a, b: a + b, costs, self.get_cost())
        n = self.get_next()
        if n is not None:
            result += n.get_total_cost()
        return result

    def get_path_length(self, incoming):
        """Get the total path length before the next register."""
        return incoming

    def push_transform(self, index, rand):
        """Push any address transforms or limits for bank index.
            index=-1 is used for the next memory.
        """
        pass

    def pop_transform(self, rand):
        """Pop any address transforms or limits."""
        pass

    def permute(self, rand):
        """Permute a memory component.
            This function will permute the memory component.
            This returns True if the component was modified and False
            otherwise.
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


def parse_memory(lexer):
    return parser.parse(lexer, constructors)
