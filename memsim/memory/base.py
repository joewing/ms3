
import copy
from functools import reduce

from memsim import lex
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

    def __init__(self):
        global next_memory_id
        self.machine = None
        self.memory_id = next_memory_id
        next_memory_id += 1

    def generate(self, gen, mach):
        """Generate the HDL model for this memory."""
        pass

    def clone(self):
        """Create a deep copy of this memory."""
        return copy.deepcopy(self)

    def get_id(self, prefix='m'):
        """Get this memory's identifier."""
        return prefix + str(self.memory_id)

    def get_ports(self, mach):
        """Get a list of main memory ports for this subsystem."""
        n = self.get_next()
        if n is not None:
            return n.get_ports(mach)
        else:
            return []

    def get_next(self):
        """Get the next memory component."""
        return None

    def get_banks(self):
        """Get memory subcomponents."""
        return []

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
        if n is not None:
            result += n.count()
        return result

    def get_cost(self):
        """Get the cost of this memory component (shallow)."""
        return 0

    def get_total_cost(self):
        """Get the total cost of the memory component and its children."""
        costs = map(lambda m: m.get_total_cost(), self.get_banks())
        result = reduce(lambda a, b: a + b, costs, self.get_cost())
        if self.get_next() is not None:
            result += self.get_next().get_total_cost()
        return result

    def get_path_length(self):
        """Get the total path length before the next register."""
        return 0

    def simplify(self):
        """Return a simplified memory subsystem."""
        banks = self.get_banks()
        for i in range(len(banks)):
            self.set_bank(i, banks[i].simplify())
        n = self.get_next()
        if n is not None:
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

    def permute(self, rand, max_cost):
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
        if self.get_next() is not None:
            self.get_next().reset(machine)

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


class MemoryList(object):

    memories = []

    def __init__(self, memories, distributions):
        self.memories = memories
        self.distributions = distributions

    def __len__(self):
        return len(self.memories)

    def __str__(self):
        if len(self.memories) > 0:
            names = map(str, self.memories)
            return reduce(lambda a, b: a + ":" + b, names)
        else:
            return ""

    def clone(self):
        return MemoryList(copy.deepcopy(self.memories), self.distributions)

    def get_cost(self):
        costs = map(lambda m: m.get_total_cost(), self.memories)
        return reduce(lambda x, y: x + y, costs, 0)

    def get_max_path_length(self):
        return max(map(lambda m: m.get_path_length(), self.memories))

    def reset(self, machine):
        for m in self.memories:
            m.reset(machine)

    def simplified(self):
        """Return a simplified version of this memory list.
            This does not mutate the original memory list.
        """
        new = self.clone()
        for i in range(len(new.memories)):
            new.memories[i] = new.memories[i].simplify()
        return new


def parse_memory(lexer):
    return parser.parse(lexer, constructors)


def parse_memory_list(lexer, dists):
    ms = []
    ms.append(parse_memory(lexer))
    while lexer.get_type() == lex.TOKEN_COLON:
        lexer.match(lex.TOKEN_COLON)
        ms.append(parse_memory(lexer))
    return MemoryList(ms, dists)
