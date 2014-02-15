import copy

from memsim import lex
from memsim.memory.base import parse_memory


class MemoryList(object):

    def __init__(self, main_memory):
        self.main_memory = main_memory
        self.memories = []
        self.fifos = []

    def __len__(self):
        return len(self.memories)

    def __str__(self):
        result = '(main ' + str(self.main_memory) + ')'
        for m in self.memories:
            result += ' ' + m.get_name()
        for size, fifo in self.fifos:
            result += ' (fifo (size ' + size + ')'
            result += fifo.get_name() + ')'
        return result

    def clone(self):
        return copy.deepcopy(self)

    def add_memory(self, mem=None):
        mem = mem.set_main(self.main_memory) if mem else self.main_memory
        self.memories.append(mem)

    def add_fifo(self, mem=None):
        mem = mem.set_main(self.main_memory) if mem else self.main_memory
        self.fifos.append((1, mem))

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
        for i in xrange(len(new.memories)):
            new.memories[i] = new.memories[i].simplify()
        return new


def parse_memory_list(lexer):
    lexer.match(lex.TOKEN_OPEN)
    value = lexer.get_value()
    lexer.match(lex.TOKEN_LITERAL)
    if value != 'main':
        raise lex.ParseError(lexer, "expected 'main' got '" + value + "'")
    main = parse_memory(lexer)
    lexer.match(lex.TOKEN_CLOSE)
    ml = MemoryList(main)
    while lexer.get_type() == lex.TOKEN_OPEN:
        ml.add_memory(parse_memory(lexer))
    return ml
