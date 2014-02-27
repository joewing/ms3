import copy

from memsim import lex
from memsim.memory.base import parse_memory
from memsim.memory.main import MainMemory
from memsim.memory.fifo import FIFO
from memsim.memory.subsystem import Subsystem


class MemoryList(object):

    def __init__(self, main_memory):
        self.main_memory = main_memory
        self.subsystems = []    # Mapping of id -> subsystem
        self.fifos = []         # Mapping of id -> fifo

    def __str__(self):
        result = '(main (memory ' + str(self.main_memory) + '))'
        for m in self.all_memories():
            result += ' ' + m.get_name()
        return result

    def _insert(self, lst, index, value):
        if index >= len(lst):
            lst.extend([None for _ in range(index - len(lst) + 1)])
        assert(lst[index] is None)
        lst[index] = value

    def choice(self, rand):
        return rand.choice(list(self.all_memories()))

    def update(self, mem):
        index = mem.index
        if isinstance(mem, FIFO):
            self.fifos[index] = mem
        elif isinstance(mem, Subsystem):
            self.subsystems[index] = mem
        else:
            assert(False)

    def get_subsystem(self, index):
        return self.subsystems[index]

    def get_fifo(self, index):
        return self.fifos[index]

    def all_memories(self):
        for m in self.all_subsystems():
            yield m
        for m in self.all_fifos():
            yield m

    def all_fifos(self):
        return [f for f in self.fifos if f is not None]

    def all_subsystems(self):
        return [s for s in self.subsystems if s is not None]

    def clone(self):
        return copy.deepcopy(self)

    def add_memory(self, mem):
        mem = mem.set_main(self.main_memory)
        index = mem.index
        if isinstance(mem, FIFO):
            self._insert(self.fifos, index, mem)
        elif isinstance(mem, Subsystem):
            self._insert(self.subsystems, index, mem)
        else:
            assert(False)

    def get_cost(self):
        return sum([m.get_total_cost() for m in self.all_memories()])

    def get_max_path_length(self):
        return max([m.get_path_length() for m in self.all_memories()])

    def reset(self, machine):
        for m in self.all_memories():
            m.reset(machine)

    def simplified(self):
        """Return a simplified version of this memory list.
            This does not mutate the original memory list.
        """
        new = self.clone()
        for s in self.all_subsystems():
            new.subsystems[s.index] = s.simplify()
        for f in self.all_fifos():
            new.fifos[f.index] = f.simplify()
        return new


def parse_memory_list(lexer):
    main = None
    memories = []
    while lexer.get_type() == lex.TOKEN_OPEN:
        mem = parse_memory(lexer)
        if isinstance(mem, MainMemory):
            main = mem
        elif isinstance(mem, FIFO) or isinstance(mem, Subsystem):
            memories.append(mem)
        else:
            raise lex.ParseError(lexer, 'invalid top-level memory')
    ml = MemoryList(main)
    for m in memories:
        ml.add_memory(m)
    return ml
