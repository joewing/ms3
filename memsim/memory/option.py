
from memsim import lex
from memsim.memory import base


class Option(base.Memory):

    def __init__(self):
        self.options = []
        self.index = 0

    def __str__(self):
        return str(self.options[self.index])

    def can_remove(self):
        return False

    def can_insert(self):
        return True

    def generate(self, gen, mach):
        self.options[self.index].generate(gen, mach)

    def get_name(self):
        return self.options[self.index].get_name()

    def add_option(self, m):
        self.options.append(m)

    def get_cost(self):
        return self.options[self.index].get_cost()

    def reset(self, machine):
        base.Memory.reset(self, machine)
        self.options[self.index].reset(machine)

    def permute(self, rand, max_cost):
        self.index = rand.randint(0, len(self.options) - 1)
        return True

    def process(self, start, write, addr, size):
        return self.options[self.index].process(start, write, addr, size)

    def done(self):
        return self.options[self.index].done()


def _create_option(lexer, args):
    result = Option()
    i = 0
    while ('memory' + str(i)) in args:
        result.add_option(args['memory' + str(i)])
        i += 1
    if i == 0:
        raise lex.ParseError(lexer, "no memories in option")
    return result
base.constructors['option'] = _create_option
