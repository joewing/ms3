from memsim import lex, parser
from memsim.memory import base, main


class Option(main.MainMemory):

    def __init__(self, index):
        main.MainMemory.__init__(self)
        self.options = []
        self.index = index

    def get_parameter_count(self):
        return 1

    def can_remove(self):
        return False

    def can_insert(self):
        return True

    def get_word_size(self):
        return self.options[self.index].get_word_size()

    def generate(self, gen, source):
        return self.options[self.index].generate(gen, source)

    def __str__(self):
        return str(self.options[self.index])

    def get_full_name(self):
        result = '(option '
        result += '(index {})'.format(self.index)
        for index, mem in enumerate(self.options):
            result += '(memory{} {})'.format(index, mem)
        result += ')'
        return result

    def add_option(self, m):
        self.options.append(m)

    def get_cost(self):
        return self.options[self.index].get_cost()

    def reset(self, machine):
        main.MainMemory.reset(self, machine)
        self.options[self.index].reset(machine)

    def permute(self, rand):
        self.index = rand.randint(0, len(self.options) - 1)
        return True


def _create_option(lexer, args):
    index = parser.get_argument(lexer, args, 'index', 0)
    result = Option(index=index)
    i = 0
    while ('memory' + str(i)) in args:
        result.add_option(args['memory' + str(i)])
        i += 1
    if i == 0:
        raise lex.ParseError(lexer, "no memories in option")
    return result
base.constructors['option'] = _create_option
