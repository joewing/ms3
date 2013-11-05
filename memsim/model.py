
from memsim import benchmarks
from memsim import lex
from memsim import machine
from memsim import memory


class Model:

    def __init__(self):
        self.machine = machine.MachineType()
        self.memory = None
        self.benchmarks = []
        self.skip = 0
        self.on = 1000
        self.seed = 7

    def __str__(self):
        result = '(machine ' + str(self.machine) + ')'
        if self.skip > 0:
            result += '(skip ' + str(self.skip) + ')'
            result += '(on ' + str(self.on) + ')'
        result += '(seed ' + str(self.seed) + ')'
        result += '(memory ' + str(self.memory) + ')'
        result += '(benchmarks '
        for b in self.benchmarks:
            result += str(b)
        result += ')'
        return result


def parse_model(lexer, model=None):
    if model is None:
        model = Model()
    while lexer.get_type() != lex.TOKEN_EOF:
        lexer.match(lex.TOKEN_OPEN)
        name = lexer.get_value()
        lexer.match(lex.TOKEN_LITERAL)
        if name == 'machine':
            model.machine = machine.parse_machine(lexer)
        elif name == 'memory':
            model.memory = memory.parse_memory(lexer)
        elif name == 'benchmarks':
            model.benchmarks = _parse_benchmarks(lexer)
        elif name == 'include':
            value = lexer.get_value()
            lexer.match(lex.TOKEN_LITERAL)
            parse_model(lex.Lexer(open(value, 'r')), model)
        elif name == 'skip':
            model.skip = _parse_int(lexer)
        elif name == 'on':
            model.on = _parse_int(lexer)
        elif name == 'seed':
            model.seed = _parse_int(lexer)
        else:
            lex.ParseError(lexer, "invalid top-level component: " + name)
        lexer.match(lex.TOKEN_CLOSE)
    return model


def _parse_int(lexer):
    value = lexer.get_value()
    lexer.match(lex.TOKEN_LITERAL)
    return int(value)


def _parse_benchmarks(lexer):
    bms = []
    while lexer.get_type() == lex.TOKEN_OPEN:
        bms.append(benchmarks.parse_benchmark(lexer))
    return bms
