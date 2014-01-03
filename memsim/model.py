
from memsim import benchmarks, lex, machine, memory


class Model(object):

    def __init__(self):
        self.machine = machine.MachineType()
        self.memory = None
        self.benchmarks = []
        self.fifos = []         # FIFO sizes.

    def __str__(self):
        result = []
        result += '(machine ' + str(self.machine) + ')'
        result += '(memory ' + str(self.memory) + ')'
        result += '(benchmarks '
        for b in self.benchmarks:
            result += str(b)
        result += ')'
        if self.fifos:
            result += '(fifos '
            for f in self.fifos:
                result += str(f)
            result += ')'
        return ''.join(result)


def parse_model_file(filename):
    try:
        with open(filename, 'r') as f:
            return parse_model(lex.Lexer(f))
    except IOError as e:
        print('ERROR:', e)
        return None


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
        elif name == 'fifos':
            model.fifos = _parse_fifos(lexer)
        elif name == 'include':
            value = lexer.get_value()
            lexer.match(lex.TOKEN_LITERAL)
            parse_model(lex.Lexer(open(value, 'r')), model)
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


def _parse_fifos(lexer):
    fifos = []
    while lexer.get_type() == lex.TOKEN_LITERAL:
        fifos += _parse_int(lexer)
    return fifos
