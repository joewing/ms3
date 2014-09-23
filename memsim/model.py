from memsim import benchmarks, lex, machine, memory


class Model(object):

    def __init__(self):
        self.label = 'undefined'
        self.machine = machine.MachineType()
        self.memory = None
        self.benchmarks = []

    def __str__(self):
        return self.get_full_name()

    def get_full_name(self):
        result = []
        result += '(label ' + str(self.label) + ')'
        result += '(machine ' + str(self.machine) + ')'
        result += '(memory ' + self.memory.get_full_name() + ')'
        result += '(benchmarks '
        for b in self.benchmarks:
            result += str(b)
        result += ')'
        return ''.join(result)

    def get_name(self):
        result = []
        result += '(label ' + str(self.label) + ')'
        result += '(machine ' + str(self.machine) + ')'
        result += '(memory ' + str(self.memory) + ')'
        result += '(benchmarks '
        for b in self.benchmarks:
            result += str(b)
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
            model.memory = memory.parse_memory_list(lexer)
        elif name == 'benchmarks':
            model.benchmarks = _parse_benchmarks(lexer)
        elif name == 'label':
            model.label = _parse_str(lexer)
        elif name == 'include':
            value = lexer.get_value()
            lexer.match(lex.TOKEN_LITERAL)
            parse_model(lex.Lexer(open(value, 'r')), model)
        else:
            lex.ParseError(lexer, 'invalid top-level component: ' + name)
        lexer.match(lex.TOKEN_CLOSE)
    return model


def _parse_str(lexer):
    value = lexer.get_value()
    lexer.match(lex.TOKEN_LITERAL)
    return value


def _parse_int(lexer):
    value = lexer.get_value()
    lexer.match(lex.TOKEN_LITERAL)
    return int(value)


def _parse_benchmarks(lexer):
    bms = []
    while lexer.get_type() == lex.TOKEN_OPEN:
        bms.append(benchmarks.parse_benchmark(lexer))
    return bms
