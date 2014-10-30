from memsim import lex, parser, util, cost


class TargetType(object):
    SIMPLE = 0
    ASIC = 1
    FPGA = 2


class GoalType(object):
    ACCESS_TIME = 0
    WRITES = 1
    ENERGY = 2
    MIX25 = 3
    MIX50 = 4
    MIX75 = 5


goals = [
    ('access_time', GoalType.ACCESS_TIME),
    ('writes', GoalType.WRITES),
    ('energy', GoalType.ENERGY),
    ('mix25', GoalType.MIX25),
    ('mix50', GoalType.MIX50),
    ('mix75', GoalType.MIX75),
]


def parse_target(s):
    if s == 'simple':
        return TargetType.SIMPLE
    elif s == 'asic':
        return TargetType.ASIC
    elif s == "fpga":
        return TargetType.FPGA
    else:
        assert(False)


def show_target(t):
    if t == TargetType.SIMPLE:
        return 'simple'
    elif t == TargetType.ASIC:
        return 'asic'
    elif t == TargetType.FPGA:
        return 'fpga'
    else:
        return '<{}>'.format(t)


def parse_goal(s):
    for name, value in goals:
        if name == s:
            return value
    assert(False)


def show_goal(t):
    for name, value in goals:
        if value == t:
            return name
    return '<{}>'.format(t)


class MachineType(object):

    def __init__(self,
                 target=TargetType.SIMPLE,
                 frequency=1e9,
                 addr_bits=32,
                 max_path_length=64,
                 max_cost=10000,
                 max_luts=10000,
                 max_regs=10000,
                 technology=0.045,
                 goal=GoalType.ACCESS_TIME,
                 part='xc7v585t'):
        self.target = target
        self.goal = goal
        self.part = part
        self.frequency = frequency
        self.technology = technology
        self.addr_bits = addr_bits
        self.addr_mask = (1 << addr_bits) - 1
        self.max_path_length = max_path_length
        self.max_cost = max_cost
        self.max_luts = max_luts
        self.max_regs = max_regs
        self.time = 0
        self.ports = []

    def __str__(self):
        result = '(target ' + show_target(self.target) + ')'
        if self.target == TargetType.FPGA:
            result += '(part ' + str(self.part) + ')'
            result += '(max_luts ' + str(self.max_luts) + ')'
            result += '(max_regs ' + str(self.max_regs) + ')'
        elif self.target == TargetType.ASIC:
            result += '(technology ' + str(self.technology) + ')'
        if self.goal != GoalType.ACCESS_TIME:
            result += '(goal ' + show_goal(self.goal) + ')'
        result += '(frequency ' + str(self.frequency) + ')'
        result += '(addr_bits ' + str(self.addr_bits) + ')'
        result += '(max_path ' + str(self.max_path_length) + ')'
        result += '(max_cost ' + str(self.max_cost) + ')'
        return result

    def get_zero_cost(self):
        return cost.Cost(cost=0, size=0, luts=0, regs=0)

    def get_max_cost(self):
        if self.target == TargetType.FPGA:
            return cost.Cost(cost=self.max_cost,
                             size=1 << self.addr_bits,
                             luts=self.max_luts,
                             regs=self.max_regs)
        else:
            return cost.Cost(cost=self.max_cost,
                             size=1 << self.addr_bits)

    def reset(self):
        self.time = 0
        for i in xrange(len(self.ports)):
            self.ports[i] = 0

    def reset_port(self, port):
        self.ports[port] = 0

    def produce(self, port):
        while len(self.ports) <= port:
            self.ports.append(0)
        self.ports[port] += 1
        return True

    def consume(self, port):
        while len(self.ports) <= port:
            self.ports.append(0)
        if self.ports[port] == 0:
            return False
        else:
            self.ports[port] -= 1
            return True

    def end(self, port):
        while len(self.ports) <= port:
            self.ports.append(0)
        self.ports[port] = -1
        return True

    def flip(self, value):
        """Reverse the bits in an addr_bits sized value."""
        src_mask = 1 << (self.addr_bits - 1)
        dest_mask = 1
        result = 0
        for i in xrange(self.addr_bits):
            if (value & src_mask) != 0:
                result |= dest_mask
            src_mask >>= 1
            dest_mask <<= 1
        return result

    def get_addr_width(self, word_size):
        """Get the number of bits needed for an address bus."""
        return self.addr_bits - util.get_bus_shift(word_size)


def parse_machine(lexer):
    args = parser.parse_arguments(lexer)
    addr_bits = parser.get_argument(lexer, args, 'addr_bits', 32)
    frequency = parser.get_argument(lexer, args, 'frequency', 1e9)
    technology = parser.get_argument(lexer, args, 'technology', 0.045)
    part = parser.get_argument(lexer, args, 'part', 'xc7v585t')
    max_path = parser.get_argument(lexer, args, 'max_path', 64)
    max_cost = parser.get_argument(lexer, args, 'max_cost', 10000)
    max_luts = parser.get_argument(lexer, args, 'max_luts', 10000)
    max_regs = parser.get_argument(lexer, args, 'max_regs', 10000)
    tstr = parser.get_argument(lexer, args, 'target', 'simple')
    gstr = parser.get_argument(lexer, args, 'goal', 'access_time')
    target = parse_target(tstr)
    if target is None:
        lex.ParseError(lexer, 'invalid target: ' + tstr)
    goal = parse_goal(gstr)
    if goal is None:
        lex.ParseError(lexer, 'invalid goal: ' + gstr)
    return MachineType(target=target,
                       goal=goal,
                       frequency=frequency,
                       addr_bits=addr_bits,
                       max_path_length=max_path,
                       max_cost=max_cost,
                       max_luts=max_luts,
                       max_regs=max_regs,
                       technology=technology,
                       part=part)
