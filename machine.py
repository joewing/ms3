
class TargetType:
    SIMPLE = 0
    ASIC = 1
    FPGA = 2


def parse_target(s):
    if s == 'simple':
        return TargetType.SIMPLE
    elif s == 'asic':
        return TargetType.ASIC
    elif s == "fpga":
        return TargetType.FPGA
    else:
        return None


def show_target(t):
    if t == TargetType.SIMPLE:
        return "simple"
    elif t == TargetType.ASIC:
        return "asic"
    elif t == TargetType.FPGA:
        return "fpga"
    else:
        return "?"


class MachineType:

    def __init__(self,
                 target=TargetType.SIMPLE,
                 frequency=1e9,
                 word_size=8,
                 addr_bits=32,
                 max_path_length=64,
                 max_cost=10000,
                 technology=0.045,
                 part='xc7v585t'):
        self.target = target
        self.part = part
        self.frequency = frequency
        self.technology = technology
        self.word_size = word_size
        self.word_bits = log2(word_size) - 1
        self.word_mask = word_size - 1
        self.addr_bits = addr_bits
        self.addr_mask = (1 << addr_bits) - 1
        self.max_path_length = max_path_length
        self.max_cost = max_cost
        self.time = 0
        self.ports = []

    def __str__(self):
        result = "(machine "
        result += "(target " + show_target(self.target) + ")"
        if self.target == TargetType.FPGA:
            result += "(part " + str(self.part) + ")"
        elif self.target == TargetType.ASIC:
            result += "(technology " + str(self.technology) + ")"
        result += "(frequency " + str(self.frequency) + ")"
        result += "(word_size " + str(self.word_size) + ")"
        result += "(addr_bits " + str(self.addr_bits) + ")"
        result += "(max_path " + str(self.max_path_length) + ")"
        result += "(max_cost " + str(self.max_cost) + ")"
        result += ")"
        return result

    def reset(self):
        self.time = 0
        for i in range(len(self.ports)):
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
        for i in range(self.addr_bits):
            if (value & src_mask) != 0:
                result |= dest_mask
            src_mask >>= 1
            dest_mask <<= 1
        return result


def log2(n):
    """Compute the log base 2 of n."""
    r = 0
    while n > 0:
        r += 1
        n >>= 1
    return r


def round_power2(n):
    """Round n up to the next highest power of 2."""
    return 1 << log2(n - 1)
