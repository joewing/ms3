import copy


class Cost(object):

    def __init__(self, cost=1 << 31, size=0, luts=0, regs=0):
        self.cost = cost
        self.size = size
        self.luts = luts
        self.regs = regs

    def clone(self):
        return copy.deepcopy(self)

    def __str__(self):
        result = str(self.cost)
        result += ', ' + str(self.luts)
        result += ', ' + str(self.regs)
        return result

    def __add__(self, other):
        return Cost(self.cost + other.cost,
                    self.size + other.size,
                    self.luts + other.luts,
                    self.regs + other.regs)

    def __sub__(self, other):
        return Cost(self.cost - other.cost,
                    self.size - other.size,
                    self.luts - other.luts,
                    self.regs - other.regs)

    def fits(self, other):
        if self.cost >= other.cost:
            return False
        if self.size >= other.size:
            return False
        if self.luts >= other.luts:
            return False
        if self.regs >= other.regs:
            return False
        return True
