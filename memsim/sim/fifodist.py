from memsim.sim.dist import BaseDistribution


class FIFODistribution(BaseDistribution):

    def __init__(self, fifo, rand):
        BaseDistribution.__init__(self, rand)
        self.fifo = fifo

    def load(self, state, index):
        pass

    def save(self, state, index):
        pass

    def is_empty(self):
        return False

    def insert_range(self, addr, size):
        pass

    def random_address(self, alignment):
        size = self.fifo.total_size()
        temp = (size + alignment - 1) // alignment
        return self.randint(0, temp - 1) * alignment
