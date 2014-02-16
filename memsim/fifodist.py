from distribution import Distribution


class FIFODistribution(Distribution):

    def __init__(self, seed):
        Distribution.__init__(self, seed)

    def load(self, state, index):
        pass

    def save(self, state, index):
        pass

    def is_empty(self):
        return False

    def insert_range(self, addr, size):
        pass

    def random_address(self, alignment):
        pass
