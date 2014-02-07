
from memsim.memory import container


class Stats(container.Container):
    """Fake memory to collect access stats from a benchmark."""

    def __init__(self, dist, mem):
        container.Container.__init__(self, mem)
        self.dist = dist

    def __str__(self):
        return '(stats ' + self.get_next().get_name() + ')'

    def process(self, start, write, addr, size):
        self.dist.insert_range(addr, size)
        return container.Container.process(self, start, write, addr, size)
