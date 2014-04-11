import random

from memsim.memory.fifo import FIFO
from memsim.memory.subsystem import Subsystem
from memsim.sim.fifodist import FIFODistribution
from memsim.sim.memdist import MemoryDistribution


class DistributionList(object):

    def __init__(self, seed):
        self.rand = random.Random(seed)
        self.start_seed = seed
        self.fifo_dists = dict()
        self.subsystem_dists = dict()

    def get_distribution(self, mem):
        if isinstance(mem, FIFO):
            return self.get_fifo_distribution(mem)
        elif isinstance(mem, Subsystem):
            return self.get_subsystem_distribution(mem)
        else:
            assert(False)

    def get_fifo_distribution(self, fifo):
        index = fifo.index
        if index not in self.fifo_dists:
            self.fifo_dists[index] = FIFODistribution(fifo, self.rand)
        return self.fifo_dists[index]

    def get_subsystem_distribution(self, subsystem):
        index = subsystem.index
        if index not in self.subsystem_dists:
            self.subsystem_dists[index] = MemoryDistribution(self.rand)
        return self.subsystem_dists[index]

    def load(self, state, m):
        for index in state.get('subsystems', []):
            if index not in self.subsystem_dists:
                self.subsystem_dists[index] = MemoryDistribution(self.rand)
            dist = self.subsystem_dists[index]
            dist.load(state, index)
            for b in m.benchmarks:
                if b.index == index:
                    if dist.is_empty():
                        b.max_addr = 0
                    else:
                        b.max_addr = dist.get_max_address()
                    break

    def save(self, state):
        for index, dist in self.subsystem_dists.items():
            dist.save(state, index)
        state['subsystems'] = self.subsystem_dists.keys()
