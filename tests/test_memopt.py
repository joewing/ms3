
import unittest

from memsim.distribution import Distribution
from memsim.memopt import MemoryOptimizer
from memsim.machine import MachineType
from memsim.memory import MemoryList
from memsim.model import Model
from tests.mocks import MockMemory


class TestMemoryOptimizer(unittest.TestCase):

    def setUp(self):
        self.model = Model()
        self.model.machine = MachineType(word_size=8, addr_bits=32)
        self.rand = Distribution(1)
        self.ml = MemoryList([])
        self.optimizer = MemoryOptimizer(self.model, self.ml, None, None)
        self.optimizer.constructors = [self.mocks_constructor]

    def mocks_constructor(self, mod, nxt, rand, cost):
        return MockMemory(nxt)

    def test_insert1(self):
        main = MockMemory()
        result = self.optimizer.insert(self.rand, main, 0, 0)
        self.assertEqual(str(result), "(mock (mock))")

    def test_insert2(self):
        main = MockMemory(MockMemory(), MockMemory())
        result = self.optimizer.insert(self.rand, main, 1, 0)
        self.assertEqual(str(result), "(mock (mock (mock))(mock))")

    def test_remove1(self):
        main = MockMemory(MockMemory(),
                          MockMemory(MockMemory(MockMemory()), MockMemory()),
                          MockMemory(MockMemory()))
        result = self.optimizer.remove(self.rand, main, 3)
        s = "(mock (mock)(mock (mock)(mock))(mock (mock)))"
        self.assertEqual(str(result), s)
