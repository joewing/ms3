
import unittest
from mocks import MockMemory
from distribution import Distribution
from optimizer import Optimizer
from machine import MachineType
from memory import MemoryList


class TestOptimizer(unittest.TestCase):

    def setUp(self):
        self.machine = MachineType(word_size=8, addr_bits=32)
        self.rand = Distribution(1)
        self.ml = MemoryList([], self.rand)
        self.optimizer = Optimizer(self.machine, self.ml, None, None)
        self.optimizer.constructors = [self.mocks_constructor]

    def mocks_constructor(self, machine, nxt, rand, cost):
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
