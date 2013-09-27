
import unittest
from MockMemory import MockMemory
from Distribution import Distribution
from Optimizer import Optimizer
from Process import ProcessList, Process
from Machine import MachineType

class TestOptimizer(unittest.TestCase):

   def setUp(self):
      self.machine = MachineType(word_size = 8, addr_bits = 32)
      self.pl = ProcessList(self.machine)
      self.rand = Distribution(1)
      self.optimizer = Optimizer(self.machine, self.rand, self.pl)
      self.optimizer.constructors = [ self.mock_constructor ]

   def mock_constructor(self, machine, nxt, rand, cost):
      return MockMemory(nxt)

   def test_insert1(self):
      main = MockMemory()
      self.pl.insert(Process(self.rand, main, None))
      result = self.optimizer.insert(self.rand, main, 0, 0)
      self.assertEqual(str(result), "(mock (mock))")

   def test_insert2(self):
      main = MockMemory(MockMemory(), MockMemory())
      self.pl.insert(Process(self.rand, main, None))
      result = self.optimizer.insert(self.rand, main, 1, 0)
      self.assertEqual(str(result), "(mock (mock (mock))(mock))")

   def test_remove1(self):
      main = MockMemory(MockMemory(),
                        MockMemory(MockMemory(MockMemory()), MockMemory()),
                        MockMemory(MockMemory()))
      self.pl.insert(Process(self.rand, main, None))
      result = self.optimizer.remove(self.rand, main, 3)
      self.assertEqual(str(result),
                       "(mock (mock)(mock (mock)(mock))(mock (mock)))")

