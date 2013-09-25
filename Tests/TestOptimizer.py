
import unittest
from MockMemory import MockMemory
from Optimizer import Optimizer
from Process import ProcessList, Process
from Machine import MachineType

class TestOptimizer(unittest.TestCase):

   def setUp(self):
      self.machine = MachineType(word_size = 8, addr_bits = 32)
      self.pl = ProcessList(self.machine)
      self.optimizer = Optimizer(self.machine, self.pl)
      self.optimizer.constructors = [ self.mock_constructor ]

   def mock_constructor(self, machine, nxt, rand, cost):
      return MockMemory(nxt)

   def test_insert1(self):
      main = MockMemory()
      self.pl.insert(Process(main, None))
      result = self.optimizer.insert(main, 0, 0)
      self.assertEqual(str(result), "(mock (mock ))")

   def test_insert2(self):
      main = MockMemory(MockMemory(), MockMemory())
      self.pl.insert(Process(main, None))
      result = self.optimizer.insert(main, 1, 0)
      self.assertEqual(str(result), "(mock (mock (mock ))(mock ))")

