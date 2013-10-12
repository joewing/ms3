
import unittest
import machine
import mock
import lex
import memory
from memory.xor import XOR

class TestXOR(unittest.TestCase):

   def setUp(self):
      self.machine = machine.MachineType()
      self.main = mock.MockMemory()
      self.join = memory.Join()
      self.bank = mock.MockMemory(self.join)

   def test_xor16(self):
      xor = XOR(self.bank, self.main, 16)
      xor.reset(self.machine)

      t = xor.process(0, False, 32, 8)
      self.assertEqual(t, 1600)
      self.assertEqual(self.bank.reads, 1)
      self.assertEqual(self.bank.writes, 0)
      self.assertEqual(self.main.reads, 1)
      self.assertEqual(self.main.writes, 0)
      self.assertEqual(self.main.last_addr, 32)
      self.assertEqual(self.main.last_size, 8)
      self.assertEqual(self.bank.last_addr, 48)
      self.assertEqual(self.bank.last_size, 8)

   def test_simplify1(self):
      xor = XOR(self.bank, self.main, 16)
      simplified = xor.simplify()
      self.assertEqual(xor, simplified)

   def test_simplify2(self):
      xor = XOR(self.bank, self.main, 0)
      simplified = xor.simplify()
      self.assertEqual(simplified, self.bank)

   def test_simplify3(self):
      xor = XOR(self.join, self.main, 8)
      simplified = xor.simplify()
      self.assertEqual(simplified, self.main)

   def test_parse(self):
      s = "(xor (value 1024)(bank (join))(memory (ram (latency 100))))"
      l = lex.Lexer(mock.MockFile(s))
      result = memory.parse_memory(l)
      self.assertEqual(str(result), s)

