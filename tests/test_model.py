from unittest import TestCase
from StringIO import StringIO

from memsim.lex import Lexer
from memsim.model import Model, parse_model
from memsim.memory.ram import RAM
from memsim.benchmarks import heap, mm


class TestModel(TestCase):

    def test_str1(self):
        m = Model()
        m.machine = 'test-machine'
        m.memory = 'test-memory'
        m.benchmarks = ['a', 'b', 'c']
        expected = '(machine test-machine)'
        expected += '(memory test-memory)'
        expected += '(benchmarks abc)'
        self.assertEqual(str(m), expected)

    def test_str2(self):
        m = Model()
        m.machine = 'test-machine'
        m.memory = 'test-memory'
        m.benchmarks = []
        expected = '(machine test-machine)'
        expected += '(memory test-memory)'
        expected += '(benchmarks )'
        self.assertEqual(str(m), expected)

    def test_parse(self):
        to_parse = '(machine (word_size 2))'
        to_parse += '(memory (main (memory (ram))))'
        to_parse += '(benchmarks (mm) (heap))'
        l = Lexer(StringIO(to_parse))
        m = parse_model(l)
        self.assertEqual(m.machine.word_size, 2)
        self.assertIsInstance(m.memory.main_memory, RAM)
        self.assertEqual(len(m.benchmarks), 2)
        self.assertIsInstance(m.benchmarks[0], mm.MM)
        self.assertIsInstance(m.benchmarks[1], heap.Heap)
