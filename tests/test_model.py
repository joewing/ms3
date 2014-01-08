
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
        m.fifos = ['f1', 'f2']
        expected = '(machine test-machine)'
        expected += '(memory test-memory)'
        expected += '(benchmarks abc)'
        expected += '(fifos f1f2)'
        self.assertEqual(str(m), expected)

    def test_str2(self):
        m = Model()
        m.machine = 'test-machine'
        m.memory = 'test-memory'
        m.benchmarks = []
        m.fifos = []
        expected = '(machine test-machine)'
        expected += '(memory test-memory)'
        expected += '(benchmarks )'
        self.assertEqual(str(m), expected)

    def test_parse(self):
        to_parse = '(machine (word_size 2))'
        to_parse += '(memory (ram))'
        to_parse += '(benchmarks (mm) (heap))'
        to_parse += '(fifos 8 16)'
        l = Lexer(StringIO(to_parse))
        m = parse_model(l)
        self.assertEqual(m.machine.word_size, 2)
        self.assertIsInstance(m.memory, RAM)
        self.assertEqual(len(m.benchmarks), 2)
        self.assertIsInstance(m.benchmarks[0], mm.MM)
        self.assertIsInstance(m.benchmarks[1], heap.Heap)
        self.assertEqual(len(m.fifos), 2)
        self.assertEqual(m.fifos[0], 8)
        self.assertEqual(m.fifos[1], 16)
