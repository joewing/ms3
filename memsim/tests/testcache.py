
import unittest

from .. import lex
from .. import machine
from .. import memory
from ..memory.cache import Cache, CachePolicy
from .. import vhdl
from . import mocks


class TestCache(unittest.TestCase):

    def setUp(self):
        self.machine = machine.MachineType()
        self.main = mocks.MockMemory()

    def test_direct(self):
        cache = Cache(self.main,
                      line_count=4,
                      line_size=2,
                      associativity=1,
                      access_time=1,
                      cycle_time=1,
                      write_back=True)
        cache.reset(self.machine)
        self.assertEqual(cache.access_time, 3)
        self.assertEqual(cache.cycle_time, 3)

        t = cache.process(0, False, 0, 1)
        self.assertEqual(t, 203)
        self.machine.time += t

        t = cache.process(0, False, 1, 1)
        self.assertEqual(t, 3)
        self.machine.time += t

        t = cache.process(0, True, 1, 1)
        self.assertEqual(t, 3)
        self.machine.time += t

        t = cache.process(0, False, 8, 1)
        self.assertEqual(t, 403)
        self.machine.time += t

        t = cache.process(0, False, 2, 2)
        self.assertEqual(t, 203)
        self.machine.time += t

        t = cache.process(0, True, 4, 2)
        self.assertEqual(t, 3)
        self.machine.time += t

        t = cache.process(0, True, 6, 1)
        self.assertEqual(t, 203)
        self.machine.time += t

    def test_set(self):
        cache = Cache(self.main,
                      line_count=4,
                      line_size=2,
                      associativity=2,
                      policy=CachePolicy.LRU,
                      access_time=1,
                      cycle_time=1,
                      write_back=True)
        cache.reset(self.machine)
        self.assertEqual(cache.access_time, 3)
        self.assertEqual(cache.cycle_time, 3)

        t = cache.process(0, False, 0, 1)
        self.assertEqual(t, 203)
        self.machine.time += t

        t = cache.process(0, False, 1, 1)
        self.assertEqual(t, 3)
        self.machine.time += t

        t = cache.process(0, True, 1, 1)
        self.assertEqual(t, 3)
        self.machine.time += t

        t = cache.process(0, False, 8, 1)
        self.assertEqual(t, 203)
        self.machine.time += t

        t = cache.process(0, False, 2, 2)
        self.assertEqual(t, 203)
        self.machine.time += t

        t = cache.process(0, True, 4, 2)
        self.assertEqual(t, 203)
        self.machine.time += t

        t = cache.process(0, True, 6, 1)
        self.assertEqual(t, 203)
        self.machine.time += t

    def testsimplify1(self):
        cache = Cache(self.main,
                      line_count=4,
                      line_size=2,
                      associativity=2,
                      policy=CachePolicy.LRU,
                      access_time=1,
                      cycle_time=1,
                      write_back=True)
        simplified = cache.simplify()
        self.assertEqual(cache, simplified)

    def test_path(self):
        cache = Cache(self.main,
                      line_count=4,
                      line_size=2,
                      associativity=2,
                      policy=CachePolicy.LRU,
                      access_time=1,
                      cycle_time=1,
                      write_back=True)
        cache.reset(self.machine)
        self.assertEqual(cache.get_path_length(), 0)
        self.assertEqual(cache.get_cost(), 4 * 2 * 8 + 4 * 17)

    def test_generate(self):
        cache = Cache(self.main,
                      line_count=4,
                      line_size=2,
                      associativity=2,
                      policy=CachePolicy.LRU,
                      access_time=1,
                      cycle_time=1,
                      write_back=True)
        gen = vhdl.VHDLGenerator()
        result = gen.generate(self.machine, cache)
        self.assertNotEqual(result, None)
        self.assertEqual(self.main.generated, 1)

    def test_parse(self):
        s = "(cache (line_count 2)(line_size 4)(associativity 2)"
        s += "(access_time 2)(cycle_time 3)"
        s += "(policy fifo)(write_back false)(memory (ram (latency 100))))"
        l = lex.Lexer(mocks.MockFile(s))
        result = memory.parse_memory(l)
        self.assertEqual(str(result), s)
