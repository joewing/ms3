
import unittest

from memsim import lex, machine, memory, vhdl
from memsim.memory.cache import Cache, CachePolicy
from memsim.tests import mocks


class TestCache(unittest.TestCase):

    def setUp(self):
        self.machine = machine.MachineType(word_size=4)
        self.main = mocks.MockMemory()

    def test_direct(self):
        cache = Cache(self.main,
                      line_count=4,
                      line_size=8,
                      associativity=1,
                      access_time=1,
                      cycle_time=1,
                      write_back=True)
        cache.reset(self.machine)
        self.assertEqual(cache.access_time, 3)
        self.assertEqual(cache.cycle_time, 3)
        cache.access_time = 1

        # Miss [-, -, -, -] -> [(0,4), -, -, -] (read 0..7)
        t = cache.process(0, False, 0, 1)
        self.assertEqual(t, 801)
        self.machine.time += t

        # Hit [(0,4), -, -, -] -> [(0,4), -, -, -]
        t = cache.process(0, False, 1, 1)
        self.assertEqual(t, 3)
        self.machine.time += t

        # Hit [(0,4), -, -, -] -> [(0*,4), -, -, -]
        t = cache.process(0, True, 1, 1)
        self.assertEqual(t, 3)
        self.machine.time += t

        # Miss [(0*,4), -, -, -] -> [(0*,4), (8,16), -, -] (read 8..15)
        t = cache.process(0, False, 8, 1)
        self.assertEqual(t, 803)
        self.machine.time += t

        # Hit [(0*,4), (8,16), -, -] -> [(0*,4), (8,16), -, -]
        t = cache.process(0, False, 2, 2)
        self.assertEqual(t, 3)
        self.machine.time += t

        # Hit [(0*,4), (8,16), -, -] -> [(0*,4*), (8,16), -, -]
        t = cache.process(0, True, 4, 2)
        self.assertEqual(t, 3)
        self.machine.time += t

        # Hit [(0*,4*), (8,16), -, -] -> [(0*,4*), (8,16), -, -]
        t = cache.process(0, True, 6, 1)
        self.assertEqual(t, 3)
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

    def test_fifo(self):
        cache = Cache(self.main,
                      line_count=2,
                      line_size=8,
                      associativity=2,
                      policy=CachePolicy.FIFO,
                      access_time=1,
                      cycle_time=1,
                      write_back=True)
        cache.reset(self.machine)
        self.assertEqual(cache.access_time, 3)
        self.assertEqual(cache.cycle_time, 3)

        # Miss [-, -] -> [(0,4), -] (read 0)
        t = cache.process(0, False, 0, 1)
        self.assertEqual(t, 803)
        self.assertEqual(self.main.reads, 1)
        self.assertEqual(self.main.writes, 0)
        self.assertEqual(self.main.last_addr, 0)
        self.machine.time += t

        # Miss [0, -] -> [(8,12), (0,4)] (read 8)
        t = cache.process(0, False, 8, 1)
        self.assertEqual(t, 803)
        self.assertEqual(self.main.reads, 2)
        self.assertEqual(self.main.writes, 0)
        self.assertEqual(self.main.last_addr, 8)
        self.machine.time += t

        # Hit [(8,12), (0,4)] -> [(8,12), (0,4)]
        t = cache.process(0, False, 0, 1)
        self.assertEqual(t, 3)
        self.assertEqual(self.main.reads, 2)
        self.assertEqual(self.main.writes, 0)
        self.assertEqual(self.main.last_addr, 8)
        self.machine.time += t

        # Hit [(8,12), (0,4)] -> [(8,12), (0,4)]
        t = cache.process(0, False, 8, 1)
        self.assertEqual(t, 3)
        self.assertEqual(self.main.reads, 2)
        self.assertEqual(self.main.writes, 0)
        self.assertEqual(self.main.last_addr, 8)
        self.machine.time += t

        # Miss [(8,12), (0,4)] -> [(16,20), (8,12)] (read 16)
        t = cache.process(0, False, 16, 1)
        self.assertEqual(t, 803)
        self.assertEqual(self.main.reads, 3)
        self.assertEqual(self.main.writes, 0)
        self.assertEqual(self.main.last_addr, 16)
        self.machine.time += t

        # Hit [(16,20), (8,12)] -> [(16,20), (8,12)]
        t = cache.process(0, False, 8, 1)
        self.assertEqual(t, 3)
        self.assertEqual(self.main.reads, 3)
        self.assertEqual(self.main.writes, 0)
        self.assertEqual(self.main.last_addr, 16)
        self.machine.time += t

        # Miss [(16,20), (8,12)] -> [(24*,28*), (16,20)]
        t = cache.process(0, True, 24, 8)
        self.assertEqual(t, 3)
        self.assertEqual(self.main.reads, 3)
        self.assertEqual(self.main.writes, 0)
        self.assertEqual(self.main.last_addr, 16)
        self.machine.time += t

        # Miss [(24*,28*), (16,20)] -> [(32,36), (24*,28*)] (read 32)
        t = cache.process(0, False, 32, 1)
        self.assertEqual(t, 803)
        self.assertEqual(self.main.reads, 4)
        self.assertEqual(self.main.writes, 0)
        self.assertEqual(self.main.last_addr, 32)
        self.machine.time += t

        # Miss [(32,36), (24*,28*)] -> [(40,44), (32,36)] (write 24, read 40)
        t = cache.process(0, False, 40, 1)
        self.assertEqual(t, 1603)
        self.assertEqual(self.main.reads, 5)
        self.assertEqual(self.main.writes, 1)
        self.assertEqual(self.main.last_addr, 40)
        self.machine.time += t

    def test_write_through(self):
        cache = Cache(self.main,
                      line_count=4,
                      line_size=4,
                      associativity=1,
                      policy=CachePolicy.LRU,
                      access_time=1,
                      cycle_time=1,
                      write_back=False)
        cache.reset(self.machine)
        self.assertEqual(cache.access_time, 3)
        self.assertEqual(cache.cycle_time, 3)

        # Miss [-, -] -> [0, -] (read 0)
        t = cache.process(0, False, 0, 1)
        self.assertEqual(t, 403)
        self.assertEqual(self.main.reads, 1)
        self.assertEqual(self.main.writes, 0)
        self.assertEqual(self.main.last_addr, 0)
        self.machine.time += t

        # Miss [0, -] -> [0, 4] (read 4)
        t = cache.process(0, False, 4, 1)
        self.assertEqual(t, 403)
        self.assertEqual(self.main.reads, 2)
        self.assertEqual(self.main.writes, 0)
        self.assertEqual(self.main.last_addr, 4)
        self.machine.time += t

        # Miss [0, 4] -> [0, 4] (write 8)
        t = cache.process(0, True, 8, 1)
        self.assertEqual(t, 103)
        self.assertEqual(self.main.reads, 2)
        self.assertEqual(self.main.writes, 1)
        self.assertEqual(self.main.last_addr, 8)
        self.machine.time += t

        # Hit [0, 4] -> [0, 4]
        t = cache.process(0, False, 0, 1)
        self.assertEqual(t, 3)
        self.assertEqual(self.main.reads, 2)
        self.assertEqual(self.main.writes, 1)
        self.assertEqual(self.main.last_addr, 8)
        self.machine.time += t

        # Hit [0, 4] -> [0, 4]
        t = cache.process(0, False, 4, 1)
        self.assertEqual(t, 3)
        self.assertEqual(self.main.reads, 2)
        self.assertEqual(self.main.writes, 1)
        self.assertEqual(self.main.last_addr, 8)
        self.machine.time += t

    def test_simplify1(self):
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

    def test_parse1(self):
        s = "(cache (line_count 2)(line_size 4)(associativity 2)"
        s += "(access_time 2)(cycle_time 3)"
        s += "(policy fifo)(write_back false)(memory (ram (latency 100))))"
        l = lex.Lexer(mocks.MockFile(s))
        result = memory.parse_memory(l)
        self.assertEqual(str(result), s)

    def test_parse2(self):
        s = "(cache (line_count 2)(line_size 4)(associativity 2)"
        s += "(access_time 2)(cycle_time 3)"
        s += "(policy bad)(write_back false)(memory (ram (latency 100))))"
        l = lex.Lexer(mocks.MockFile(s))
        with self.assertRaises(lex.ParseError):
            memory.parse_memory(l)
