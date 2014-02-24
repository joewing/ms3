import unittest

from memsim import lex, machine, memory, vhdl
from memsim.memory import MemoryList
from memsim.memory.cache import Cache, CachePolicy
from memsim.memory.subsystem import Subsystem
from tests import mocks


class TestCache(unittest.TestCase):

    def setUp(self):
        self.machine = machine.MachineType()
        self.main = mocks.MockMemory()
        self.main.word_size = 4

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
        t = cache.process(1, False, 0, 1)
        self.assertEqual(t, 801 + 1)
        self.machine.time += t

        # Hit [(0,4), -, -, -] -> [(0,4), -, -, -]
        t = cache.process(2, False, 1, 1)
        self.assertEqual(t, 3 + 2 - 2)
        self.machine.time += t

        # Hit [(0,4), -, -, -] -> [(0*,4), -, -, -]
        t = cache.process(3, True, 1, 1)
        self.assertEqual(t, 3 + 3 - 2)
        self.machine.time += t

        # Miss [(0*,4), -, -, -] -> [(0*,4), (8,16), -, -] (read 8..15)
        t = cache.process(4, False, 8, 1)
        self.assertEqual(t, 803 + 4 - 2)
        self.machine.time += t

        # Hit [(0*,4), (8,16), -, -] -> [(0*,4), (8,16), -, -]
        t = cache.process(5, False, 2, 2)
        self.assertEqual(t, 3 + 5 - 2)
        self.machine.time += t

        # Hit [(0*,4), (8,16), -, -] -> [(0*,4*), (8,16), -, -]
        t = cache.process(6, True, 4, 2)
        self.assertEqual(t, 3 + 6 - 2)
        self.machine.time += t

        # Hit [(0*,4*), (8,16), -, -] -> [(0*,4*), (8,16), -, -]
        t = cache.process(7, True, 6, 1)
        self.assertEqual(t, 3 + 7 - 2)
        self.machine.time += t

        t = cache.done()
        self.assertEqual(t, 2)

    def test_set(self):
        cache = Cache(self.main,
                      line_count=4,
                      line_size=4,
                      associativity=2,
                      policy=CachePolicy.LRU,
                      access_time=1,
                      cycle_time=1,
                      write_back=True)
        cache.reset(self.machine)
        self.assertEqual(cache.access_time, 3)
        self.assertEqual(cache.cycle_time, 3)

        t = cache.process(10, False, 0, 1)
        self.assertEqual(t, 403 + 10)
        self.machine.time += t

        t = cache.process(11, False, 1, 1)
        self.assertEqual(t, 3 + 11)
        self.machine.time += t

        t = cache.process(12, True, 1, 1)
        self.assertEqual(t, 3 + 12)
        self.machine.time += t

        t = cache.process(13, False, 8, 1)
        self.assertEqual(t, 403 + 13)
        self.machine.time += t

        t = cache.process(14, False, 12, 2)
        self.assertEqual(t, 403 + 14)
        self.machine.time += t

        t = cache.process(15, True, 4, 2)
        self.assertEqual(t, 403 + 15)
        self.machine.time += t

        t = cache.process(16, True, 16, 1)
        self.assertEqual(t, 803 + 16)
        self.machine.time += t

        t = cache.done()
        self.assertEqual(t, 0)

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

        # Miss [-, -] -> [(0,4), -] (read 0,4)
        t = cache.process(0, False, 0, 1)
        self.assertEqual(t, 803)
        self.assertEqual(self.main.reads, 2)
        self.assertEqual(self.main.writes, 0)
        self.assertEqual(self.main.last_addr, 4)
        self.machine.time += t

        # Miss [0, -] -> [(8,12), (0,4)] (read 8,12)
        t = cache.process(0, False, 8, 1)
        self.assertEqual(t, 803)
        self.assertEqual(self.main.reads, 4)
        self.assertEqual(self.main.writes, 0)
        self.assertEqual(self.main.last_addr, 12)
        self.machine.time += t

        # Hit [(8,12), (0,4)] -> [(8,12), (0,4)]
        t = cache.process(0, False, 0, 1)
        self.assertEqual(t, 3)
        self.assertEqual(self.main.reads, 4)
        self.assertEqual(self.main.writes, 0)
        self.assertEqual(self.main.last_addr, 12)
        self.machine.time += t

        # Hit [(8,12), (0,4)] -> [(8,12), (0,4)]
        t = cache.process(0, False, 8, 1)
        self.assertEqual(t, 3)
        self.assertEqual(self.main.reads, 4)
        self.assertEqual(self.main.writes, 0)
        self.assertEqual(self.main.last_addr, 12)
        self.machine.time += t

        # Miss [(8,12), (0,4)] -> [(16,20), (8,12)] (read 16,20)
        t = cache.process(0, False, 16, 1)
        self.assertEqual(t, 803)
        self.assertEqual(self.main.reads, 6)
        self.assertEqual(self.main.writes, 0)
        self.assertEqual(self.main.last_addr, 20)
        self.machine.time += t

        # Hit [(16,20), (8,12)] -> [(16,20), (8,12)]
        t = cache.process(0, False, 8, 1)
        self.assertEqual(t, 3)
        self.assertEqual(self.main.reads, 6)
        self.assertEqual(self.main.writes, 0)
        self.assertEqual(self.main.last_addr, 20)
        self.machine.time += t

        # Miss [(16,20), (8,12)] -> [(24*,28*), (16,20)]
        t = cache.process(0, True, 24, 8)
        self.assertEqual(t, 3)
        self.assertEqual(self.main.reads, 6)
        self.assertEqual(self.main.writes, 0)
        self.assertEqual(self.main.last_addr, 20)
        self.machine.time += t

        # Miss [(24*,28*), (16,20)] -> [(32,36), (24*,28*)] (read 32, 36)
        t = cache.process(0, False, 32, 1)
        self.assertEqual(t, 803)
        self.assertEqual(self.main.reads, 8)
        self.assertEqual(self.main.writes, 0)
        self.assertEqual(self.main.last_addr, 36)
        self.machine.time += t

        # Miss [(32,36), (24*,28*)] -> [(40,44), (32,36)]
        # (write 24,28; read 40,44)
        t = cache.process(0, False, 40, 1)
        self.assertEqual(t, 1603)
        self.assertEqual(self.main.reads, 10)
        self.assertEqual(self.main.writes, 2)
        self.assertEqual(self.main.last_addr, 44)
        self.machine.time += t

        t = cache.done()
        self.assertEqual(t, 0)

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

        t = cache.done()
        self.assertEqual(t, 0)

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
        gen = vhdl.VHDLGenerator(self.machine)
        ml = MemoryList(self.main)
        ml.add_memory(Subsystem(0, cache))
        result = gen.generate(ml)
        self.assertNotEqual(result, None)
        self.assertEqual(self.main.generated, 1)

    def test_parse1(self):
        s = '(cache (line_count 2)(line_size 4)(associativity 2)'
        s += '(access_time 2)(cycle_time 3)'
        s += '(policy fifo)(write_back false)(memory (ram)))'
        l = lex.Lexer(mocks.MockFile(s))
        result = memory.parse_memory(l)
        expected = '(cache (line_count 2)(line_size 4)(associativity 2)'
        expected += '(access_time 2)(cycle_time 3)'
        expected += '(policy fifo)(write_back false)(memory (main)))'
        self.assertEqual(str(result), expected)

    def test_parse2(self):
        s = '(cache (line_count 2)(line_size 4)(associativity 2)'
        s += '(access_time 2)(cycle_time 3)'
        s += '(policy bad)(write_back false)(memory (main)))'
        l = lex.Lexer(mocks.MockFile(s))
        with self.assertRaises(lex.ParseError):
            memory.parse_memory(l)
