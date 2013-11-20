
import unittest

from memsim.database import simple
from memsim.database.instance import set_instance
from memsim.machine import MachineType, TargetType
from memsim.memory import cache
from memsim.memory import cacti
from memsim.memory import spm
from memsim.tests.mocks import MockMemory


class TestCACTI(unittest.TestCase):

    def setUp(self):
        self.db = simple.SimpleDatabase()
        set_instance(self.db)
        self.machine = MachineType(target=TargetType.ASIC,
                                   word_size=4)

    def test_cache1(self):

        # Add the parameters to the database.
        params = cacti.CACTIParams()
        params.size = self.machine.word_size * 16 * 8
        params.block_size = self.machine.word_size * 8
        params.bus_bits = self.machine.addr_bits + self.machine.word_size * 8
        params.associativity = 2
        params.is_cache = True
        self.db.add_cacti_result(params, 5, 6, 1.2)

        # Create a cache to match the parameters.
        m = MockMemory()
        c = cache.Cache(m,
                        line_count=16,
                        line_size=8,
                        associativity=2,
                        policy=cache.CachePolicy.LRU,
                        write_back=True)

        # Check the parameters.
        c.reset(self.machine)
        self.assertEqual(c.get_cost(), 1.2)
        self.assertEqual(c.access_time, 5)
        self.assertEqual(c.cycle_time, 6)

    def test_spm1(self):

        # Add the parameters to the database.
        params = cacti.CACTIParams()
        params.size = 256
        params.block_size = self.machine.word_size
        params.bus_bits = self.machine.word_size * 8
        params.associativity = 1
        params.is_cache = False
        self.db.add_cacti_result(params, 2, 3, 4)

        # Create an SPM to match the parameters.
        m = MockMemory()
        s = spm.SPM(m, size=256)

        # Check the parameters.
        s.reset(self.machine)
        self.assertEqual(s.get_cost(), 4)
        self.assertEqual(s.access_time, 2)
        self.assertEqual(s.cycle_time, 3)
