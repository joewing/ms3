

import unittest
from ..machine import MachineType, TargetType, log2, round_power2


class TestMachine(unittest.TestCase):

    def test_type(self):
        m = MachineType(TargetType.SIMPLE, 1e9, 4, 32)
        self.assertEqual(m.word_size, 4)
        self.assertEqual(m.word_bits, 2)
        self.assertEqual(m.word_mask, 3)
        self.assertEqual(m.addr_bits, 32)
        self.assertEqual(m.addr_mask, 0xFFFFFFFF)

    def test_log2(self):
        self.assertEqual(log2(0), 0)
        self.assertEqual(log2(5), 3)
        self.assertEqual(log2(7), 3)
        self.assertEqual(log2(8), 4)

    def test_rp2(self):
        self.assertEqual(round_power2(4), 4)
        self.assertEqual(round_power2(5), 8)
        self.assertEqual(round_power2(7), 8)
        self.assertEqual(round_power2(8), 8)
