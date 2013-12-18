
import unittest

from memsim import util


class TestUtil(unittest.TestCase):

    def test_log2(self):
        self.assertEqual(util.log2(0), 0)
        self.assertEqual(util.log2(5), 3)
        self.assertEqual(util.log2(7), 3)
        self.assertEqual(util.log2(8), 4)

    def test_rp2(self):
        self.assertEqual(util.round_power2(4), 4)
        self.assertEqual(util.round_power2(5), 8)
        self.assertEqual(util.round_power2(7), 8)
        self.assertEqual(util.round_power2(8), 8)

    def test_get_experiment_name(self):
        full_name, expected = 'experiments/exp-a-b', 'exp'
        self.assertEqual(util.get_experiment_name(full_name), expected)
