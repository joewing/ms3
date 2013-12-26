
import unittest

from memsim.distribution import Distribution


class MockDistribution(Distribution):

    def __init__(self):
        Distribution.__init__(self, 0)
        self.index = 0

    def randint(self, a, b):
        v = self.index
        self.index += 1
        result = (v % (b - a + 1)) + a
        assert(result >= a)
        assert(result <= b)
        return result


class TestDistribution(unittest.TestCase):

    def test_range(self):
        dist = MockDistribution()
        dist.insert_range(8, 16)        # 8 - 24

        # Select first range (0)
        # Select end of range (1)
        a = dist.random_address(4)
        self.assertEqual(a, 20)

        # Select first (and only) range (2)
        # Select lower half of range (3) -> (8 - 16)
        # Select lower half of range (4) -> (8 - 12)
        # Break due to alignment -> 8
        a = dist.random_address(4)
        self.assertEqual(a, 8)

        self.assertEqual(dist.get_min_address(), 8)
        self.assertEqual(dist.get_max_address(), 24)
        self.assertEqual(dist.get_size(), 16)

    def test_transform(self):
        dist = MockDistribution()
        dist.insert_range(0, 16)
        dist.push_transform(lambda a: a + 4)

        # Select first range (0)
        # Select end of range (1)
        a = dist.random_address(4)
        self.assertEqual(a, 16)

        dist.pop_transform()

        # Select first (only) range (2)
        # Select lower half (3) -> (0 - 8)
        # Select lower half (4) -> (0 - 4)
        # Break due to alignment -> 0
        a = dist.random_address(4)
        self.assertEqual(a, 0)

    def test_limit(self):
        dist = MockDistribution()
        dist.insert_range(0, 16)
        dist.push_limit(0, 8)

        # Select first (0)
        # Select end of range (1)
        # Invalid address.
        # Select only range (2)
        # Select lower half (3) -> (0 - 8)
        # Select lower half (4) -> (0 - 4)
        # Select upper half (5) -> (2 - 4)
        # Break due to alignment -> 2
        a = dist.random_address(2)
        self.assertEqual(a, 2)

        dist.pop_limit()
