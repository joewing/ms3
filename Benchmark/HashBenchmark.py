
from Benchmark import *
from Machine import *
import random

class HashBenchmark(Benchmark):

   def __init__(self, machine, seed, count):
      self.seed = seed
      self.count = count
      self.size = 4

   def run(self):
      self.rand = random.Random(self.seed)
      for i in range(self.count):
         addr = self.rand.randint(0, self.count - 1) * self.size
         yield create_access(address = addr, size = self.size)

