
from Benchmark import Benchmark
import random

class Hash(Benchmark):
   """Benchmark to simulate probes into a hash table."""

   def __init__(self, seed, count):
      self.seed = seed
      self.count = count

   def run(self):
      rand = random.Random(self.seed)
      for i in range(self.count):
         addr = rand.randint(0, self.count - 1)
         yield self.read(addr)

