
from Benchmark import Benchmark
import random

class Hash(Benchmark):
   """Benchmark to simulate probes into a hash table."""

   def __init__(self, seed, count, input_port, output_port):
      Benchmark.__init__(self)
      self.seed = seed
      self.count = count
      self.input_port = input_port
      self.output_port = output_port

   def run(self):
      rand = random.Random(self.seed)
      for i in range(self.count):
         if self.input_port >= 0:
            yield self.consume(self.input_port)
         addr = rand.randint(0, self.count - 1)
         yield self.read(addr)
         if self.output_port >= 0:
            yield self.produce(self.output_port)

