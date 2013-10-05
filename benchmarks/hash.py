
import random
import parser
import base

class Hash(base.Benchmark):
   """Benchmark to simulate probes into a hash table."""

   def __init__(self, seed, count, input_port, output_port):
      base.Benchmark.__init__(self)
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

def _create_hash(args):
   seed = parser.get_argument(args, 'seed', 7)
   count = parser.get_argument(args, 'count', 65536)
   input_port = parser.get_argument(args, 'input_port', -1)
   output_port = parser.get_argument(args, 'output_port', -1)
   return Hash(seed, count, input_port, output_port)
base.constructors['hash'] = _create_hash

