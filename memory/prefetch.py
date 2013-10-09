

import base
import parser

def random_prefetch(machine, nxt, rand, cost):
   stride = machine.word_size * rand.randint(-8, 8)
   result = Prefetch(nxt, stride)
   result.reset(machine)
   if result.get_cost() <= cost:
      return result
   else:
      return None

class Prefetch(base.Container):

   def __init__(self, mem, stride):
      base.Container.__init__(self, mem)
      self.stride = stride
      self.pending = 0

   def __str__(self):
      result  = "(prefetch "
      result += "(stride " + str(self.stride) + ")"
      result += "(memory " + str(self.mem) + ")"
      result += ")"
      return result

   def permute(self, rand, max_cost):
      self.stride = self.machine.word_size * rand.randint(-8, 8)
      return True

   def reset(self, m):
      base.Container.reset(self, m)
      self.pending = 0

   def done(self):
      return self.pending

   def process(self, write, addr, size):
      result = self.mem.process(write, addr, size) + self.pending
      if not write:
         temp = (addr + self.stride) & self.machine.addr_mask
         self.pending = self.mem.process(write, temp, 1)
      return result

def _create_prefetch(args):
   mem = parser.get_argument(args, 'mem')
   stride = parser.get_argument(args, 'stride')
   return Prefetch(mem, stride)
base.constructors['prefetch'] = _create_prefetch

