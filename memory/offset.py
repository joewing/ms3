
import base
import parser

def random_offset(machine, nxt, rand, cost):
   if rand.randbool():
      offset = rand.randint(-machine.word_size, machine.word_size)
   else:
      offset = rand.random_address(machine.word_size)
   return Offset(base.Join(), nxt, offset)

class Offset(base.Transform):

   def __init__(self, bank, mem, offset):
      base.Transform.__init__(self, bank, mem)
      self.offset = offset

   def __str__(self):
      result  = '(offset '
      result += '(value ' + str(self.offset) + ')'
      result += '(bank ' + str(self.bank) + ')'
      result += '(memory ' + str(self.mem) + ')'
      result += ')'
      return result

   def is_empty(self):
      return self.offset == 0

   def permute(self, rand, max_cost):
      word_size = self.machine.word_size
      if rand.randbool():
         self.offset = rand.randint(-word_size, word_size)
      else:
         self.offset = rand.random_address(word_size)
      return True

   def push_transform(self, index, rand):
      if index == 0:
         rand.push_transform(lambda a: a + self.offset)
      else:
         rand.push_transform(lambda a: a)

   def pop_transform(self, rand):
      rand.pop_transform()

   def process(self, write, addr, size):
      addr = (addr + self.offset) & self.machine.addr_mask
      return self.bank.process(write, addr, size)

   def forward(self, index, write, addr, size):
      assert(index == 0)
      addr = (addr - self.offset) & self.machine.addr_mask
      return self.mem.process(write, addr, size) 

def _create_offset(args):
   offset = parser.get_argument(args, 'value', 0)
   mem = parser.get_argument(args, 'memory')
   bank = parser.get_argument(args, 'bank')
   return Offset(bank, mem, offset)
base.constructors['offset'] = _create_offset

