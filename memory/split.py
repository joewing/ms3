
import base
import parser

def random_split(machine, nxt, rand, cost):
   offset = rand.random_address(machine.word_size)
   bank0 = base.Join(0)
   bank1 = base.Join(1)
   result = Split(bank0, bank1, nxt, offset)
   if result.get_cost() <= cost:
      return result
   else:
      return None

class Split(base.Memory):

   def __init__(self, bank0, bank1, mem, offset):
      self.bank0 = bank0
      self.bank1 = bank1
      self.mem = mem
      self.offset = offset
      base.set_parent(bank0, self)
      base.set_parent(bank1, self)

   def __str__(self):
      result  = "(split "
      result += "(offset " + str(self.offset) + ")"
      result += "(bank0 " + str(self.bank0) + ")"
      result += "(bank1 " + str(self.bank1) + ")"
      result += "(memory " + str(self.mem) + ")"
      result += ")"
      return result

   def get_next(self):
      return self.mem

   def set_next(self, n):
      self.mem = n

   def get_banks(self):
      return [self.bank0, self.bank1]

   def set_bank(self, i, b):
      assert(i >= 0 and i <= 1)
      if i == 0:
         self.bank0 = b
      else:
         self.bank1 = b

   def permute(self, rand, max_cost):
      self.offset = rand.random_address(self.machine.word_size)
      return True

   def simplify(self):
      self.bank0 = self.bank0.simplify()
      self.bank1 = self.bank1.simplify()
      self.mem = self.mem.simplify()
      if isinstance(self.bank0, base.Join) and \
         isinstance(self.bank1, base.Join):
         return self.mem
      return self

   def done(self):
      return self.mem.done()

   def process(self, start, write, addr, size):
      mask = self.machine.addr_mask
      last = (addr + size - 1) & mask
      result = start
      if addr > last:
         result = self._do_process(result, addr, mask - addr + 1, write)
         result = self._do_process(result, 0, last + 1, write)
      else:
         result = self._do_process(result, addr, size, write)
      return result

   def _do_process(self, start, addr, size, write):
      last = (addr + size - 1) & self.machine.addr_mask
      result = start
      if addr < self.offset:
         if last <= self.offset:
            temp_size = size
         else:
            temp_size = self.offset - addr
         result = self.bank0.process(result, write, addr, temp_size)
      if last >= self.offset:
         if addr >= self.offset:
            temp_addr = addr - self.offset
            temp_size = size
         else:
            temp_addr = 0
            temp_size = last - self.offset + 1
         result = self.bank1.process(result, write, temp_addr, temp_size)
      return result

   def forward(self, index, start, write, addr, size):
      if index == 1:
         addr = (addr + self.offset) & self.machine.addr_mask
         return self.mem.process(start, write, addr, size)
      else:
         return self.mem.process(start, write, addr, size)

def _create_split(lexer, args):
   offset = parser.get_argument(lexer, args, 'offset', 0)
   mem = parser.get_argument(lexer, args, 'memory')
   bank0 = parser.get_argument(lexer, args, 'bank0')
   bank1 = parser.get_argument(lexer, args, 'bank1')
   return Split(bank0, bank1, mem, offset)
base.constructors['split'] = _create_split

