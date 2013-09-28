
from Memory import Memory
from Join import Join

def random_split(machine, nxt, rand, cost):
   offset = rand.random_address(machine.word_size)
   bank0 = Join(0)
   bank1 = Join(1)
   result = Split(bank0, bank1, nxt, offset)
   bank0.parent = result
   bank1.parent = result
   if result.get_cost() <= cost:
      return result
   else:
      return None

class Split(Memory):

   def __init__(self, bank0, bank1, mem, offset=0):
      self.bank0 = bank0
      self.bank1 = bank1
      self.mem = mem
      self.offset = offset

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
      if isinstance(self.bank0, Join) and isinstance(self.bank1, Join):
         return self.mem
      return self

   def done(self):
      return self.mem.done()

   def process(self, write, addr, size):
      mask = self.machine.addr_mask
      last = (addr + size - 1) & mask
      result = 0
      if addr > last:
         result += self._do_process(addr, mask - addr + 1, write)
         result += self._do_process(0, last + 1, write)
      else:
         result += self._do_process(addr, size, write)
      return result

   def _do_process(self, addr, size, write):
      last = (addr + size - 1) & self.machine.addr_mask
      result = 0
      if addr < self.offset:
         if last <= self.offset:
            temp_size = size
         else:
            temp_size = self.offset - addr
         result += self.bank0.process(write, addr, temp_size)
      if last >= self.offset:
         if addr >= self.offset:
            temp_addr = addr - self.offset
            temp_size = size
         else:
            temp_addr = 0
            temp_size = last - self.offset + 1
         result += self.bank1.process(write, temp_addr, temp_size)
      return result

   def forward(self, index, write, addr, size):
      if index == 1:
         addr = (addr + self.offset) & self.machine.addr_mask
         return self.mem.process(write, addr, size)
      else:
         return self.mem.process(write, addr, size)

