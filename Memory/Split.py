
import random
from Memory import Memory
from Join import Join
from Machine import *

def random_split(machine, nxt, rand, cost):
   offset = rand.random_address(machine.word_size)
   bank0 = Join(machine)
   bank1 = Join(machine)
   result = Split(machine, bank0, bank1, nxt, offset)
   bank0.parent = result
   bank1.parent = result
   if result.get_cost() <= cost:
      return result
   else:
      return None

class Split(Memory):

   def __init__(self, machine, bank0, bank1, mem, offset=0):
      self.machine = machine
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

   def done(self):
      return self.mem.done()

   def process(self, access):
      addr = get_address(access)
      size = get_size(access)
      write = is_write(access)
      last = (addr + size - 1) & self.machine.addr_mask
      result = get_cycles(access)
      if addr < self.offset:
         if size <= self.offset:
            temp_size = size
         else:
            temp_size = self.offset - addr
         temp = create_access(write, addr, temp_size)
         result += self.bank0.process(temp)
      if last >= self.offset:
         if addr >= self.offset:
            temp_addr = addr - self.offset
            temp_size = size
         else:
            temp_addr = 0
            temp_size = last - self.offset + 1
         temp = create_access(write, temp_addr, temp_size)
         result += self.bank1.process(temp)
      return result

   def forward(self, access):
      return self.mem.process(access)

