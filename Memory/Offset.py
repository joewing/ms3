
import random
from Memory import Memory
from Join import Join
from Machine import *

def random_offset(machine, nxt, rand, cost):
   if rand.randbool():
      offset = rand.randint(-machine.word_size, machine.word_size)
   else:
      offset = rand.random_address(machine.word_size)
   join = Join(machine)
   result = Offset(machine, join, nxt, offset)
   join.parent = result
   return result

class Offset(Memory):

   def __init__(self, machine, bank, mem, offset=0):
      self.machine = machine
      self.bank = bank
      self.mem = mem
      self.offset = offset

   def __str__(self):
      result  = '(offset '
      result += '(value ' + str(self.offset) + ')'
      result += '(bank ' + str(self.bank) + ')'
      result += '(memory ' + str(self.mem) + ')'
      result += ')'
      return result

   def get_next(self):
      return self.mem

   def set_next(self, n):
      self.mem = n

   def get_banks(self):
      return [self.bank]

   def set_bank(self, i, b):
      assert(i == 0)
      self.bank = b

   def permute(self, rand, max_cost):
      word_size = self.machine.word_size
      if rand.randbool():
         self.offset = rand.randint(-word_size, word_size)
      else:
         self.offset = rand.random_address(word_size)
      return True

   def push_transform(self, index, rand):
      assert(index == 0)
      rand.push_transform(lambda a: a + self.offset)

   def pop_transform(self, rand):
      rand.pop_transform()

   def done(self):
      return self.mem.done()

   def process(self, access):
      address = (get_address(access) + self.offset) & self.machine.addr_mask
      updated = clone_access(access, address = address)
      return self.bank.process(updated)

   def forward(self, access):
      address = (get_address(access) - self.offset) & self.machine.addr_mask
      updated = clone_access(access, address = address)
      return self.mem.process(updated)
      

