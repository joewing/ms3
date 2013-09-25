
import random
from Memory import Memory
from Machine import *

class Offset(Memory):

   def __init__(self, machine, mem, offset=0):
      self.mem = mem
      self.offset = offset
      self.addr_mask = machine.addr_mask

   def __str__(self):
      result  = '(offset '
      result += '(value ' + str(self.offset) + ')'
      result += '(memory ' + str(self.mem) + ')'
      result += ')'
      return result

   def get_components(self):
      return [self.mem]

   def permute(self, rand, max_cost):
      self.offset = rand.randint(-64, 64)
      return True

   def reset(self):
      self.mem.reset()

   def done(self):
      return self.mem.done()

   def process(self, access):
      address = (get_address(access) + self.offset) & self.addr_mask
      updated = clone_access(access, address = address)
      return self.mem.process(updated)

