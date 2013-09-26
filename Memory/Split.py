
import random
from Memory import Memory
from Join import Join
from Machine import *

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
      return [bank0, bank1]

   def set_bank(self, i, b):
      assert(i >= 0 and i <= 1)
      if i == 0:
         self.bank0 = b
      else:
         self.bank1 = b

   def permute(self, rand, max_cost):
      self.offset = rand.rand_address()



