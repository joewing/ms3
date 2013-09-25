
import random

from Memory.Offset import Offset
from Memory.SPM import SPM

class Optimizer:

   def __init__(self, pl,
                max_cost = 1e6,
                iterations = 1000,
                seed = 7,
                permute_only = False):
      self.pl = pl
      self.max_cost = max_cost
      self.iterations = iterations
      self.rand = random.Random(seed)
      self.permute_only = permute_only

   def count(self, mem):
      """Count memory components."""
      if mem == None:
         return 0
      result = 1
      for m in mem.get_components():
         result += self.count(m)
      return result

   def permute(self, mem, index, max_cost):
      """Permute a specific memory component.
         Returns True if successful.
      """
      if index == 0:
         mc = max_cost + mem.get_cost()
         return mem.permute(self.rand, mc)
      components = mem.get_components()
      t = 0
      for i in range(len(components)):
         c = self.count(components[i])
         if index <= t:
            return self.permute(components[i], index - 1 - t, max_cost)
         t += c
      return False

   def modify(self):
      """Modify the memory subsystem."""

      # Loop until we successfully modify the memory subsystem.
      total_cost = self.pl.get_cost()
      stat = False
      while not stat:

         # Select a process to modify.
         pindex = self.rand.randint(0, len(self.pl.processes) - 1)
         p = self.pl.processes[pindex]
         count = self.count(p.mem)

         # Select an action to perform.
         action = self.rand.randint(0, 15)
         if action == 0:   # Insert
            pass
         elif action == 1 and count > 1: # Remove
            pass
         else:             # Permute
            index = self.rand.randint(0, count - 1)
            mc = self.max_cost - total_cost
            stat = self.permute(p.mem, index, mc)

   def optimize(self):
      pass

