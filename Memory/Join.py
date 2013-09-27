
from Memory import Memory

class Join(Memory):

   parent = None

   def __init__(self, machine, index = 0):
      self.machine = machine
      self.index = index

   def __str__(self):
      return "(join)"

   def process(self, write, addr, size):
      return self.parent.forward(self.index, write, addr, size)

