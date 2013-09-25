
from Memory import Memory

class Join(Memory):

   def __init__(self, machine, parent):
      self.machine = machine
      self.parent = parent

   def __str__(self):
      return "(join)"

   def process(self, access):
      return self.parent.forward(access)

