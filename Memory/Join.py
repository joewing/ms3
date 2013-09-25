
from Memory import Memory

class Join(Memory):

   parent = None

   def __init__(self, machine):
      self.machine = machine

   def __str__(self):
      return "(join)"

   def process(self, access):
      return self.parent.forward(access)

