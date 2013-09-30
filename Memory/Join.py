
from Memory import Memory

class Join(Memory):

   parent = None

   def __init__(self, index = 0):
      self.index = index

   def __str__(self):
      return "(join)"

   def process(self, write, addr, size):
      return self.parent.forward(self.index, write, addr, size)

def find_join(mem, parent = None):
   while mem != None:
      if isinstance(mem, Join) and mem.parent is parent:
         break
      mem = mem.get_next()
   return mem

def set_parent(bank, parent):
   join = find_join(bank)
   join.parent = parent

