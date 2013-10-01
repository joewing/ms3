
from Memory import Memory

class Option(Memory):

   def __init__(self):
      self.options = []
      self.index = 0

   def __str__(self):
      return str(self.options[self.index])

   def add_option(self, m):
      self.options.append(m)

   def get_cost(self):
      return self.options[self.index].get_cost()

   def reset(self, machine):
      Memory.reset(self, machine)
      self.options[self.index].reset(machine)

   def permute(self, rand, max_cost):
      self.index = rand.randint(0, len(self.options) - 1)
      return True

   def process(self, write, addr, size):
      return self.options[self.index].process(write, addr, size)

   def done(self):
      return self.options[self.index].done()

