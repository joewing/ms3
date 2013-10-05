
import base
import lex
import parser

class Option(base.Memory):

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
      base.Memory.reset(self, machine)
      self.options[self.index].reset(machine)

   def permute(self, rand, max_cost):
      self.index = rand.randint(0, len(self.options) - 1)
      return True

   def process(self, write, addr, size):
      return self.options[self.index].process(write, addr, size)

   def done(self):
      return self.options[self.index].done()

def _create_option(args):
   result = Option()
   i = 0
   while ('memory' + str(i)) in args:
      result.add_option(args['memory' + str(i)])
      i += 1
   if i == 0:
      raise lex.ParseError("no memories in option")
   return result
base.constructors['option'] = _create_option

