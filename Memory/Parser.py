
from Lexer import *
from Join import Join
from Offset import Offset
from SPM import SPM
from RAM import RAM

class Parser:

   joins = []

   def __init__(self, machine, f):
      self.lexer = Lexer(f)
      self.machine = machine
      self.parsers = {
         'join':     self.parse_join,
         'offset':   self.parse_offset,
         'spm':      self.parse_spm,
         'ram':      self.parse_ram
      }

   def parse(self):
      self.lexer.match(TOKEN_OPEN)
      name = self.lexer.get_value()
      if name in self.parsers:
         parser = self.parsers[name]
         self.lexer.match(TOKEN_LITERAL)
         args = self.parse_arguments()
         result = parser(args)
      else:
         raise ParseError("invalid memory: " + self.lexer.get_value())
      self.lexer.match(TOKEN_CLOSE)
      return result

   def parse_arguments(self):
      result = dict()
      while self.lexer.get_type() == TOKEN_OPEN:
         self.lexer.match(TOKEN_OPEN)
         name = self.lexer.get_value()
         self.lexer.match(TOKEN_LITERAL)
         if self.lexer.get_type() == TOKEN_LITERAL:
            value = self.lexer.get_value()
            self.lexer.match(TOKEN_LITERAL)
         else:
            value = self.parse()
         result[name] = value
         self.lexer.match(TOKEN_CLOSE)
      return result

   def get_argument(self, args, name, default = None):
      if name in args:
         value = args[name]
         if isinstance(default, int):
            return int(value)
         elif isinstance(default, float):
            return float(value)
         else:
            return args[name]
      else:
         return default

   def parse_join(self, args):
      result = Join(self.machine)
      self.joins.append(result)
      return result

   def parse_offset(self, args):
      offset = self.get_argument(args, 'value', 0)
      mem = self.get_argument(args, 'memory')
      bank = self.get_argument(args, 'bank')
      result = Offset(self.machine, bank, mem, offset)
      if len(self.joins) == 0:
         raise ParseError("no join for offset")
      join = self.joins.pop()
      join.parent = result
      return result

   def parse_spm(self, args):
      mem = self.get_argument(args, 'memory')
      word_count = self.get_argument(args, 'word_count', 0)
      latency = self.get_argument(args, 'latency', 2)
      return SPM(self.machine, mem, word_count, latency)

   def parse_ram(self, args):
      latency = self.get_argument(args, 'latency', 100)
      return RAM(self.machine, latency)

