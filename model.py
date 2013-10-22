
import benchmarks
import lex
import machine
import memory
import parser

class Model:

   def __init__(self):
      self.machine = machine.MachineType()
      self.memory = None
      self.benchmarks = []
      self.skip = 0
      self.on = 1000
      self.seed = 7

   def __str__(self):
      result  = '(machine ' + str(self.machine) + ')'
      if self.skip > 0:
         result += '(skip ' + str(self.skip) + ')'
         result += '(on ' + str(self.on) + ')'
      result += '(seed ' + str(self.seed) + ')'
      result += '(memory ' + str(self.memory) + ')'
      result += '(benchmarks '
      for b in self.benchmarks:
         result += str(b)
      result += ')'
      return result

def parse_model(lexer, model = Model()):
   while lexer.get_type() != lex.TOKEN_EOF:
      lexer.match(lex.TOKEN_OPEN)
      name = lexer.get_value()
      lexer.match(lex.TOKEN_LITERAL)
      if name == 'machine':
         model.machine = _parse_machine(lexer)
      elif name == 'memory':
         model.memory = memory.parse_memory(lexer)
      elif name == 'benchmarks':
         model.benchmarks = _parse_benchmarks(lexer)
      elif name == 'include':
         value = lexer.get_value()
         lexer.match(lex.TOKEN_LITERAL)
         parse_model(lex.Lexer(open(value, 'r')), model)
      elif name == 'skip':
         model.skip = _parse_int(lexer)
      elif name == 'on':
         model.on = _parse_int(lexer)
      elif name == 'seed':
         model.seed = _parse_int(lexer)
      else:
         lex.ParseError(lexer, "invalid top-level component: " + name)
      lexer.match(lex.TOKEN_CLOSE)
   return model

def _parse_int(lexer):
   value = lexer.get_value()
   lexer.match(lex.TOKEN_LITERAL)
   return int(value)

def _parse_machine(lexer):
   args = parser.parse_arguments(lexer)
   word_size = parser.get_argument(lexer, args, 'word_size', 8)
   addr_bits = parser.get_argument(lexer, args, 'addr_bits', 32)
   frequency = parser.get_argument(lexer, args, 'frequency', 1e9)
   technology = parser.get_argument(lexer, args, 'technology', 0.045)
   part = parser.get_argument(lexer, args, 'part', 'xc7v585t')
   max_path = parser.get_argument(lexer, args, 'max_path', 64)
   max_cost = parser.get_argument(lexer, args, 'max_cost', 10000)
   tstr = parser.get_argument(lexer, args, 'target', 'simple')
   target = machine.parse_target(tstr)
   if target == None:
      lex.ParseError(lexer, "invalid target: " + tstr)
   return machine.MachineType(target = target,
                              frequency = frequency,
                              word_size = word_size,
                              addr_bits = addr_bits,
                              max_path_length = max_path,
                              max_cost = max_cost,
                              technology = technology,
                              part = part)

def _parse_benchmarks(lexer):
   bms = []
   while lexer.get_type() == lex.TOKEN_OPEN:
      bms.append(benchmarks.parse_benchmark(lexer))
   return bms

