
import benchmarks
import lex
import machine
import memory
import parser

def parse_model(lexer, mach = None, mem = None, bms = []):
   while lexer.get_type() != lex.TOKEN_EOF:
      lexer.match(lex.TOKEN_OPEN)
      name = lexer.get_value()
      lexer.match(lex.TOKEN_LITERAL)
      if name == 'machine':
         mach = _parse_machine(lexer)
      elif name == 'memory':
         mem = memory.parse_memory(lexer)
      elif name == 'benchmarks':
         bms = _parse_benchmarks(lexer)
      elif name == 'include':
         value = lexer.get_value()
         lexer.match(lex.TOKEN_LITERAL)
         mach, mem, bms = parse_model(lex.Lexer(open(value, 'r')),
                                      mach, mem, bms)
      else:
         lex.ParseError(lexer, "invalid top-level component: " + name)
      lexer.match(lex.TOKEN_CLOSE)
   return mach, mem, bms

def _parse_machine(lexer):
   args = parser.parse_arguments(lexer)
   word_size = parser.get_argument(lexer, args, 'word_size', 8)
   addr_bits = parser.get_argument(lexer, args, 'addr_bits', 32)
   frequency = parser.get_argument(lexer, args, 'frequency', 1e9)
   technology = parser.get_argument(lexer, args, 'technology', 0.045)
   max_path = parser.get_argument(lexer, args, 'max_path', 64)
   tstr = parser.get_argument(lexer, args, 'target', 'simple')
   target = machine.parse_target(tstr)
   if target == None:
      lex.ParseError(lexer, "invalid target: " + tstr)
   return machine.MachineType(target = target,
                              frequency = frequency,
                              word_size = word_size,
                              addr_bits = addr_bits,
                              max_path_length = max_path,
                              technology = technology)

def _parse_benchmarks(lexer):
   bms = []
   while lexer.get_type() == lex.TOKEN_OPEN:
      bms.append(benchmarks.parse_benchmark(lexer))
   return bms

