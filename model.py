
import benchmarks
import lex
import machine
import memory
import parser

def parse_model(lexer):
   mach = None
   mem = None
   bms = []
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
      else:
         lex.ParseError("invalid top-level component: " + name)
      lexer.match(lex.TOKEN_CLOSE)
   return mach, mem, bms

def _parse_machine(lexer):
   args = parser.parse_arguments(lexer)
   word_size = parser.get_argument(args, 'word_size', 8)
   addr_bits = parser.get_argument(args, 'addr_bits', 32)
   tstr = parser.get_argument(args, 'target', 'simple')
   target = machine.parse_target(tstr)
   if target == None:
      lex.ParseError("invalid target: " + tstr)
   return machine.MachineType(target, word_size, addr_bits)

def _parse_benchmarks(lexer):
   bms = []
   while lexer.get_type() == lex.TOKEN_OPEN:
      bms.append(benchmarks.parse_benchmark(lexer))
   return bms

