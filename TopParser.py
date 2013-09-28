
import Lexer
import BenchmarkParser
import MemoryParser
import Parser
from Machine import MachineType

def parse(lexer):
   machine = None
   memory = None
   benchmarks = []
   while lexer.get_type() != Lexer.TOKEN_EOF:
      lexer.match(Lexer.TOKEN_OPEN)
      name = lexer.get_value()
      lexer.match(Lexer.TOKEN_LITERAL)
      if name == 'machine':
         machine = _parse_machine(lexer)
      elif name == 'memory':
         memory = MemoryParser.parse_memory(lexer)
      elif name == 'benchmarks':
         benchmarks = _parse_benchmarks(lexer)
      else:
         Lexer.ParseError("invalid top-level component: " + name)
      lexer.match(Lexer.TOKEN_CLOSE)
   return machine, memory, benchmarks

def _parse_machine(lexer):
   args = Parser.parse_arguments(lexer)
   word_size = Parser.get_argument(args, 'word_size', 8)
   addr_bits = Parser.get_argument(args, 'addr_bits', 32)
   return MachineType(word_size, addr_bits)

def _parse_benchmarks(lexer):
   benchmarks = []
   while lexer.get_type() == Lexer.TOKEN_OPEN:
      benchmarks.append(BenchmarkParser.parse_benchmark(lexer))
   return benchmarks

