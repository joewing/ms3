
import Lexer
import BenchmarkParser
import MemoryParser
from Machine import MachineType

def parse(lexer):
   machine = _parse_machine(lexer)
   memory = MemoryParser.parse_memory(lexer)
   benchmarks = _parse_benchmarks(lexer)
   return (machine, memory, benchmarks)

def _parse_machine(lexer):
   lexer.match(Lexer.TOKEN_OPEN)
   name = lexer.get_value()
   lexer.match(Lexer.TOKEN_LITERAL)
   if name != 'machine':
      Lexer.ParseError("error: got '" + name + "' expected 'machine'")
   word_size = 8
   addr_bits = 32
   while lexer.get_type() == Lexer.TOKEN_OPEN:
      lexer.match(Lexer.TOKEN_OPEN)
      name = lexer.get_value()
      lexer.match(Lexer.TOKEN_LITERAL)
      value = lexer.get_value()
      lexer.match(Lexer.TOKEN_LITERAL)
      if name == 'word_size':
         word_size = int(value)
      elif name == 'addr_bits':
         addr_bits = int(value)
      else:
         raise Lexer.ParseError("invalid machine option: '" + name + "'") 
      lexer.match(Lexer.TOKEN_CLOSE)
   lexer.match(Lexer.TOKEN_CLOSE)
   return MachineType(word_size, addr_bits)

def _parse_benchmarks(lexer):
   benchmarks = []
   lexer.match(Lexer.TOKEN_OPEN)
   name = lexer.get_value()
   lexer.match(Lexer.TOKEN_LITERAL)
   if name != 'benchmarks':
      Lexer.ParseError("error: got '" + name + "' expected 'benchmarks'")
   while lexer.get_type() == Lexer.TOKEN_OPEN:
      benchmarks.append(BenchmarkParser.parse_benchmark(lexer))
   lexer.match(Lexer.TOKEN_CLOSE)
   return benchmarks

