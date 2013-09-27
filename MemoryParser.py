
import Lexer
import Parser
import Memory.Join as Join
import Memory.Offset as Offset
import Memory.SPM as SPM
import Memory.RAM as RAM

_memory_constructors = dict()

def parse_memory(machine, lexer, joins = []):
   return Parser.parse(machine, lexer, _memory_constructors, joins)

def _create_join(machine, joins, args):
   result = Join.Join(machine)
   joins.append(result)
   return result
_memory_constructors['join'] = _create_join

def _create_offset(machine, joins, args):
   offset = Parser.get_argument(args, 'value', 0)
   mem = Parser.get_argument(args, 'memory')
   bank = Parser.get_argument(args, 'bank')
   result = Offset.Offset(machine, bank, mem, offset)
   if len(joins) == 0:
      raise Lexer.ParseError("no join for offset")
   join = joins.pop()
   join.parent = result
   return result
_memory_constructors['offset'] = _create_offset

def _create_spm(machine, joins, args):
   mem = Parser.get_argument(args, 'memory')
   word_count = Parser.get_argument(args, 'word_count', 0)
   latency = Parser.get_argument(args, 'latency', 2)
   return SPM.SPM(machine, mem, word_count, latency)
_memory_constructors['spm'] = _create_spm

def _create_ram(machine, joins, args):
   latency = Parser.get_argument(args, 'latency', 100)
   return RAM.RAM(machine, latency)
_memory_constructors['ram'] = _create_ram

