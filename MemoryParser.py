
import Lexer
import Parser
import Memory.Join as Join
import Memory.Offset as Offset
import Memory.SPM as SPM
import Memory.RAM as RAM
import Memory.DRAM as DRAM

_memory_constructors = dict()

def parse_memory(lexer, joins = []):
   return Parser.parse(lexer, _memory_constructors, joins)

def _create_join(joins, args):
   result = Join.Join()
   joins.append(result)
   return result
_memory_constructors['join'] = _create_join

def _create_offset(joins, args):
   offset = Parser.get_argument(args, 'value', 0)
   mem = Parser.get_argument(args, 'memory')
   bank = Parser.get_argument(args, 'bank')
   result = Offset.Offset(bank, mem, offset)
   if len(joins) == 0:
      raise Lexer.ParseError("no join for offset")
   join = joins.pop()
   join.parent = result
   return result
_memory_constructors['offset'] = _create_offset

def _create_spm(joins, args):
   mem = Parser.get_argument(args, 'memory')
   word_count = Parser.get_argument(args, 'word_count', 0)
   latency = Parser.get_argument(args, 'latency', 2)
   return SPM.SPM(mem, word_count, latency)
_memory_constructors['spm'] = _create_spm

def _create_ram(joins, args):
   latency = Parser.get_argument(args, 'latency', 100)
   return RAM.RAM(latency)
_memory_constructors['ram'] = _create_ram

def _create_dram(joins, args):
   multiplier = Parser.get_argument(args, 'multiplier', 1)
   cas_cycles = Parser.get_argument(args, 'cas_cycles', 5)
   rcd_cycles = Parser.get_argument(args, 'rcd_cycles', 5)
   rp_cycles = Parser.get_argument(args, 'rp_cycles', 5)
   wb_cycles = Parser.get_argument(args, 'wb_cycles', 0)
   page_size = Parser.get_argument(args, 'page_size', 1024)
   page_count = Parser.get_argument(args, 'page_count', 16384)
   width = Parser.get_argument(args, 'width', 2)
   burst_size = Parser.get_argument(args, 'burst_size', 1)
   open_page_mode = Parser.get_argument(args, 'open_page_mode', True)
   return DRAM.DRAM(multiplier = multiplier,
                    cas_cycles = cas_cycles,
                    rcd_cycles = rcd_cycles,
                    rp_cycles = rp_cycles,
                    wb_cycles = wb_cycles,
                    page_size = page_size,
                    page_count = page_count,
                    width = width,
                    burst_size = burst_size,
                    open_page_mode = open_page_mode)
_memory_constructors['dram'] = _create_dram

