
import Lexer
import Parser
import Benchmark.Hash as Hash
import Benchmark.MM as MM

_benchmark_constructors = dict()

def parse_benchmark(machine, lexer):
   return Parser.parse(machine, lexer, _benchmark_constructors, None)

def _create_hash(machine, state, args):
   seed = Parser.get_argument(args, 'seed', 7)
   count = Parser.get_argument(args, 'count', 65536)
   return Hash.Hash(machine, seed, count)
_benchmark_constructors['hash'] = _create_hash

def _create_mm(machine, state, args):
   size = Parser.get_argument(args, 'size', 64)
   iterations = Parser.get_argument(args, 'iterations', 1)
   return MM.MM(machine, size, iterations)
_benchmark_constructors['mm'] = _create_mm

