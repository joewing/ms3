
import Lexer
import Parser
import Benchmark.Hash as Hash
import Benchmark.MM as MM

_benchmark_constructors = dict()

def parse_benchmark(lexer):
   return Parser.parse(lexer, _benchmark_constructors)

def _create_hash(state, args):
   seed = Parser.get_argument(args, 'seed', 7)
   count = Parser.get_argument(args, 'count', 65536)
   return Hash.Hash(seed, count)
_benchmark_constructors['hash'] = _create_hash

def _create_mm(state, args):
   size = Parser.get_argument(args, 'size', 64)
   iterations = Parser.get_argument(args, 'iterations', 1)
   return MM.MM(size, iterations)
_benchmark_constructors['mm'] = _create_mm

