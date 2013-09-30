
import Parser
import Benchmark.Hash as Hash
import Benchmark.Heap as Heap
import Benchmark.MM as MM
import Benchmark.QSort as QSort
import Benchmark.Trace as Trace

_benchmark_constructors = dict()

def parse_benchmark(lexer):
   return Parser.parse(lexer, _benchmark_constructors)

def _create_hash(state, args):
   seed = Parser.get_argument(args, 'seed', 7)
   count = Parser.get_argument(args, 'count', 65536)
   return Hash.Hash(seed, count)
_benchmark_constructors['hash'] = _create_hash

def _create_heap(state, args):
   seed = Parser.get_argument(args, 'seed', 7)
   size = Parser.get_argument(args, 'size', 1024)
   return Heap.Heap(seed, size)
_benchmark_constructors['heap'] = _create_heap

def _create_mm(state, args):
   size = Parser.get_argument(args, 'size', 64)
   iterations = Parser.get_argument(args, 'iterations', 1)
   return MM.MM(size, iterations)
_benchmark_constructors['mm'] = _create_mm

def _create_qsort(state, args):
   seed = Parser.get_argument(args, 'seed', 7)
   size = Parser.get_argument(args, 'size', 1024)
   return QSort.QSort(seed, size)
_benchmark_constructors['qsort'] = _create_qsort

def _create_trace(state, args):
   file_name = Parser.get_argument(args, 'file', 'trace.txt')
   return Trace.Trace(file_name)
_benchmark_constructors['trace'] = _create_trace

