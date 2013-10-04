
import Parser
import Benchmark.Cholesky as Cholesky
import Benchmark.Hash as Hash
import Benchmark.Heap as Heap
import Benchmark.Maze as Maze
import Benchmark.MM as MM
import Benchmark.QSort as QSort
import Benchmark.Trace as Trace

_benchmark_constructors = dict()

def parse_benchmark(lexer):
   return Parser.parse(lexer, _benchmark_constructors)

def _create_cholesky(state, args):
   size = Parser.get_argument(args, 'size', 128)
   input_port = Parser.get_argument(args, 'input_port', -1)
   output_port = Parser.get_argument(args, 'output_port', -1)
   return Cholesky.Cholesky(size, input_port, output_port)
_benchmark_constructors['cholesky'] = _create_cholesky

def _create_hash(state, args):
   seed = Parser.get_argument(args, 'seed', 7)
   count = Parser.get_argument(args, 'count', 65536)
   input_port = Parser.get_argument(args, 'input_port', -1)
   output_port = Parser.get_argument(args, 'output_port', -1)
   return Hash.Hash(seed, count, input_port, output_port)
_benchmark_constructors['hash'] = _create_hash

def _create_heap(state, args):
   seed = Parser.get_argument(args, 'seed', 7)
   size = Parser.get_argument(args, 'size', 1024)
   input_port = Parser.get_argument(args, 'input_port', -1)
   output_port = Parser.get_argument(args, 'output_port', -1)
   return Heap.Heap(seed, size, input_port, output_port)
_benchmark_constructors['heap'] = _create_heap

def _create_maze(state, args):
   width = Parser.get_argument(args, 'width', 32)
   height = Parser.get_argument(args, 'height', 32)
   seed = Parser.get_argument(args, 'seed', 5)
   return Maze.Maze(width, height, seed)
_benchmark_constructors['maze'] = _create_maze

def _create_mm(state, args):
   size = Parser.get_argument(args, 'size', 64)
   iterations = Parser.get_argument(args, 'iterations', 1)
   input_port = Parser.get_argument(args, 'input_port', -1)
   output_port = Parser.get_argument(args, 'output_port', -1)
   return MM.MM(size = size, iterations = iterations,
                input_port = input_port, output_port = output_port)
_benchmark_constructors['mm'] = _create_mm

def _create_qsort(state, args):
   seed = Parser.get_argument(args, 'seed', 7)
   size = Parser.get_argument(args, 'size', 1024)
   input_port = Parser.get_argument(args, 'input_port', -1)
   output_port = Parser.get_argument(args, 'output_port', -1)
   return QSort.QSort(seed, size, input_port, output_port)
_benchmark_constructors['qsort'] = _create_qsort

def _create_trace(state, args):
   file_name = Parser.get_argument(args, 'file', 'trace.txt')
   return Trace.Trace(file_name)
_benchmark_constructors['trace'] = _create_trace

