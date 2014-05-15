from memsim.benchmarks.base import Benchmark, parse_benchmark
assert Benchmark
assert parse_benchmark

# Import all benchmarks to register them with the parser.
from memsim.benchmarks import cholesky
assert cholesky

from memsim.benchmarks import hash
assert hash

from memsim.benchmarks import heap
assert heap

from memsim.benchmarks import maze
assert maze

from memsim.benchmarks import mm
assert mm

from memsim.benchmarks import pca
assert pca

from memsim.benchmarks import qsort
assert qsort

from memsim.benchmarks import trace
assert trace

from memsim.benchmarks import split
assert split
