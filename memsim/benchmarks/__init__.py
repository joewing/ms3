from .base import Benchmark, parse_benchmark
assert Benchmark
assert parse_benchmark

# Import all benchmarks to register them with the parser.
from . import cholesky
assert cholesky

from . import hash
assert hash

from . import heap
assert heap

from . import maze
assert maze

from . import mm
assert mm

from . import pca
assert pca

from . import qsort
assert qsort

from . import trace
assert trace
