from base import Benchmark, parse_benchmark
assert Benchmark
assert parse_benchmark

# Import all benchmarks to register them with the parser.
import cholesky
assert cholesky

import hash
assert hash

import heap
assert heap

import maze
assert maze

import mm
assert mm

import pca
assert pca

import qsort
assert qsort

import trace
assert trace
