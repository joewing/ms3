from base import Memory, MemoryList, parse_memory, parse_memory_list
assert Memory
assert MemoryList
assert parse_memory
assert parse_memory_list

# Import components to register them with the parser.
import cache
assert cache

import dram
assert dram

import join
assert join

import offset
assert offset

import option
assert option

import prefetch
assert prefetch

import ram
assert ram

import shift
assert shift

import split
assert split

import spm
assert spm

import xor
assert xor
