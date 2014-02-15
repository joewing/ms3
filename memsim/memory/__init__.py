from memsim.memory.base import Memory, parse_memory
assert Memory
assert parse_memory

from memsim.memory.memlist import MemoryList, parse_memory_list
assert MemoryList
assert parse_memory_list

# Import components to register them with the parser.
from memsim.memory import cache
assert cache

from memsim.memory import dram
assert dram

from memsim.memory import join
assert join

from memsim.memory import offset
assert offset

from memsim.memory import option
assert option

from memsim.memory import prefetch
assert prefetch

from memsim.memory import ram
assert ram

from memsim.memory import shift
assert shift

from memsim.memory import split
assert split

from memsim.memory import spm
assert spm

from memsim.memory import xor
assert xor

from memsim.memory import trace
assert trace
