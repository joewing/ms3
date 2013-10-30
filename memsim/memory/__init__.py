from .base import Memory, MemoryList, parse_memory, parse_memory_list
assert Memory
assert MemoryList
assert parse_memory
assert parse_memory_list

# Import components to register them with the parser.
from . import cache
assert cache

from . import dram
assert dram

from . import join
assert join

from . import offset
assert offset

from . import option
assert option

from . import prefetch
assert prefetch

from . import ram
assert ram

from . import shift
assert shift

from . import split
assert split

from . import spm
assert spm

from . import xor
assert xor
