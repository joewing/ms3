
from __future__ import print_function
from memsim.access import AccessType


class Process(object):
    """Class to represent processes that perform memory accesses."""

    def __init__(self, pl, benchmark, directory):
        """Initialize a process.

        Arguments:
            pl:         The ProcessList that owns this process.
            benchmark:  The Benchmark to generate the memory accesses.
            directory:  Data directory.
        """
        self.pl = pl
        self.benchmark = benchmark
        self.directory = directory
        self.mem = None
        self.machine = None
        self.pending_access = None
        self.size = -1

    def total_size(self, directory):
        """Get the total size of the memory subsystem in bytes."""
        if self.size < 0:
            self.size = self.mem.total_size()
            if self.size < 0:
                self.size = self.benchmark.get_size(directory)
                self.mem.set_depth(self.size // self.mem.get_word_size())
        return self.size

    def reset(self, machine, mem):
        """Reset this process for the next simulation.

        Arguments:
            machine is the MachineType to use.
            mem is the memory subsystem.
        """
        self.machine = machine
        self.mem = mem
        self.pending_access = None
        self.benchmark.reset(self.directory)
        self.mem.reset(machine)
