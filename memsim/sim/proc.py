
from __future__ import print_function
from memsim.memory.base import send_request
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
        self.delay = False
        self.size = -1
        self.offset = 0

    def has_delay(self):
        """Determine if prefetching would be beneficial."""
        return self.delay

    def total_size(self, directory):
        """Get the total size of the memory subsystem in bytes."""
        if self.size < 0:
            self.size = self.mem.total_size()
            if self.size < 0:
                self.size = self.benchmark.get_size(directory)
                self.mem.set_depth(self.size // self.mem.get_word_size())
        return self.size

    def reset(self, machine, mem, offset):
        """Reset this process for the next simulation.

        Arguments:
            machine is the MachineType to use.
            mem is the memory subsystem.
            offset is the offset for memory accesses
        """
        self.machine = machine
        self.mem = mem
        self.pending_access = None
        self.offset = offset
        self.benchmark.reset(self.directory)
        self.generator = self.benchmark.run()
        self.mem.reset(machine)

    def done(self):
        """Get the final simulation time for this process.

        This returns an absolute time.
        """
        return self.mem.done()

    def _process(self, access):
        at, addr, size = access
        if at == AccessType.READ:
            return send_request(self.mem, self.offset, 0, False, addr, size)
        elif at == AccessType.WRITE:
            return send_request(self.mem, self.offset, 0, True, addr, size)
        elif at == AccessType.MODIFY:
            temp = send_request(self.mem, self.offset, 0, False, addr, size)
            return send_request(self.mem, self.offset, temp, True, addr, size)
        elif at == AccessType.IDLE:
            self.delay = self.delay or (addr > 0)
            return addr
        elif at == AccessType.PRODUCE:
            self.delay = True
            temp = self.pl.produce(self, addr)
            if temp < 0:
                self.pending_access = access
            return temp
        elif at == AccessType.CONSUME:
            self.delay = True
            temp = self.pl.consume(self, addr)
            if temp < 0:
                self.pending_access = access
            return temp
        elif at == AccessType.PEEK:
            self.delay = True
            temp = self.pl.peek(self, addr, size)
            if temp < 0:
                self.pending_access = access
            return temp
        elif at == AccessType.INPUT:
            self.delay = True
            temp = self.pl.consume(self, addr)
            if temp < 0:
                temp = self.pl.consume(self, size)
            return max(0, temp)
        elif at == AccessType.OUTPUT:
            self.delay = True
            temp = self.pl.produce(self, addr)
            if temp < 0:
                temp = self.pl.produce(self, size)
            return max(0, temp)
        elif at == AccessType.END:
            self.machine.end(addr)
            return 0
        else:
            assert(False)
            return 0

    def step(self):
        """Execute the next event.

        This returns the amount of time used (a delta).
        This will return -1 if the process is blocked.
        """

        if self.pending_access is not None:
            # Process a pending process.
            access = self.pending_access
            self.pending_access = None
            return self._process(access)
        else:
            # Process the next access.
            return self._process(next(self.generator))
