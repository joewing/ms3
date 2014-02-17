
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
        self.consume_waiting = -1
        self.produce_waiting = -1
        self.delay = False

    def has_delay(self):
        """Determine if prefetching would be beneficial."""
        return self.delay

    def reset(self, machine, mem, offset):
        """Reset this process for the next simulation.

        Arguments:
            machine is the MachineType to use.
            mem is the memory subsystem.
            offset is the offset for memory accesses
        """
        self.machine = machine
        self.mem = mem
        self.consume_waiting = -1
        self.produce_waiting = -1
        self.benchmark.reset(offset, self.directory)
        self.generator = self.benchmark.run()
        self.mem.reset(machine)

    def done(self):
        """Get the final simulation time for this process.

        This returns an absolute time.
        """
        return self.mem.done()

    def step(self):
        """Execute the next event.

        This returns the amount of time used (a delta).
        This will return -1 if the process is blocked.
        """

        # Check if we're waiting on a FIFO.
        if self.consume_waiting >= 0:
            temp = self.pl.consume(self.consume_waiting)
            if temp >= 0:
                self.consume_waiting = -1
            return temp
        if self.produce_waiting >= 0:
            temp = self.pl.produce(self.produce_waiting)
            if temp >= 0:
                self.produce_waiting = -1
            return temp

        # Perform the access.
        at, addr, size = next(self.generator)
        if at == AccessType.READ:
            return send_request(self.mem, 0, False, addr, size)
        elif at == AccessType.WRITE:
            return send_request(self.mem, 0, True, addr, size)
        elif at == AccessType.IDLE:
            self.delay = self.delay or (addr > 0)
            return addr
        elif at == AccessType.PRODUCE:
            self.delay = True
            temp = self.pl.produce(addr)
            if temp < 0:
                self.produce_waiting = addr
            return temp
        elif at == AccessType.CONSUME:
            self.delay = True
            temp = self.pl.consume(addr)
            if temp < 0:
                self.consume_waiting = addr
            return temp
        elif at == AccessType.END:
            self.machine.end(addr)
            return 0
        else:
            assert(False)
            return 0
