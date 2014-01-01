
from __future__ import print_function
import sys

from memsim import priorityqueue
from memory.base import send_request


class AccessType(object):
    """Enumeration of possible memory access types."""
    READ = 0         # Read from memory
    WRITE = 1        # Write to memory
    IDLE = 2         # Idle
    CONSUME = 3     # Consume a value from an input port
    PRODUCE = 4     # Produce a value on an output port
    END = 5          # Produce a value indicating the end of a stream


class Process(object):
    """Class to represent processes that perform memory accesses."""

    def __init__(self, benchmark):
        """Initialize a process.
            benchmark is the Benchmark to generate the memory accesses.
        """
        self.mem = None
        self.benchmark = benchmark
        self.machine = None
        self.waiting = -1
        self.delay = False

    def has_delay(self):
        return self.delay

    def reset(self, machine, mem, offset, directory):
        """Reset this process for the next simulation.
            machine is the MachineType to use.
            mem is the memory subsystem.
            offset is the address offset for this process.
            directory is the directory containing trace data.
        """
        self.mem = mem
        self.machine = machine
        self.benchmark.reset(offset, directory)
        self.generator = self.benchmark.run()
        self.mem.reset(machine)

    def done(self):
        """Get the final simulation time for this process.
            Note that this returns an absolute time.
        """
        return self.mem.done()

    def _process(self, write, addr, size):

        # Perform the first part of the access.
        result = 0
        offset = addr & self.machine.word_mask
        if offset:
            first_size = self.machine.word_size - offset
            result = self.mem.process(result, write, addr, first_size)
            addr += first_size
            size -= first_size

        # Perform remaining accesses.
        while size > 0:
            current_size = min(size, self.machine.word_size)
            result = self.mem.process(result, write, addr, current_size)
            addr += current_size
            size -= current_size

        return result

    def step(self):
        """Peform the next event.
            This returns the amount of time used (a delta).
            If the result is negative, then this process is waiting
            to consume an input.
        """

        # Don't continue if we are waiting to consume an input.
        if self.waiting >= 0:
            if self.machine.consume(self.waiting):
                self.waiting = -1
                return -1

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
            self.machine.produce(addr)
            return 0
        elif at == AccessType.CONSUME:
            self.delay = True
            if not self.machine.consume(addr):
                self.waiting = addr
            return 0
        elif at == AccessType.END:
            self.machine.end(addr)
            return 0
        else:
            assert(False)
            return 0


class ProcessList(object):
    """Class to schedule a list of processes on a machine."""

    def __init__(self, machine, processes, directory):
        """Initialize the process list.
            machine is the MachineType instance to use.
            processes is a list of Process objects.
            directory is the directory containing trace data.
        """
        self.heap = priorityqueue.PriorityQueue()
        self.machine = machine
        self.processes = processes
        self.directory = directory

    def has_delay(self):
        """Determine if there are blocking operations.
            This is used to determine if prefetching is useful.
        """
        return any(map(lambda p: p.has_delay(), self.processes))

    def run(self, ml):
        """Run a simulation.
            ml is the MemoryList describing the memories to use.
        """
        # Reset to prepare for the simulation.
        self.machine.reset()
        for i in xrange(len(self.processes)):
            p = self.processes[i]
            p.reset(self.machine, ml.memories[i], self.machine.flip(i),
                    self.directory)
            self.heap.push(0, p)

        # Run the simulation until there are no more events to process.
        while not self.heap.empty():
            self.machine.time = max(self.machine.time, self.heap.key())
            p = self.heap.value()
            self.heap.pop()
            try:
                delta = p.step()
                if delta >= 0:
                    self.heap.push(self.machine.time + delta, p)
                elif not self.heap.empty():
                    self.heap.push(self.machine.time + self.heap.key(), p)
                else:
                    print('ERROR: wait to consume without active producers')
                    sys.exit(-1)
            except StopIteration:
                pass

        # Take into account any leftover time.
        for p in self.processes:
            t = p.done()
            if t > self.machine.time:
                self.machine.time = t

        return self.machine.time


def evaluate(mod, ml, directory):
    """Evaluate the specified memory."""
    processes = []
    for i in xrange(len(mod.benchmarks)):
        processes.append(Process(mod.benchmarks[i]))
    pl = ProcessList(mod.machine, processes, directory)
    return pl.run(ml), ml.get_cost()
