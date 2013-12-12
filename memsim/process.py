
from __future__ import print_function
import sys

from memsim import memory, priorityqueue


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

    def __init__(self, dist, benchmark):
        """Initialize a process.
            dist is the Distribution to use.
            benchmark is the Benchmark to generate the memory accesses.
        """
        self.mem = None
        self.dist = dist
        self.benchmark = benchmark
        self.machine = None
        self.waiting = -1
        self.delay = False

    def has_delay(self):
        return self.delay

    def reset(self, machine, mem, offset, directory, on, skip):
        """Reset this process for the next simulation.
            machine is the MachineType to use.
            mem is the memory subsystem.
            offset is the address offset for this process.
            directory is the directory containing trace data.
            on is the number of items to process each segment.
            skip is the number of items to skip each segment.
        """
        self.mem = mem
        self.machine = machine
        self.benchmark.reset(offset, directory, on, skip)
        self.generator = self.benchmark.run()
        self.mem.reset(machine)

    def done(self):
        """Get the final simulation time for this process.
            Note that this returns an absolute time.
        """
        return self.mem.done()

    def skip(self, n):
        """Skip the next n events."""
        if self.waiting >= 0:
            self.machine.reset_port(self.waiting)
        if not self.benchmark.skip(n):
            for _ in xrange(n):
                next(self.generator)

    def step(self, first):
        """Peform the next event.
            first is set to True on the first execution.
            This returns the amount of time used (a delta).
            If the result is negative, then this process is waiting
            to consume an input.
        """

        # Don't continue if we are waiting to consume an input.
        if self.waiting >= 0:
            if self.machine.consume(self.waiting):
                self.waiting = -1
                return -1

        # Get the next access and register it with the distribution.
        at, addr, size = next(self.generator)
        if first:
            if addr > self.machine.addr_mask:
                print("ERROR: address out of range")
                sys.exit(-1)
            self.dist.insert_range(addr, size)

        # Perform the access.
        if at == AccessType.READ:
            return self.mem.process(0, False, addr, size)
        elif at == AccessType.WRITE:
            return self.mem.process(0, True, addr, size)
        elif at == AccessType.IDLE:
            self.delay = self.delay or (addr > 0)
            return addr
        elif at == AccessType.PRODUCE:
            self.machine.produce(addr)
        elif at == AccessType.CONSUME:
            self.delay = True
            if not self.machine.consume(addr):
                self.waiting = addr
        elif at == AccessType.END:
            self.machine.end(addr)
        else:
            assert(False)
        return 0


class ProcessList(object):
    """Class to schedule a list of processes on a machine."""

    def __init__(self, first, machine, processes, directory, on, skip):
        """Initialize the process list.
            machine is the MachineType instance to use.
            processes is a list of Process objects.
            directory is the directory containing trace data.
            on is the number of access to process per segment.
            skip is the number of access to skip per segment.
        """
        self.heap = priorityqueue.PriorityQueue()
        self.machine = machine
        self.processes = processes
        self.first = first
        self.on = on
        self.skip = skip
        self.trace_length = 0
        self.directory = directory

    def has_delay(self):
        """Determine if there are blocking operations.
            This is used to determine if prefetching is useful.
        """
        return any(map(lambda p: p.has_delay(), self.processes))

    def run(self, ml, limit):
        """Run a simulation.
            ml is the MemoryList describing the memories to use.
        """
        # Reset to prepare for the simulation.
        self.machine.reset()
        for i in xrange(len(self.processes)):
            p = self.processes[i]
            p.reset(self.machine, ml.memories[i], self.machine.flip(i),
                    self.directory, self.on, self.skip)
            self.heap.push(0, p)

        # Run the simulation until there are no more events to process.
        current_length = 0
        count = 0
        while not self.heap.empty():
            self.machine.time = max(self.machine.time, self.heap.key())
            p = self.heap.value()
            self.heap.pop()
            try:
                delta = p.step(self.first)
                if delta >= 0:
                    self.heap.push(self.machine.time + delta, p)
                elif not self.heap.empty():
                    self.heap.push(self.machine.time + self.heap.key(), p)
                else:
                    print("ERROR: wait to consume without active producers")
                    sys.exit(-1)
            except StopIteration:
                pass
            current_length += 1
            count += 1
            if count == self.on and self.skip > 0:
                ps = self.heap.get_values()
                self.heap.reset()
                for p in ps:
                    try:
                        p.skip(self.skip)
                        self.heap.push(0, p)
                    except StopIteration:
                        pass
                count = 0
            if current_length > self.trace_length:
                self.trace_length = current_length
            elif self.machine.time > limit and limit > 0:
                percent = 100.0 * float(current_length) / self.trace_length
                print('Prune (' + str(percent) + '%)')
                multiplier = float(self.trace_length) / current_length
                self.machine.time *= multiplier
                break

        # Take into account any leftover time.
        for p in self.processes:
            t = p.done()
            if t > self.machine.time:
                self.machine.time = t

        # No longer the first execution.
        if self.first:
            self.first = False

        return self.machine.time


def evaluate(m, directory):
    """Evaluate the specified model."""
    distributions = []
    processes = []
    memories = []
    for i in xrange(len(m.benchmarks)):
        distributions.append(None)
        processes.append(Process(None, m.benchmarks[i]))
        memories.append(m.memory)
    pl = ProcessList(False, m.machine, processes, directory, m.on, m.skip)
    ml = memory.MemoryList(memories, distributions)
    return pl.run(ml, 0), ml.get_cost()
