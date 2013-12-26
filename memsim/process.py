
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

    def __init__(self, machine, processes, directory):
        """Initialize the process list.
            machine is the MachineType instance to use.
            processes is a list of Process objects.
            directory is the directory containing trace data.
        """
        self.heap = priorityqueue.PriorityQueue()
        self.machine = machine
        self.processes = processes
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
                    self.directory)
            self.heap.push(0, p)

        # Run the simulation until there are no more events to process.
        current_length = 0
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
            current_length += 1
            if current_length > self.trace_length:
                self.trace_length = current_length
            elif self.machine.time > limit and limit > 0:
                percent = 100.0 * float(current_length) / self.trace_length
                print('Prune (' + str(percent) + '%)')
                multiplier = float(self.trace_length) / current_length
                self.machine.time = long(self.machine.time * multiplier)
                break

        # Take into account any leftover time.
        for p in self.processes:
            t = p.done()
            if t > self.machine.time:
                self.machine.time = t

        return self.machine.time


def evaluate(m, directory):
    """Evaluate the specified model."""
    processes = []
    memories = []
    for i in xrange(len(m.benchmarks)):
        processes.append(Process(m.benchmarks[i]))
        memories.append(m.memory)
    pl = ProcessList(m.machine, processes, directory)
    ml = memory.MemoryList(memories)
    return pl.run(ml, 0), ml.get_cost()
