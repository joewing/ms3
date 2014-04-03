from __future__ import print_function
from subprocess import Popen, PIPE
from os.path import isfile
import re
import sys

from memsim import priorityqueue, util
from memsim.model import Model
from .proc import Process


class ProcessList(object):
    """Class to schedule a list of processes on a machine."""

    def __init__(self, machine, directory):
        """Initialize the process list.

        Arguments:
            machine:    The MachineType instance to use.
            directory:  The directory containing trace data.
        """
        self.heap = priorityqueue.PriorityQueue()
        self.machine = machine
        self.directory = directory
        self.processes = []
        self.consumers = dict()
        self.producers = dict()
        self.ml = None

    def add_benchmark(self, benchmark):
        """Create a process for the specified benchmark."""
        proc = Process(self, benchmark, self.directory)
        self.processes.append(proc)

    def has_delay(self):
        """Determine if there are blocking operations.

        This is used to determine if prefetching is useful.
        """
        return any(map(lambda p: p.has_delay(), self.processes))

    def produce(self, p, index):
        """Produce a value on the specified FIFO (0 based).

        Returns the access cycle count or -1 if full.
        """
        rc = self.ml.get_fifo(index).produce()
        if rc < 0:
            self.producers[index] = p
        elif index in self.consumers:
            self.heap.push(self.machine.time, self.consumers[index])
            del self.consumers[index]
        return rc

    def consume(self, p, index):
        """Consume a value from the specified FIFO (0 based).

        Returns the access cycle count or -1 if empty.
        """
        rc = self.ml.get_fifo(index).consume()
        if rc < 0:
            self.consumers[index] = p
        elif index in self.producers:
            self.heap.push(self.machine.time, self.producers[index])
            del self.producers[index]
        return rc

    def peek(self, p, index, offset):
        """Peek at a value on the specified FIFO (0 base).

        Returns the access cycle count or -1 if not available.
        """
        rc = self.ml.get_fifo(index).peek(offset)
        if rc < 0:
            self.consumers[index] = p
        elif index in self.producers:
            self.heap.push(self.machine.time, self.producers[index])
            del self.producers[index]
        return rc

    def reset(self, ml):
        self.ml = ml
        self.machine.reset()
        self.producers = dict()
        self.consumers = dict()
        offset = 0
        for f in self.ml.all_fifos():
            offset = util.align(f.get_word_size(), offset)
            f.set_offset(offset)
            f.reset(self.machine)
            offset += f.total_size()
        for p in self.processes:
            index = p.benchmark.index
            mem = ml.get_subsystem(index)
            offset = util.align(mem.get_word_size(), offset)
            mem.set_offset(offset)
            p.reset(self.machine, mem, offset)
            self.heap.push(0, p)
            offset += p.total_size(self.directory)
        assert(offset < (1 << self.machine.addr_bits))

    def fastsim(self, ml):

        # Check if fastsim is available.
        cmd = 'fastsim/fastsim'
        if not isfile(cmd):
            print('{} not found'.format(cmd))
            return -1

        # Run the simulation.
        total = -1
        expr = re.compile(r'([a-z]+)([0-9]*) ([0-9]+)')
        mod = Model()
        mod.memory = ml
        mod.machine = self.machine
        mod.benchmarks = [p.benchmark for p in self.processes]
        p = Popen([cmd, '-d', self.directory], stdin=PIPE, stdout=PIPE)
        result, _ = p.communicate(input=str(mod))

        # Parse the results.
        for line in result.decode(sys.stdout.encoding).split('\n'):
            for m in expr.finditer(line):
                name = m.group(1)
                index = 0
                if len(m.group(2)) > 0:
                    index = int(m.group(2))
                result = int(m.group(3))
                if name == 'fifo':
                    mem = ml.get_fifo(index)
                    mem.score = result
                elif name == 'subsystem':
                    mem = ml.get_subsystem(index)
                    mem.score = result
                elif name == 'total':
                    total = result
                else:
                    print(name)
                    assert(False)
        return total

    def run(self, ml, use_fastsim):
        """Run a simulation.

        Argument:
            ml: The MemoryList describing the memories to use.
        """
        # Reset to prepare for the simulation.
        self.reset(ml)

        # Try to use fastsim.
        if use_fastsim:
            rc = self.fastsim(ml)
            if rc >= 0:
                return rc

        # fastsim not available.
        # Run the simulation until there are no more events to process.
        while not self.heap.empty():
            self.machine.time = max(self.machine.time, self.heap.key())
            p = self.heap.value()
            self.heap.pop()
            try:
                delta = p.step()
                if delta >= 0:
                    next_time = self.machine.time + delta
                    self.heap.push(next_time, p)
            except StopIteration:
                pass

        # Take into account any leftover time.
        for p in self.processes:
            t = p.done()
            if t > self.machine.time:
                self.machine.time = t

        return self.machine.time
