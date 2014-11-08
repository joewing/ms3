from __future__ import print_function
from subprocess import Popen, PIPE
from os.path import isfile
import re
import sys

from memsim import priorityqueue, util
from memsim.machine import GoalType
from memsim.model import Model
from memsim.fifostats import FIFOStats
from .proc import Process


def _parse_fifo_data(data):
    expr = re.compile(r'\[(.*)\] \[(.*)\]')
    m = expr.search(data)
    g1, g2 = m.group(1).split(' '), m.group(2).split(' ')
    pdata = [int(i) for i in g1 if len(i) > 0]
    cdata = [int(i) for i in g2 if len(i) > 0]
    return pdata, cdata


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
        self.ml = None

    def add_benchmark(self, benchmark):
        """Create a process for the specified benchmark."""
        proc = Process(self, benchmark, self.directory)
        self.processes.append(proc)

    def reset(self, ml):
        self.ml = ml
        self.machine.reset()
        for f in self.ml.all_fifos():
            f.reset(self.machine)
        for p in self.processes:
            index = p.benchmark.index
            mem = ml.get_subsystem(index)
            p.reset(self.machine, mem)
            self.heap.push(0, p)

    def fastsim(self, ml, subsystem):

        # Check if fastsim is available.
        cmd = 'fastsim/fastsim'
        if not isfile(cmd):
            print('{} not found'.format(cmd))
            assert(False)

        # Run the simulation.
        total = -1
        writes = -1
        energy = -1
        expr = re.compile(r'([a-z]+)([0-9]*) (.+)$')
        mod = Model()
        mod.memory = ml
        mod.machine = self.machine
        mod.benchmarks = [p.benchmark for p in self.processes]
        args = [cmd, '-d', self.directory, '-s', str(subsystem)]
        p = Popen(args, stdin=PIPE, stdout=PIPE)
        result, _ = p.communicate(input=str(mod))

        # Parse the results.
        fifo_stats = FIFOStats()
        for line in result.decode(sys.stdout.encoding).split('\n'):
            for m in expr.finditer(line):
                name = m.group(1)
                index = 0
                if len(m.group(2)) > 0:
                    index = int(m.group(2))
                result = m.group(3)
                if name == 'fifo':
                    score, fifo_data = result.split(' ', 1)
                    pdata, cdata = _parse_fifo_data(fifo_data)
                    mem = ml.get_fifo(index)
                    mem.score = score
                    fifo_stats.update(index, pdata, cdata)
                elif name == 'subsystem':
                    mem = ml.get_subsystem(index)
                    mem.score = int(result)
                elif name == 'total':
                    total = int(result)
                elif name == 'writes':
                    writes = int(result)
                elif name == 'energy':
                    energy = int(float(result))
                else:
                    print(name)
                    assert(False)
        if total < 0:
            print('subsystem: {}'.format(subsystem))
            print('model: {}'.format(mod))
        assert(total >= 0)
        assert(energy >= 0)
        assert(writes >= 0)
        if self.machine.goal == GoalType.ACCESS_TIME:
            return total, fifo_stats
        elif self.machine.goal == GoalType.WRITES:
            return writes, fifo_stats
        elif self.machine.goal == GoalType.ENERGY:
            return energy, fifo_stats
        else:
            assert(False)

    def run(self, ml, subsystem):
        """Run a simulation.

        Argument:
            ml: The MemoryList describing the memories to use.
        """
        self.reset(ml)
        return self.fastsim(ml, subsystem)
