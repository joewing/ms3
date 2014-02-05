
import sys
from memsim import priorityqueue
from .proc import Process
from .fifo import FIFO


class ProcessList(object):
    """Class to schedule a list of processes on a machine."""

    def __init__(self, machine, directory):
        """Initialize the process list.

        Arguments:
            machine: The MachineType instance to use.
            directory: The directory containing trace data.
        """
        self.heap = priorityqueue.PriorityQueue()
        self.machine = machine
        self.processes = []
        self.directory = directory
        self.address_offset = 0
        self.fifos = dict()     # Mapping of FIFO index to FIFO.

    def add_benchmark(self, benchmark, size):
        """Add a process for the specified benchmark.

        Arguments:
            benchmark: The benchmark to add.
            size: The amount of memory to allocate for the benchmark.
        """
        offset = self.address_offset
        self.address_offset += size
        proc = Process(self, benchmark, offset, self.directory)
        self.processes.append(proc)

    def add_fifo(self, index, f):
        """Add a simulated FIFO.

        Arguments:
            index: The FIFO index (used in traces).
            f: The FIFO.
        """
        offset = self.address_offset
        self.address_offset += f.total_size()
        f.set_offset(offset)
        self.fifos[index] = f

    def has_delay(self):
        """Determine if there are blocking operations.

        This is used to determine if prefetching is useful.
        """
        return any(map(lambda p: p.has_delay(), self.processes))

    def produce(self, index):
        """Produce a value on the specified FIFO (0 based).

        Returns the access cycle count or -1 if full.
        """
        return self.fifos[index].produce()

    def consume(self, index):
        """Consume a value from the specified FIFO (0 based).

        Returns the access cycle count or -1 if empty.
        """
        return self.fifos[index].consume()

    def run(self, ml):
        """Run a simulation.

        Argument:
            ml: The MemoryList describing the memories to use.
        """
        # Reset to prepare for the simulation.
        memory_index = 0
        self.machine.reset()
        for p in self.processes:
            mem = ml.memories[memory_index]
            p.reset(self.machine, mem)
            self.heap.push(0, p)
            memory_index += 1
        for f in self.fifos:
            mem = ml.memories[memory_index]
            f.reset(self.machine, mem)
            memory_index += 1

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
