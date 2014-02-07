
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

    def add_fifo(self, f):
        """Add a simulated FIFO.

        Arguments:
            index: The FIFO index (used in traces).
            f: The FIFO.
        """
        offset = self.address_offset
        self.address_offset += f.total_size()
        f.set_offset(offset)
        self.fifos[f.index] = f

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

    def is_deadlocked(self):
        for p in self.heap.values():
            if p.consume_waiting < 0 and p.produce_waiting < 0:
                return False
            if p.consume_waiting >= 0:
                if not self.fifos[p.consume_waiting].is_empty():
                    return False
            elif p.produce_waiting >= 0:
                if not self.fifos[p.produce_waiting].is_full():
                    return False
        return True

    def run(self, ml):
        """Run a simulation.

        Argument:
            ml: The MemoryList describing the memories to use.
        """
        # Reset to prepare for the simulation.
        memory_index = 0
        self.machine.reset()
        for f in self.fifos.values():
            f.reset(self.machine, ml.memories[memory_index])
            memory_index += 1
        for p in self.processes:
            p.reset(self.machine, ml.memories[memory_index])
            self.heap.push(0, p)
            memory_index += 1

        # Run the simulation until there are no more events to process.
        while not self.heap.empty():
            self.machine.time = max(self.machine.time, self.heap.key())
            p = self.heap.value()
            self.heap.pop()
            try:
                delta = p.step()
                if delta >= 0:
                    next_time = self.machine.time + delta
                elif not self.heap.empty() and not self.is_deadlocked():
                    next_time = self.heap.key() + 100
                else:
                    break
                self.heap.push(next_time, p)
            except StopIteration:
                pass

        # Take into account any leftover time.
        for p in self.processes:
            t = p.done()
            if t > self.machine.time:
                self.machine.time = t

        return self.machine.time
