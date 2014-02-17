from memsim import priorityqueue
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

    def produce(self, index):
        """Produce a value on the specified FIFO (0 based).

        Returns the access cycle count or -1 if full.
        """
        fifo = self.ml.get_fifo(index)
        return fifo.produce()

    def consume(self, index):
        """Consume a value from the specified FIFO (0 based).

        Returns the access cycle count or -1 if empty.
        """
        fifo = self.ml.get_fifo(index)
        return fifo.consume()

    def _is_deadlocked(self):
        for p in self.heap.values():
            if p.consume_waiting < 0 and p.produce_waiting < 0:
                return False
            if p.consume_waiting >= 0:
                fifo = self.ml.get_fifo(p.consume_waiting)
                if not fifo.is_empty():
                    return False
            elif p.produce_waiting >= 0:
                fifo = self.ml.get_fifo(p.produce_waiting)
                if not fifo.is_full():
                    return False
        return True

    def _reset(self, ml):
        self.ml = ml
        offset = 0
        self.machine.reset()
        for f in self.ml.all_fifos():
            offset = self.machine.align(offset)
            f.set_offset(offset)
            f.reset(self.machine)
            offset += f.total_size()
        for p in self.processes:
            offset = self.machine.align(offset)
            index = p.benchmark.index
            mem = ml.get_subsystem(index)
            p.reset(self.machine, mem, offset)
            self.heap.push(0, p)
            offset += p.benchmark.get_size()
        assert(offset < (1 << self.machine.addr_bits))

    def run(self, ml):
        """Run a simulation.

        Argument:
            ml: The MemoryList describing the memories to use.
        """
        # Reset to prepare for the simulation.
        self._reset(ml)

        # Run the simulation until there are no more events to process.
        while not self.heap.empty():
            self.machine.time = max(self.machine.time, self.heap.key())
            p = self.heap.value()
            self.heap.pop()
            try:
                delta = p.step()
                if delta >= 0:
                    next_time = self.machine.time + delta
                elif not self.heap.empty() and not self._is_deadlocked():
                    next_time = self.heap.key() + 1
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
