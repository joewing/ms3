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

    def reset(self, ml):
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
        self.reset(ml)

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
                elif self.heap.empty() and self._is_deadlocked():
                    break
            except StopIteration:
                pass

        # Take into account any leftover time.
        for p in self.processes:
            t = p.done()
            if t > self.machine.time:
                self.machine.time = t

        return self.machine.time
