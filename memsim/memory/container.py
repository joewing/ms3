
from memsim.memory import base


class Container(base.Memory):
    """A memory containing another memory (caches, etc.)."""

    def __init__(self, mem):
        base.Memory.__init__(self)
        self.mem = mem

    def get_next(self):
        return self.mem

    def set_next(self, n):
        self.mem = n

    def done(self):
        return self.mem.done()

    def get_path_length(self):
        return self.mem.get_path_length()

    def process(self, start, write, addr, size):
        return self.mem.process(start, write, addr, size)
