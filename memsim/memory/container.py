from memsim.memory import base, join


class Container(base.Memory):
    """A memory containing another memory (caches, etc.)."""

    def __init__(self, mem):
        base.Memory.__init__(self)
        self.mem = mem

    def get_parameter_count(self):
        return self.mem.get_parameter_count()

    def get_size(self):
        return 1 + self.mem.get_size()

    def can_remove(self):
        return all(map(lambda b: isinstance(b, join.Join), self.get_banks()))

    def can_insert(self):
        return True

    def get_next(self):
        return self.mem

    def set_next(self, n):
        self.mem = n

    def done(self):
        return self.mem.done()

    def get_path_length(self, incoming):
        word_size = self.get_word_size()
        next_word_size = self.mem.get_word_size()
        if next_word_size != word_size:
            incoming += 8
        nl = self.mem.get_path_length(incoming)
        return max(incoming, nl)

    def process(self, start, write, addr, size):
        return self.mem.process(start, write, addr, size)
