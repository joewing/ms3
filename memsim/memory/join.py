from memsim.memory import base


class Join(base.Memory):

    def __init__(self, index=0):
        base.Memory.__init__(self)
        self.index = index
        self.parent = None

    def __str__(self):
        return '(join)'

    def get_parameter_count(self):
        return 0

    def get_word_size(self):
        return self.parent.get_word_size()

    def can_remove(self):
        return False

    def can_insert(self):
        return True

    def generate(self, gen, source):
        name = self.get_id()
        gen.declare_signals(name, self.get_word_size())
        return name

    def get_path_length(self, incoming):
        nl = self.parent.get_forward_path_length(incoming)
        return max(incoming, nl)


def find_join(mem, parent=None):
    while mem is not None:
        if isinstance(mem, Join) and mem.parent is parent:
            break
        mem = mem.get_next()
    return mem


def set_parent(bank, parent):
    join = find_join(bank)
    join.parent = parent


def _create_join(lexer, args):
    return Join()
base.constructors['join'] = _create_join
