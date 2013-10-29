
import container
import join


class Transform(container.Container):
    """A memory that transforms the address space for a bank."""

    def __init__(self, bank, mem):
        container.Container.__init__(self, mem)
        self.bank = bank
        join.set_parent(bank, self)

    def generate_transform(self, op, value, inverse, gen, mach):

        name = self.get_id()
        bname = self.bank.get_id()
        oname = self.get_next().get_id()
        jname = join.find_join(self.bank, self).get_id()
        word_width = mach.word_size * 8

        self.get_next().generate(gen, mach)
        self.bank.generate(gen, mach)
        gen.declare_signals(name, mach.word_size)

        # Transform into the bank.
        gen.add_code(name + "_inst : entity work." + op)
        gen.enter()
        gen.add_code("generic map (")
        gen.enter()
        gen.add_code("ADDR_WIDTH => ADDR_WIDTH,")
        gen.add_code("WORD_WIDTH => " + str(word_width) + ",")
        gen.add_code("VALUE => " + str(value))
        gen.leave()
        gen.add_code(")")
        gen.add_code("port map (")
        gen.enter()
        gen.add_code("clk => clk,")
        gen.add_code("rst => rst,")
        gen.add_code("addr => " + name + "_addr,")
        gen.add_code("din => " + name + "_din,")
        gen.add_code("dout => " + name + "_dout,")
        gen.add_code("re => " + name + "_re,")
        gen.add_code("we => " + name + "_we,")
        gen.add_code("mask => " + name + "_mask,")
        gen.add_code("ready => " + name + "_ready,")
        gen.add_code("maddr => " + bname + "_addr,")
        gen.add_code("min => " + bname + "_dout,")
        gen.add_code("mout => " + bname + "_din,")
        gen.add_code("mre => " + bname + "_re,")
        gen.add_code("mwe => " + bname + "_we,")
        gen.add_code("mmask => " + bname + "_mask,")
        gen.add_code("mready => " + bname + "_ready")
        gen.leave()
        gen.add_code(");")
        gen.leave()

        # Transform out of the bank.
        gen.add_code(jname + "_inst : entity work." + op)
        gen.enter()
        gen.add_code("generic map (")
        gen.enter()
        gen.add_code("ADDR_WIDTH      => ADDR_WIDTH,")
        gen.add_code("WORD_WIDTH      => " + str(word_width) + ",")
        gen.add_code("VALUE             => " + str(inverse))
        gen.leave()
        gen.add_code(")")
        gen.add_code("port map (")
        gen.enter()
        gen.add_code("clk => clk,")
        gen.add_code("rst => rst,")
        gen.add_code("addr => " + jname + "_addr,")
        gen.add_code("din => " + jname + "_din,")
        gen.add_code("dout => " + jname + "_dout,")
        gen.add_code("re => " + jname + "_re,")
        gen.add_code("we => " + jname + "_we,")
        gen.add_code("mask => " + jname + "_mask,")
        gen.add_code("ready => " + jname + "_ready,")
        gen.add_code("maddr => " + oname + "_addr,")
        gen.add_code("min => " + oname + "_dout,")
        gen.add_code("mout => " + oname + "_din,")
        gen.add_code("mre => " + oname + "_re,")
        gen.add_code("mwe => " + oname + "_we,")
        gen.add_code("mmask => " + oname + "_mask,")
        gen.add_code("mready => " + oname + "_ready")
        gen.leave()
        gen.add_code(");")
        gen.leave()

    def get_banks(self):
        return [self.bank]

    def set_bank(self, i, b):
        assert(i == 0)
        self.bank = b

    def get_transform_path_length(self):
        """Get the path length through the transform."""
        assert(False)

    def get_path_length(self):
        tl = self.get_transform_path_length()
        return tl + self.bank.get_path_length()

    def get_forward_path_length(self):
        """Get the path length leaving the transform."""
        tl = self.get_transform_path_length()
        return tl + self.get_next().get_path_length()

    def is_empty(self):
        return False

    def combine(self, other):
        """Combine this transform with another transform of the same type."""
        assert(False)

    def simplify(self):
        self.bank = self.bank.simplify()
        self.mem = self.mem.simplify()
        if isinstance(self.bank, join.Join):
            return self.mem
        if self.__class__ is self.bank.__class__:
            self.combine(self.bank)
            j = join.find_join(self.bank.bank, self.bank)
            self.bank = self.bank.bank
            j.parent = self
        if self.is_empty():
            last, t = None, self.bank
            while (not isinstance(t, join.Join)) or t.parent is not self:
                last, t = t, t.get_next()
            assert(last is not None)  # We already checked for an empty bank.
            last.set_next(self.mem)
            return self.bank
        return self

    def forward(self, index, start, write, addr, size):
        """Forward a request from the bank to the following memory."""
        assert(False)
