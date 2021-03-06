from memsim.memory import container, join


class Transform(container.Container):
    """A memory that transforms the address space for a bank."""

    def __init__(self, bank, mem):
        container.Container.__init__(self, mem)
        self.bank = bank
        join.set_parent(bank, self)

    def generate_transform(self, op, value, inverse, gen, source):

        name = gen.get_name(source, self)
        jname = join.find_join(self.bank, self).get_id()
        word_size = self.get_word_size()
        addr_width = gen.get_addr_width(word_size)
        word_width = word_size * 8

        oname = gen.generate_next(self, self.get_next())
        bname = gen.generate_next(self, self.bank)
        gen.declare_signals(name, self.get_word_size())

        # Transform into the bank.
        gen.add_code(name + '_inst : entity work.' + op)
        gen.enter()
        gen.add_code('generic map (')
        gen.enter()
        gen.add_code('ADDR_WIDTH => ' + str(addr_width) + ',')
        gen.add_code('WORD_WIDTH => ' + str(word_width) + ',')
        gen.add_code('VALUE => ' + str(value))
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
        gen.add_code(jname + '_inst : entity work.' + op)
        gen.enter()
        gen.add_code('generic map (')
        gen.enter()
        gen.add_code('ADDR_WIDTH      => ' + str(addr_width) + ',')
        gen.add_code("WORD_WIDTH      => " + str(word_width) + ",")
        gen.add_code("VALUE           => " + str(inverse))
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
        return name

    def get_parameter_count(self):
        count = container.Container.get_parameter_count(self)
        return count + 1

    def get_banks(self):
        return [self.bank]

    def set_bank(self, i, b):
        assert(i == 0)
        self.bank = b

    def get_word_size(self):
        return self.mem.get_word_size()

    def can_remove(self):
        return isinstance(self.bank, join.Join)

    def get_transform_path_length(self):
        """Get the path length through the transform."""
        assert(False)

    def get_path_length(self, incoming):
        incoming += self.get_transform_path_length()
        nl = self.bank.get_path_length(incoming)
        return max(incoming, nl)

    def get_forward_path_length(self, incoming):
        """Get the path length leaving the transform."""
        incoming += self.get_transform_path_length()
        nl = container.Container.get_path_length(self, incoming)
        return max(incoming, nl)
