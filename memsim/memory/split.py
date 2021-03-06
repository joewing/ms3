from memsim import parser
from memsim.memory import base, container, join


def random_split(machine, nxt, rand):
    offset = rand.random_address(nxt.get_word_size())
    bank0 = join.Join(0)
    bank1 = join.Join(1)
    result = Split(bank0, bank1, nxt, offset)
    result.reset(machine)
    return result


class Split(container.Container):

    def __init__(self, bank0, bank1, mem, offset):
        container.Container.__init__(self, mem)
        self.bank0 = bank0
        self.bank1 = bank1
        self.offset = offset
        join.set_parent(bank0, self)
        join.set_parent(bank1, self)

    def get_name(self, full):
        result = '(split '
        result += '(offset ' + str(self.offset) + ')'
        result += '(bank0 ' + str(self.bank0.get_name(full)) + ')'
        result += '(bank1 ' + str(self.bank1.get_name(full)) + ')'
        result += '(memory ' + str(self.mem.get_name(full)) + ')'
        result += ')'
        return result

    def get_parameter_count(self):
        count = container.Container.get_parameter_count(self)
        count += self.bank0.get_parameter_count()
        count += self.bank1.get_parameter_count()
        return count + 1

    def get_word_size(self):
        return self.mem.get_word_size()

    def generate(self, gen, source):

        oname = gen.generate_next(self, self.get_next())
        b0name = gen.generate_next(self, self.bank0)
        b1name = gen.generate_next(self, self.bank1)

        name = gen.get_name(source, self)
        j0name = join.find_join(self.bank0, self).get_id()
        j1name = join.find_join(self.bank1, self).get_id()

        word_size = self.get_word_size()
        word_width = word_size * 8
        addr_width = gen.get_addr_width(word_size)
        word_offset = self.offset // word_size
        offset_bits = []
        for i in reversed(xrange(0, addr_width)):
            if word_offset & (1 << i):
                offset_bits.append('1')
            else:
                offset_bits.append('0')
        offset_str = ''.join(offset_bits)

        gen.add_code(name + '_combine : entity work.combine')
        gen.enter()
        gen.add_code('generic map (')
        gen.enter()
        gen.add_code('ADDR_WIDTH => ' + str(addr_width) + ',')
        gen.add_code('WORD_WIDTH => ' + str(word_width) + ',')
        gen.add_code('OFFSET => "' + offset_str + '"')
        gen.leave()
        gen.add_code(')')
        gen.add_code('port map (')
        gen.enter()
        gen.add_code("clk => clk,")
        gen.add_code("rst => rst,")
        gen.add_code("addr0 => " + j0name + "_addr,")
        gen.add_code("din0 => " + j0name + "_din,")
        gen.add_code("dout0 => " + j0name + "_dout,")
        gen.add_code("re0 => " + j0name + "_re,")
        gen.add_code("we0 => " + j0name + "_we,")
        gen.add_code("mask0 => " + j0name + "_mask,")
        gen.add_code("ready0 => " + j0name + "_ready,")
        gen.add_code("addr1 => " + j1name + "_addr,")
        gen.add_code("din1 => " + j1name + "_din,")
        gen.add_code("dout1 => " + j1name + "_dout,")
        gen.add_code("re1 => " + j1name + "_re,")
        gen.add_code("we1 => " + j1name + "_we,")
        gen.add_code("mask1 => " + j1name + "_mask,")
        gen.add_code("ready1 => " + j1name + "_ready,")
        gen.add_code("maddr => " + oname + "_addr,")
        gen.add_code("mout => " + oname + "_din,")
        gen.add_code("min => " + oname + "_dout,")
        gen.add_code("mre => " + oname + "_re,")
        gen.add_code("mwe => " + oname + "_we,")
        gen.add_code("mmask => " + oname + "_mask,")
        gen.add_code("mready => " + oname + "_ready")
        gen.leave()
        gen.add_code(");")
        gen.leave()

        gen.declare_signals(name, word_size)
        gen.add_code(name + '_sp : entity work.split')
        gen.enter()
        gen.add_code('generic map (')
        gen.enter()
        gen.add_code('ADDR_WIDTH        => ' + str(addr_width) + ',')
        gen.add_code("WORD_WIDTH        => " + str(word_width) + ",")
        gen.add_code('OFFSET            => "' + offset_str + '"')
        gen.leave()
        gen.add_code(")")
        gen.add_code("port map (")
        gen.add_code("clk => clk,")
        gen.add_code("rst => rst,")
        gen.add_code("addr => " + name + "_addr,")
        gen.add_code("din => " + name + "_din,")
        gen.add_code("dout => " + name + "_dout,")
        gen.add_code("re => " + name + "_re,")
        gen.add_code("we => " + name + "_we,")
        gen.add_code("mask => " + name + "_mask,")
        gen.add_code("ready => " + name + "_ready,")
        gen.add_code("maddr0 => " + b0name + "_addr,")
        gen.add_code("mout0 => " + b0name + "_din,")
        gen.add_code("min0 => " + b0name + "_dout,")
        gen.add_code("mre0 => " + b0name + "_re,")
        gen.add_code("mwe0 => " + b0name + "_we,")
        gen.add_code("mmask0 => " + b0name + "_mask,")
        gen.add_code("mready0 => " + b0name + "_ready,")
        gen.add_code("maddr1 => " + b1name + "_addr,")
        gen.add_code("mout1 => " + b1name + "_din,")
        gen.add_code("min1 => " + b1name + "_dout,")
        gen.add_code("mre1 => " + b1name + "_re,")
        gen.add_code("mwe1 => " + b1name + "_we,")
        gen.add_code("mmask1 => " + b1name + "_mask,")
        gen.add_code("mready1 => " + b1name + "_ready")
        gen.add_code(");")
        gen.leave()
        return name

    def get_cost(self):
        temp = Split(join.Join(0), join.Join(1), self.get_next(),
                     self.get_word_size())
        temp.reset(self.machine)
        return container.Container.get_cost(temp)

    def get_next(self):
        return self.mem

    def set_next(self, n):
        self.mem = n

    def get_banks(self):
        return [self.bank0, self.bank1]

    def set_bank(self, i, b):
        assert(i >= 0 and i <= 1)
        if i == 0:
            self.bank0 = b
        else:
            self.bank1 = b

    def permute(self, rand):
        word_size = self.get_word_size()
        offset = self.offset
        action = rand.randint(0, 3)
        if action == 0 or action == 1:
            # Increment/decrement.
            direction = word_size if action == 0 else -word_size
            self.offset += direction
            while rand.randbool():
                self.offset += direction
            if self.offset < rand.get_min_address():
                self.offset = rand.get_min_address()
            if self.offset > rand.get_max_address():
                self.offset = rand.get_max_address()
        elif action == 2:
            # Resample the prior.
            self.offset = rand.random_address(word_size)
        else:
            # Swap banks.
            self.bank0, self.bank1 = self.bank1, self.bank0
        return True

    def get_path_length(self, incoming):
        start = incoming + self.machine.addr_bits
        b0 = self.bank0.get_path_length(start)
        b1 = self.bank1.get_path_length(start)
        return max(start, b0, b1)

    def get_forward_path_length(self, incoming):
        incoming += self.machine.addr_bits
        nl = container.Container.get_path_length(self, incoming)
        return max(incoming, nl)


def _create_split(lexer, args):
    offset = parser.get_argument(lexer, args, 'offset', 0)
    mem = parser.get_argument(lexer, args, 'memory')
    bank0 = parser.get_argument(lexer, args, 'bank0')
    bank1 = parser.get_argument(lexer, args, 'bank1')
    return Split(bank0, bank1, mem, offset)
base.constructors['split'] = _create_split
