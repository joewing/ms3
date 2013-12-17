
from memsim import machine
from memsim import parser
from memsim.memory import base
from memsim.memory import join


def random_split(machine, nxt, rand, cost):
    offset = rand.random_address(machine.word_size)
    bank0 = join.Join(0)
    bank1 = join.Join(1)
    result = Split(bank0, bank1, nxt, offset)
    return result if result.get_cost() <= cost else None


class Split(base.Memory):

    def __init__(self, bank0, bank1, mem, offset):
        base.Memory.__init__(self)
        self.bank0 = bank0
        self.bank1 = bank1
        self.mem = mem
        self.offset = offset
        join.set_parent(bank0, self)
        join.set_parent(bank1, self)

    def __str__(self):
        result = "(split "
        result += "(offset " + str(self.offset) + ")"
        result += "(bank0 " + str(self.bank0) + ")"
        result += "(bank1 " + str(self.bank1) + ")"
        result += "(memory " + str(self.mem) + ")"
        result += ")"
        return result

    def generate(self, gen, mach):

        self.get_next().generate(gen, mach)
        self.bank0.generate(gen, mach)
        self.bank1.generate(gen, mach)

        name = self.get_id()
        oname = self.get_next().get_id()
        word_width = mach.word_size * 8
        boffset = machine.log2(self.offset) - 1
        b0name = self.bank0.get_id()
        b1name = self.bank1.get_id()
        j0name = join.find_join(self.bank0, self).get_id()
        j1name = join.find_join(self.bank1, self).get_id()

        gen.add_code(name + "_combine : entity work.combine")
        gen.enter()
        gen.add_code("generic map (")
        gen.enter()
        gen.add_code("ADDR_WIDTH => ADDR_WIDTH,")
        gen.add_code("WORD_WIDTH => " + str(word_width) + ",")
        gen.add_code("BOFFSET => " + str(boffset))
        gen.leave()
        gen.add_code(")")
        gen.add_code("port map (")
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

        gen.declare_signals(name, mach.word_size)
        gen.add_code(name + "_sp : entity work.split")
        gen.enter()
        gen.add_code("generic map (")
        gen.enter()
        gen.add_code("ADDR_WIDTH        => ADDR_WIDTH,")
        gen.add_code("WORD_WIDTH        => " + str(word_width) + ",")
        gen.add_code("BOFFSET            => " + str(boffset))
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

    def permute(self, rand, max_cost):
        if rand.randbool():
            # Change the offset.
            self.offset = rand.random_address(self.machine.word_size)
        else:
            # Swap banks.
            self.bank0, self.bank1 = self.bank1, self.bank0
        return True

    def simplify(self):
        self.bank0 = self.bank0.simplify()
        self.bank1 = self.bank1.simplify()
        self.mem = self.mem.simplify()
        if isinstance(self.bank0, join.Join) and \
           isinstance(self.bank1, join.Join):
            return self.mem
        return self

    def get_path_length(self):
        b0 = self.bank0.get_path_length()
        b1 = self.bank1.get_path_length()
        return self.machine.addr_bits + max(b0, b1)

    def get_forward_path_length(self):
        return self.machine.addr_bits + self.get_next().get_path_length()

    def done(self):
        return self.mem.done()

    def process(self, start, write, addr, size):
        mask = self.machine.addr_mask
        last = (addr + size - 1) & mask
        result = start
        if addr > last:
            result = self._do_process(result, addr, mask - addr + 1, write)
            result = self._do_process(result, 0, last + 1, write)
        else:
            result = self._do_process(result, addr, size, write)
        return result

    def _do_process(self, start, addr, size, write):
        last = (addr + size - 1) & self.machine.addr_mask
        result = start
        if addr < self.offset:
            if last <= self.offset:
                temp_size = size
            else:
                temp_size = self.offset - addr
            result = self.bank0.process(result, write, addr, temp_size)
        if last >= self.offset:
            if addr >= self.offset:
                temp_addr = addr - self.offset
                temp_size = size
            else:
                temp_addr = 0
                temp_size = last - self.offset + 1
            result = self.bank1.process(result, write, temp_addr, temp_size)
        return result

    def forward(self, index, start, write, addr, size):
        if index == 1:
            addr = (addr + self.offset) & self.machine.addr_mask
            return self.mem.process(start, write, addr, size)
        else:
            return self.mem.process(start, write, addr, size)


def _create_split(lexer, args):
    offset = parser.get_argument(lexer, args, 'offset', 0)
    mem = parser.get_argument(lexer, args, 'memory')
    bank0 = parser.get_argument(lexer, args, 'bank0')
    bank1 = parser.get_argument(lexer, args, 'bank1')
    return Split(bank0, bank1, mem, offset)
base.constructors['split'] = _create_split
