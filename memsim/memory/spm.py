from memsim import machine, parser, util, cost
from memsim.memory import base, cacti, container, xilinx


# Minimum number of words.
MIN_SPM_WORDS = 512


def random_spm(machine, nxt, rand):
    word_size = nxt.get_word_size()
    size = MIN_SPM_WORDS * word_size
    spm = SPM(nxt, word_size, size)
    spm.reset(machine)
    while rand.randint(0, 1) == 0:
        spm.size *= 2
        spm.update_latency()
    return spm


class SPM(container.Container):

    def __init__(self, mem, word_size, size, access_time=0, cycle_time=0):
        container.Container.__init__(self, mem)
        self.word_size = word_size
        self.size = size
        self.access_time = access_time
        self.cycle_time = cycle_time
        self.pending = 0

    def __str__(self):
        result = '(spm '
        result += '(word_size ' + str(self.word_size) + ')'
        result += '(size ' + str(self.size) + ')'
        if self.access_time > 0:
            result += '(access_time ' + str(self.access_time) + ')'
        if self.cycle_time > 0:
            result += '(cycle_time ' + str(self.cycle_time) + ')'
        result += '(memory ' + str(self.mem.get_name()) + ')'
        result += ')'
        return result

    def get_parameter_count(self):
        count = container.Container.get_parameter_count(self)
        return count + 2

    def get_word_size(self):
        return self.word_size

    def generate(self, gen, source):
        name = gen.get_name(source, self)
        word_size = self.get_word_size()
        word_width = word_size * 8
        addr_width = gen.get_addr_width(word_size)
        oname = gen.generate_next(self, self.get_next())
        size_bits = util.log2(self.size // word_size) - 1
        gen.declare_signals(name, word_size)
        gen.add_code(name + '_inst : entity work.spm')
        gen.enter()
        gen.add_code('generic map (')
        gen.enter()
        gen.add_code('ADDR_WIDTH => ' + str(addr_width) + ',')
        gen.add_code('WORD_WIDTH => ' + str(word_width) + ',')
        gen.add_code('SIZE_BITS  => ' + str(size_bits))
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

    def update_latency(self):
        if self.machine.target == machine.TargetType.ASIC:
            self.access_time = cacti.get_access_time(self.machine, self)
            self.cycle_time = cacti.get_cycle_time(self.machine, self)
        elif self.machine.target == machine.TargetType.FPGA:
            self.access_time = 2
            self.cycle_time = 2

    def get_cost(self):
        if self.machine.target == machine.TargetType.SIMPLE:
            return cost.Cost(self.size * self.word_size)
        elif self.machine.target == machine.TargetType.ASIC:
            return cost.Cost(cacti.get_area(self.machine, self))
        elif self.machine.target == machine.TargetType.FPGA:
            return xilinx.get_cost(self.machine, self)
        else:
            assert(False)

    def permute(self, rand):
        temp = rand.randint(0, 3)
        if temp == 0 and self.size > self.word_size * MIN_SPM_WORDS:
            self.size //= 2
            self.update_latency()
            return True
        elif temp == 1:
            self.size *= 2
            self.update_latency()
            return True
        elif temp == 2 and self.word_size > 1:
            self.word_size //= 2
            self.update_latency()
            return True
        elif temp == 3 and self.size > self.word_size * MIN_SPM_WORDS:
            self.word_size *= 2
            self.update_latency()
            return True
        self.update_latency()
        return False

    def simplify(self):
        self.mem = self.mem.simplify()
        if self.size < self.word_size:
            return self.mem
        else:
            return self

    def reset(self, m):
        container.Container.reset(self, m)
        self.pending = 0
        self.update_latency()

    def push_transform(self, index, rand):
        assert(index == -1)
        rand.push_limit(self.size, self.machine.addr_mask)

    def pop_transform(self, rand):
        rand.pop_limit()

    def get_path_length(self, incoming):
        incoming += self.machine.addr_bits
        nl = container.Container.get_path_length(self, incoming)
        return max(incoming, nl)

    def done(self):
        t = container.Container.done(self)
        return max(self.pending - self.machine.time, t)

    def process(self, baddr, start, write, addr, size):
        word_size = self.get_word_size()
        result = max(start, self.pending - self.machine.time)
        last_addr = (addr + size) & self.machine.addr_mask
        if addr < self.size and last_addr <= self.size:
            # Complete hits the scrachpad
            offset = addr % word_size
            count = (size + word_size + offset - 1) // word_size
            self.pending = self.machine.time + result
            self.pending += max(self.cycle_time - self.access_time, 0)
            result += (count - 1) * self.cycle_time + self.access_time
        elif addr >= self.size and last_addr > self.size:
            # Completely misses the scratchpad
            self.pending = self.machine.time + result
            result = base.send_request(self.mem, baddr, result,
                                       write, addr, size)
        elif addr > self.size and last_addr < self.size:
            # First part hits, second part misses
            msize = size - last_addr + 1
            count = (last_addr + word_size) // word_size
            result += (count - 1) * self.cycle_time + self.access_time
            self.pending = self.machine.time + result
            self.pending += max(self.cycle_time - self.access_time, 0)
            result = base.send_request(self.mem, baddr, result,
                                       write, addr, msize)
        else:
            # First part misses, second part hits
            hsize = self.size - addr
            offset = addr % word_size
            count = (hsize + word_size + offset - 1) // word_size
            result += (count - 1) * self.cycle_time + self.access_time
            self.pending = self.machine.time + result
            self.pending += max(self.cycle_time - self.access_time, 0)
            result = base.send_request(self.mem, baddr, result, write,
                                       self.size, size - hsize)
        return result


def _create_spm(lexer, args):
    mem = parser.get_argument(lexer, args, 'memory')
    word_size = parser.get_argument(lexer, args, 'word_size', 4)
    size = parser.get_argument(lexer, args, 'size', 0)
    access_time = parser.get_argument(lexer, args, 'access_time', 2)
    cycle_time = parser.get_argument(lexer, args, 'cycle_time', access_time)
    return SPM(mem, word_size, size, access_time, cycle_time)
base.constructors['spm'] = _create_spm
