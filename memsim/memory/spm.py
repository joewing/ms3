
from memsim import machine, parser, util
from memsim.memory import base, cacti, container, xilinx


# Minimum SPM size in bytes.
MIN_SPM_SIZE = 512


def random_spm(machine, nxt, rand, cost):
    size = MIN_SPM_SIZE
    spm = SPM(nxt, size)
    spm.reset(machine)
    while spm.get_cost() < cost:
        spm.size *= 2
        spm.update_latency()
        if spm.get_cost() > cost:
            spm.size //= 2
            spm.update_latency()
            break
        elif rand.randint(0, 8) == 0:
            break
    return spm if spm.get_cost() <= cost else None


class SPM(container.Container):

    def __init__(self, mem, size=0,
                 access_time=0,
                 cycle_time=0):
        container.Container.__init__(self, mem)
        self.size = size
        self.access_time = access_time
        self.cycle_time = cycle_time
        self.pending = 0

    def __str__(self):
        result = "(spm "
        result += "(size " + str(self.size) + ")"
        if self.access_time > 0:
            result += "(access_time " + str(self.access_time) + ")"
        if self.cycle_time > 0:
            result += "(cycle_time " + str(self.cycle_time) + ")"
        result += "(memory " + str(self.mem) + ")"
        result += ")"
        return result

    def generate(self, gen, mach):
        name = self.get_id()
        oname = self.get_next().get_id()
        word_width = mach.word_size * 8
        size_bits = util.log2(self.size // mach.word_size) - 1
        self.get_next().generate(gen, mach)
        gen.declare_signals(name, mach.word_size)
        gen.add_code(name + "_inst : entity work.spm")
        gen.enter()
        gen.add_code("generic map (")
        gen.enter()
        gen.add_code("ADDR_WIDTH => ADDR_WIDTH,")
        gen.add_code("WORD_WIDTH => " + str(word_width) + ",")
        gen.add_code("SIZE_BITS  => " + str(size_bits))
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

    def update_latency(self):
        if self.machine.target == machine.TargetType.ASIC:
            self.access_time = cacti.get_access_time(self.machine, self)
            self.cycle_time = cacti.get_cycle_time(self.machine, self)
        elif self.machine.target == machine.TargetType.FPGA:
            self.access_time = 2
            self.cycle_time = 2

    def get_cost(self):
        if self.machine.target == machine.TargetType.SIMPLE:
            return self.size * 8
        elif self.machine.target == machine.TargetType.ASIC:
            return cacti.get_area(self.machine, self)
        elif self.machine.target == machine.TargetType.FPGA:
            return xilinx.get_bram_count(self.machine, self)
        else:
            assert(False)

    def permute(self, rand, max_cost):
        if self.size > self.machine.word_size and rand.randint(0, 1) == 1:
            self.size //= 2
        else:
            self.size *= 2
            if self.get_cost() > max_cost:
                self.size //= 2
                self.update_latency()
                return False
        self.update_latency()
        return True

    def simplify(self):
        self.mem = self.mem.simplify()
        if self.size == 0:
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

    def get_path_length(self):
        return self.machine.addr_bits + self.get_next().get_path_length()

    def done(self):
        return max(self.pending - self.machine.time, 0)

    def process(self, start, write, addr, size):
        result = max(start, self.pending - self.machine.time)
        last_addr = (addr + size) & self.machine.addr_mask
        if addr < self.size and last_addr <= self.size:
            # Complete hits the scrachpad
            offset = addr % self.machine.word_size
            count = (size + self.machine.word_size + offset - 1)
            count //= self.machine.word_size
            self.pending = self.machine.time + result
            self.pending += max(self.cycle_time - self.access_time, 0)
            result += (count - 1) * self.cycle_time + self.access_time
        elif addr >= self.size and last_addr > self.size:
            # Completely misses the scratchpad
            self.pending = self.machine.time + result
            result = self.mem.process(result, write, addr, size)
        elif addr > self.size and last_addr < self.size:
            # First part hits, second part misses
            msize = size - last_addr + 1
            count = (last_addr + self.machine.word_size)
            count //= self.machine.word_size
            result += (count - 1) * self.cycle_time + self.access_time
            self.pending = self.machine.time + result
            self.pending += max(self.cycle_time - self.access_time, 0)
            result = self.mem.process(result, write, addr, msize)
        else:
            # First part misses, second part hits
            hsize = self.size - addr
            offset = addr % self.machine.word_size
            count = (hsize + self.machine.word_size + offset - 1)
            count //= self.machine.word_size
            result += (count - 1) * self.cycle_time + self.access_time
            self.pending = self.machine.time + result
            self.pending += max(self.cycle_time - self.access_time, 0)
            result = self.mem.process(result, write, self.size, size - hsize)
        return result


def _create_spm(lexer, args):
    mem = parser.get_argument(lexer, args, 'memory')
    size = parser.get_argument(lexer, args, 'size', 0)
    access_time = parser.get_argument(lexer, args, 'access_time', 2)
    cycle_time = parser.get_argument(lexer, args, 'cycle_time', access_time)
    return SPM(mem, size, access_time, cycle_time)
base.constructors['spm'] = _create_spm
