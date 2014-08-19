from memsim import lex, machine, parser, util, cost
from memsim.memory import base, cacti, container, xilinx


class CachePolicy(object):
    LRU = 0
    MRU = 1
    FIFO = 2
    PLRU = 3
    MAX_POLICY = 3


policy_map = {
    CachePolicy.LRU:  "lru",
    CachePolicy.MRU:  "mru",
    CachePolicy.FIFO: "fifo",
    CachePolicy.PLRU: "plru"
}


def show_policy(value):
    return policy_map.get(value, "?")


def parse_policy(lexer, name):
    for (k, v) in policy_map.items():
        if name == v:
            return k
    raise lex.ParseError(lexer, "invalid cache policy: " + name)


def random_cache(machine, nxt, rand):
    line_size = nxt.get_word_size()
    line_count = 16
    associativity = 1
    policy = rand.randint(0, CachePolicy.MAX_POLICY)
    write_back = rand.randbool()
    result = Cache(nxt,
                   line_count=line_count,
                   line_size=line_size,
                   associativity=associativity,
                   policy=policy,
                   write_back=write_back)
    result.reset(machine)
    while rand.randint(0, 1) == 0:
        result.line_count *= 2
    result.update_latency()
    return result


class CacheLine(object):
    tag = -1
    age = 0
    dirty = False


class Cache(container.Container):

    def __init__(self, mem,
                 line_count=1,
                 line_size=8,
                 associativity=1,
                 access_time=0,
                 cycle_time=0,
                 policy=CachePolicy.LRU,
                 write_back=True):
        container.Container.__init__(self, mem)
        self.line_count = line_count
        self.line_size = line_size
        self.associativity = associativity
        self.access_time = access_time
        self.cycle_time = cycle_time
        self.policy = policy
        self.write_back = write_back
        self.lines = list()
        self.pending = 0

    def __str__(self):
        result = '(cache '
        result += '(line_count ' + str(self.line_count) + ')'
        result += '(line_size ' + str(self.line_size) + ')'
        result += '(associativity ' + str(self.associativity) + ')'
        if self.access_time > 0:
            result += '(access_time ' + str(self.access_time) + ')'
        if self.cycle_time > 0:
            result += '(cycle_time ' + str(self.cycle_time) + ')'
        if self.associativity > 1:
            result += '(policy ' + show_policy(self.policy) + ')'
        if self.write_back:
            result += '(write_back true)'
        else:
            result += '(write_back false)'
        result += '(memory ' + str(self.mem.get_name()) + ')'
        result += ')'
        return result

    def get_parameter_count(self):
        count = container.Container.get_parameter_count(self)
        return count + 5

    def generate(self, gen, source):
        name = source.get_id() + self.get_id()
        word_size = self.get_word_size()
        oname = gen.generate_next(self, self.get_next())
        addr_width = gen.get_addr_width(word_size)
        word_width = word_size * 8
        line_size_bits = util.log2(8 * self.line_size // word_width - 1)
        line_count_bits = \
            util.log2(self.line_count // self.associativity - 1)
        assoc_bits = util.log2(self.associativity - 1)
        gen.declare_signals(name, self.get_word_size())
        gen.add_code(name + '_inst : entity work.cache')
        gen.enter()
        gen.add_code('generic map (')
        gen.enter()
        gen.add_code('ADDR_WIDTH => ' + str(addr_width) + ',')
        gen.add_code('WORD_WIDTH => ' + str(word_width) + ',')
        gen.add_code('LINE_SIZE_BITS => ' + str(line_size_bits) + ',')
        gen.add_code('LINE_COUNT_BITS => ' + str(line_count_bits) + ',')
        gen.add_code('ASSOC_BITS => ' + str(assoc_bits) + ',')
        replacement = -1
        if self.policy == CachePolicy.LRU:
            replacement = 0
        elif self.policy == CachePolicy.MRU:
            replacement = 1
        elif self.policy == CachePolicy.FIFO:
            replacement = 2
        elif self.policy == CachePolicy.PLRU:
            replacement = 3
        assert(replacement >= 0)
        gen.add_code('REPLACEMENT => ' + str(replacement) + ',')
        if self.write_back:
            gen.add_code('WRITE_POLICY => 0')
        else:
            gen.add_code('WRITE_POLICY => 1')
        gen.leave()
        gen.add_code(')')
        gen.add_code('port map (')
        gen.enter()
        gen.add_code('clk => clk,')
        gen.add_code('rst => rst,')
        gen.add_code('addr => ' + name + '_addr,')
        gen.add_code('din => ' + name + '_din,')
        gen.add_code('dout => ' + name + '_dout,')
        gen.add_code('re => ' + name + '_re,')
        gen.add_code('we => ' + name + '_we,')
        gen.add_code('mask => ' + name + '_mask,')
        gen.add_code('ready => ' + name + '_ready,')
        gen.add_code('maddr => ' + oname + '_addr,')
        gen.add_code('min => ' + oname + '_dout,')
        gen.add_code('mout => ' + oname + '_din,')
        gen.add_code('mre => ' + oname + '_re,')
        gen.add_code('mwe => ' + oname + '_we,')
        gen.add_code('mmask => ' + oname + '_mask,')
        gen.add_code('mready => ' + oname + '_ready')
        gen.leave()
        gen.add_code(');')
        gen.leave()
        return name

    def get_word_size(self):
        return self.line_size

    def get_path_length(self, incoming):
        nl = container.Container.get_path_length(self, 16)
        return max(incoming + 16, nl)

    def update_latency(self):
        if self.machine.target == machine.TargetType.ASIC:
            self.access_time = cacti.get_access_time(self.machine, self)
            self.cycle_time = cacti.get_cycle_time(self.machine, self)
        elif self.machine.target == machine.TargetType.FPGA:
            self.access_time = 3
            self.cycle_time = 3

    def get_cost(self):
        if self.machine.target == machine.TargetType.SIMPLE:
            index_bits = util.log2(self.line_count - 1)
            word_size = self.get_word_size()
            line_words = (self.line_size + word_size - 1) // word_size
            ls_bits = util.log2(line_words - 1)
            tag_bits = max(self.machine.addr_bits - index_bits - ls_bits, 0)
            width = 1 + tag_bits
            if self.associativity > 1:
                if self.policy == CachePolicy.PLRU:
                    width += 1
                else:
                    width += util.log2(self.associativity - 1)
            if self.write_back:
                width += 1
            width *= self.associativity
            depth = self.line_count // self.associativity
            return cost.Cost(width * depth)
        if self.machine.target == machine.TargetType.ASIC:
            return cost.Cost(cacti.get_area(self.machine, self))
        elif self.machine.target == machine.TargetType.FPGA:
            return xilinx.get_cost(self.machine, self)

    def permute(self, rand):
        param_count = 8
        param = rand.randint(0, param_count - 1)
        line_size = self.line_size
        line_count = self.line_count
        associativity = self.associativity
        policy = self.policy
        write_back = self.write_back
        for i in xrange(param_count):
            if param == 0:
                self.line_size *= 2
                self.update_latency()
                return True
            elif param == 1 and line_size > 1:
                self.line_size //= 2
                self.update_latency()
                return True
            elif param == 2:
                self.line_count *= 2
                self.update_latency()
                return True
            elif param == 3 and line_count > associativity:
                self.line_count //= 2
                self.update_latency()
                return True
            elif param == 4 and associativity < line_count:
                self.associativity *= 2
                self.update_latency()
                return True
            elif param == 5 and associativity > 1:
                self.associativity //= 2
                self.update_latency()
                return True
            elif param == 6:
                self.policy = rand.randint(0, CachePolicy.MAX_POLICY)
                self.update_latency()
                return True
            else:
                self.write_back = not self.write_back
                self.update_latency()
                return True
            param = (param + 1) % param_count
        self.update_latency()
        return False

    def reset(self, m):
        container.Container.reset(self, m)
        self.pending = 0
        self.lines = list()
        for i in xrange(self.line_count):
            self.lines.append(CacheLine())
        self.update_latency()

    def done(self):
        t = container.Container.done(self)
        return max(self.pending - self.machine.time, t)

    def process(self, baddr, start, write, addr, size):
        extra = size // self.line_size
        mask = self.machine.addr_mask
        temp = addr
        result = max(start, self.pending - self.machine.time)
        for i in xrange(extra):
            result = self._do_process(baddr, result,
                                      write, temp, self.line_size)
            temp = (temp + self.line_size) & mask
        if size > extra * self.line_size:
            result = self._do_process(baddr, result, write, temp,
                                      size - extra * self.line_size)
        self.pending = self.machine.time + result
        self.pending += max(self.cycle_time - self.access_time, 0)
        return result

    def _access_line(self, baddr, write, t, addr):
        return base.send_request(self.mem, baddr, t, write,
                                 addr, self.line_size)

    def _do_process(self, baddr, start, write, addr, size):
        tag = addr & ~(self.line_size - 1)
        set_size = self.line_count // self.associativity
        line_addr = addr // self.line_size
        first_line = line_addr % set_size

        # Update ages
        age_sum = 0
        for i in xrange(self.associativity):
            line_index = first_line + i * set_size
            line = self.lines[line_index]
            age_sum += line.age
            if self.policy != CachePolicy.PLRU:
                line.age += 1

        # Check if this address is in the cache.
        to_replace = self.lines[first_line]
        age = to_replace.age
        for i in xrange(self.associativity):
            line_index = first_line + i * set_size
            line = self.lines[line_index]
            if tag == line.tag:  # Hit
                if self.policy == CachePolicy.PLRU:
                    if age_sum + 1 == self.associativity:
                        for j in xrange(self.associativity):
                            self.lines[first_line + j * set_size].age = 0
                    line.age = 1
                elif self.policy != CachePolicy.FIFO:
                    line.age = 0
                if (not write) or self.write_back:
                    line.dirty = line.dirty or write
                    return start + self.access_time
                else:
                    t = self._access_line(baddr, True, start, tag)
                    return t + self.access_time
            elif self.policy == CachePolicy.MRU:
                if line.age < age:
                    to_replace = line
                    age = line.age
            elif self.policy == CachePolicy.PLRU:
                if line.age == 0:
                    to_replace = line
                    age = line.age
            else:
                if line.age > age:
                    to_replace = line
                    age = line.age

        # If we got here we have a cache miss.
        # to_replace will be the line we need to replace.
        if (not write) or self.write_back:

            # Evict this entry if necessary.
            time = start + self.access_time
            if to_replace.dirty:
                time = self._access_line(baddr, True, time, to_replace.tag)
                to_replace.dirty = False
            to_replace.tag = tag
            to_replace.dirty = write

            # Update the age.
            if self.policy == CachePolicy.PLRU:
                if age_sum + 1 == self.associativity:
                    for j in xrange(self.associativity):
                        self.lines[first_line + j * set_size].age = 0
                to_replace.age = 1
            else:
                to_replace.age = 0

            # Read the new entry.
            if (not write) or size != self.line_size:
                time = self._access_line(baddr, False, time, tag)

            return time

        else:
            # Write on a write-through cache.
            t = base.send_request(self.mem, baddr, start, True, addr, size)
            return t + self.access_time


def _create_cache(lexer, args):
    line_count = parser.get_argument(lexer, args, 'line_count', 1)
    line_size = parser.get_argument(lexer, args, 'line_size', 8)
    associativity = parser.get_argument(lexer, args, 'associativity', 1)
    access_time = parser.get_argument(lexer, args, 'access_time', 1)
    cycle_time = parser.get_argument(lexer, args, 'cycle_time', access_time)
    policy_str = parser.get_argument(lexer, args, 'policy', 'lru')
    policy = parse_policy(lexer, policy_str)
    write_back = parser.get_argument(lexer, args, 'write_back', True)
    mem = parser.get_argument(lexer, args, 'memory')
    return Cache(mem=mem,
                 line_count=line_count,
                 line_size=line_size,
                 associativity=associativity,
                 access_time=access_time,
                 cycle_time=cycle_time,
                 policy=policy,
                 write_back=write_back)
base.constructors['cache'] = _create_cache
