from memsim import parser
from memsim.memory import subsystem, base, xilinx
from memsim.machine import TargetType


class FIFO(subsystem.Subsystem):
    """Container for FIFO memories."""

    def __init__(self, index, mem, word_size, depth, bram, min_depth):
        """Create a memory to be used as a FIFO.

        Arguments:
            index:      A unique identifier for this FIFO.
            mem:        The memory subsystem.
            word_size:  The size of an item in the FIFO.
            depth:      The depth of the FIFO in items.
            bram:       Set if the FIFO is to be implemented in BRAM.
            min_depth:  Minimum depth in items.
        """
        depth = max(depth, min_depth)
        subsystem.Subsystem.__init__(self, index, word_size, depth, mem)
        self.min_depth = min_depth
        self.bram = bram

    def get_name(self, full):
        result = '(fifo '
        result += '(id ' + str(self.index) + ')'
        result += '(depth ' + str(self.depth) + ')'
        if self.bram and self.depth > 1:
            result += '(bram true)'
        if self.min_depth > 1:
            result += '(min_depth ' + str(self.min_depth) + ')'
        result += '(word_size ' + str(self.word_size) + ')'
        result += '(memory ' + self.get_next().get_name(full) + ')'
        result += ')'
        return result

    def is_fifo(self):
        return True

    def get_consume_time(self):
        return self.min_consume_time

    def get_produce_time(self):
        return self.min_produce_time

    def get_parameter_count(self):
        count = subsystem.Subsystem.get_parameter_count(self)
        return count + 1

    def total_size(self):
        return self.depth * self.word_size

    def get_cost(self):
        return self.machine.get_zero_cost()

    def reset(self, machine):
        subsystem.Subsystem.reset(self, machine)

    def generate(self, gen, source):
        name = gen.get_name(source, self)
        word_size = self.get_word_size()
        addr_width = gen.get_addr_width(word_size)
        word_width = word_size * 8
        gen.declare_signal(name + '_din', str(word_width))
        gen.declare_signal(name + '_dout', str(word_width))
        gen.declare_signal(name + '_re')
        gen.declare_signal(name + '_we')
        gen.declare_signal(name + '_avail')
        gen.declare_signal(name + '_full')
        oname = None
        if self.depth > 1 and not self.bram:
            oname = gen.generate_next(self, self.mem)
        gen.add_code(name + ' : entity work.fifo')
        gen.enter()
        gen.add_code('generic map(')
        gen.enter()
        gen.add_code('WIDTH => ' + str(word_width) + ',')
        gen.add_code('ADDR_WIDTH => ' + str(addr_width) + ',')
        gen.add_code('BRAM => ' + ('true,' if self.bram else 'false,'))
        gen.add_code('DEPTH => ' + str(self.depth))
        gen.leave()
        gen.add_code(')')
        gen.add_code('port map (')
        gen.enter()
        gen.add_code('clk => clk,')
        gen.add_code('rst => rst,')
        gen.add_code('din => ' + name + '_din,')
        gen.add_code('dout => ' + name + '_dout,')
        gen.add_code('re => ' + name + '_re,')
        gen.add_code('we => ' + name + '_we,')
        gen.add_code('avail => ' + name + '_avail,')
        gen.add_code('full => ' + name + '_full,')
        if self.depth > 1 and not self.bram:
            gen.add_code('mem_addr => ' + oname + '_addr,')
            gen.add_code('mem_in => ' + oname + '_dout,')
            gen.add_code('mem_out => ' + oname + '_din,')
            gen.add_code('mem_mask => ' + oname + '_mask,')
            gen.add_code('mem_re => ' + oname + '_re,')
            gen.add_code('mem_we => ' + oname + '_we,')
            gen.add_code('mem_ready => ' + oname + '_ready')
        else:
            gen.add_code('mem_addr => open,')
            gen.add_code('mem_in => (others => \'X\'),')
            gen.add_code('mem_out => open,')
            gen.add_code('mem_mask => open,')
            gen.add_code('mem_re => open,')
            gen.add_code('mem_we => open,')
            gen.add_code('mem_ready => \'X\'')
        gen.leave()
        gen.add_code(');')
        gen.leave()
        return name

    def permute(self, rand):
        action_count = 3
        action = rand.randint(0, action_count - 1)
        for i in xrange(action_count):
            if action == 0 and self.machine.target == TargetType.FPGA:
                self.bram = not self.bram
                return True
            elif action == 1:
                self.depth *= 2
                return True
            elif action == 2 and self.depth // 2 >= self.min_depth:
                self.depth //= 2
                return True
            action = (action + 1) % action_count
        assert(self.depth >= self.min_depth)
        return False


def _create_fifo(lexer, args):
    index = parser.get_argument(lexer, args, 'id', 0)
    word_size = parser.get_argument(lexer, args, 'word_size', 4)
    depth = parser.get_argument(lexer, args, 'depth', 1)
    bram = parser.get_argument(lexer, args, 'bram', False)
    min_depth = parser.get_argument(lexer, args, 'min_depth', 1)
    mem = parser.get_argument(lexer, args, 'memory')
    return FIFO(index=index, mem=mem, depth=depth, min_depth=min_depth,
                bram=bram, word_size=word_size)
base.constructors['fifo'] = _create_fifo
