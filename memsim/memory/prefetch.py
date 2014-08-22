from memsim import parser, machine
from memsim.memory import base, container, xilinx


def random_prefetch(machine, nxt, rand):
    stride = nxt.get_word_size() * rand.randint(-8, 8)
    result = Prefetch(nxt, stride)
    result.reset(machine)
    return result


class Prefetch(container.Container):

    def __init__(self, mem, stride):
        container.Container.__init__(self, mem)
        self.stride = stride

    def __str__(self):
        result = "(prefetch "
        result += "(stride " + str(self.stride) + ")"
        result += "(memory " + str(self.mem.get_name()) + ")"
        result += ")"
        return result

    def get_parameter_count(self):
        count = container.Container.get_parameter_count(self)
        return count + 1

    def get_word_size(self):
        return self.mem.get_word_size()

    def get_path_length(self, incoming):
        incoming += self.machine.addr_bits
        nl = container.Container.get_path_length(self, incoming)
        return max(incoming, nl)

    def get_cost(self):
        temp = Prefetch(self.get_next(), self.get_word_size())
        temp.reset(self.machine)
        return container.Container.get_cost(temp)

    def generate(self, gen, source):
        name = gen.get_name(source, self)
        word_size = self.get_word_size()
        word_width = word_size * 8
        addr_width = gen.get_addr_width(word_size)
        oname = gen.generate_next(self, self.get_next())
        gen.declare_signals(name, self.get_word_size())
        gen.add_code(name + "_inst : entity work.prefetch")
        gen.enter()
        gen.add_code("generic map (")
        gen.enter()
        gen.add_code('ADDR_WIDTH => ' + str(addr_width) + ',')
        gen.add_code("WORD_WIDTH => " + str(word_width) + ",")
        gen.add_code("STRIDE     => " + str(self.stride))
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

    def permute(self, rand):
        stride = self.stride
        self.stride = self.get_word_size() * rand.randint(-8, 8)
        return True

    def set_next(self, n):
        container.Container.set_next(self, n)

    def reset(self, m):
        container.Container.reset(self, m)


def _create_prefetch(lexer, args):
    mem = parser.get_argument(lexer, args, 'memory')
    stride = parser.get_argument(lexer, args, 'stride', 0)
    return Prefetch(mem, stride)
base.constructors['prefetch'] = _create_prefetch
