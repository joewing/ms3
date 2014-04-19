from memsim.memory.fifo import FIFO


class VHDLGenerator(object):
    """Class used to generate VHDL code."""

    def __init__(self, machine):
        self.machine = machine
        self.sigs = ''
        self.code = ''
        self.result = ''
        self.indent = 0

    def enter(self):
        """Increase the indent level."""
        self.indent += 1

    def leave(self):
        """Decrease the indent level."""
        assert(self.indent > 0)
        self.indent -= 1

    def add_code(self, s):
        """Add to the code section."""
        self.code += '  ' * self.indent + s + '\n'

    def add_sig(self, s):
        """Add to the signal section."""
        self.sigs += '  ' * self.indent + s + '\n'

    def append(self, s):
        """Append to the result (clients should use add_code or add_sig)."""
        self.result += '  ' * self.indent + s + '\n'

    def get_addr_width(self, word_size):
        return self.machine.get_addr_width(word_size)

    def get_name(self, source, mem):
        """Get the base port name for connecting source to mem."""
        if source is not None:
            return source.get_id() + mem.get_id()
        else:
            return mem.get_id()

    def declare_signal(self, name, width_str=None):
        """Declare a signal."""
        if width_str is None:
            ts = 'std_logic'
        else:
            ts = 'std_logic_vector(' + width_str + ' - 1 downto 0)'
        self.add_sig('signal ' + name + ' : ' + ts + ';')

    def declare_signals(self, name, word_size):
        """Declare the signals for communication between componeents."""
        word_width = word_size * 8
        addr_width = self.get_addr_width(word_size)
        self.declare_signal(name + "_addr", str(addr_width))
        self.declare_signal(name + "_din", str(word_width))
        self.declare_signal(name + "_dout", str(word_width))
        self.declare_signal(name + "_re")
        self.declare_signal(name + "_we")
        self.declare_signal(name + "_mask", str(word_width // 8))
        self.declare_signal(name + "_ready")

    def generate_next(self, source, mem):
        """Generate VHDL for the next memory in the subsystem.

        source is the upstream memory.
        mem is the downstream memory to create.
        Returns the base name for the interface to the (adapted)
        downstream memory.
        """
        name = mem.generate(self, source)
        next_word_size = mem.get_word_size()
        word_size = source.get_word_size()
        if next_word_size == word_size:
            return name

        in_word_width = word_size * 8
        in_addr_width = self.get_addr_width(word_size)
        out_word_width = next_word_size * 8
        out_addr_width = self.get_addr_width(next_word_size)
        adapted = name + 'b'
        self.declare_signals(adapted, word_size)
        self.add_code(name + '_adapter : entity work.adapter')
        self.enter()
        self.add_code('generic map(')
        self.enter()
        self.add_code('IN_ADDR_WIDTH => ' + str(in_addr_width) + ',')
        self.add_code('IN_WORD_WIDTH => ' + str(in_word_width) + ',')
        self.add_code('OUT_ADDR_WIDTH => ' + str(out_addr_width) + ',')
        self.add_code('OUT_WORD_WIDTH => ' + str(out_word_width))
        self.leave()
        self.add_code(')')
        self.add_code('port map (')
        self.enter()
        self.add_code('clk => clk,')
        self.add_code('rst => rst,')
        self.add_code('addr => ' + adapted + '_addr,')
        self.add_code('din => ' + adapted + '_din,')
        self.add_code('dout => ' + adapted + '_dout,')
        self.add_code('re => ' + adapted + '_re,')
        self.add_code('we => ' + adapted + '_we,')
        self.add_code('mask => ' + adapted + '_mask,')
        self.add_code('ready => ' + adapted + '_ready,')
        self.add_code('maddr => ' + name + '_addr,')
        self.add_code('min => ' + name + '_dout,')
        self.add_code('mout => ' + name + '_din,')
        self.add_code('mre => ' + name + '_re,')
        self.add_code('mwe => ' + name + '_we,')
        self.add_code('mmask => ' + name + '_mask,')
        self.add_code('mready => ' + name + '_ready')
        self.leave()
        self.add_code(');')
        self.leave()
        return adapted

    def generate(self, ml):
        """Generate VHDL for the specified memory list."""
        self.result = ''

        # Generate subsystems.
        for m in ml.active_memories():
            self.enter()
            m.generate(self, None)
            self.leave()
            assert(self.indent == 0)

        # Generate the top level.
        self.append("library ieee;")
        self.append("use ieee.std_logic_1164.all;")
        self.append("use ieee.numeric_std.all;")
        self.append("entity mem is")
        self.enter()
        self.append("port (")
        self.enter()
        self._emit_downstream_ports(ml)
        self._emit_upstream_ports(ml)
        self.append("clk : in std_logic;")
        self.append("rst : in std_logic")
        self.leave()
        self.append(");")
        self.leave()
        self.append("end mem;")
        self.append("architecture rtl of mem is")
        self.enter()
        self._emit_downstream_signals(ml)
        self.leave()
        self.result += self.sigs
        self.append("begin")
        self.result += self.code
        self.enter()
        self._connect_downstream_ports(ml)
        self._connect_upstream_ports(ml)
        self.leave()
        self.append("end rtl;")
        assert(self.indent == 0)
        return self.result

    def _get_interface_name(self, mem):
        main = mem.get_main()
        while mem.get_next() is not main:
            mem = mem.get_next()
        main = mem.get_next()
        return self.get_name(mem, main)

    def _emit_downstream_ports(self, ml):
        mem = ml.main_memory
        word_size = mem.get_word_size()
        word_width = word_size * 8
        addr_width = self.get_addr_width(word_size)
        pname = "port0"
        addr_range = str(addr_width - 1) + ' downto 0'
        word_range = str(word_width - 1) + ' downto 0'
        mask_top = str(word_size - 1)
        self.append(pname + '_addr : out std_logic_vector(' +
                    addr_range + ');')
        self.append(pname + '_din : in std_logic_vector(' +
                    word_range + ');')
        self.append(pname + '_dout : out std_logic_vector(' +
                    word_range + ');')
        self.append(pname + "_re : out std_logic;")
        self.append(pname + "_we : out std_logic;")
        self.append(pname + "_mask : out " +
                    "std_logic_vector(" + mask_top + " downto 0);")
        self.append(pname + "_ready : in std_logic;")

    def _emit_downstream_signals(self, ml):
        main = ml.main_memory
        word_size = main.get_word_size()
        word_width = word_size * 8
        port_count = len(list(ml.active_memories()))
        port_spec = 'std_logic_vector(' + str(port_count - 1) + ' downto 0)'
        bus_width = word_width * port_count
        bus_spec = 'std_logic_vector(' + str(bus_width - 1) + ' downto 0)'
        addr_width = self.get_addr_width(word_size) * port_count
        addr_spec = 'std_logic_vector(' + str(addr_width - 1) + ' downto 0)'
        mask_width = word_size * port_count
        mask_spec = 'std_logic_vector(' + str(mask_width - 1) + ' downto 0)'
        pname = 'port0'
        self.append('signal ' + pname + '_addr_vec : ' + addr_spec + ';')
        self.append('signal ' + pname + '_dout_vec : ' + bus_spec + ';')
        self.append('signal ' + pname + '_din_vec : ' + bus_spec + ';')
        self.append('signal ' + pname + '_mask_vec : ' + mask_spec + ';')
        self.append('signal ' + pname + '_re_vec : ' + port_spec + ';')
        self.append('signal ' + pname + '_we_vec : ' + port_spec + ';')
        self.append('signal ' + pname + '_ready_vec : ' + port_spec + ';')

    def _connect_downstream_ports(self, ml):

        main = ml.main_memory
        word_size = main.get_word_size()
        word_width = word_size * 8

        pname = 'port0'
        addr_ports = []
        din_ports = []
        mask_ports = []
        for i, mem in enumerate(ml.active_memories()):
            name = self._get_interface_name(mem)
            vec = '_vec(' + str(i) + ')'
            addr_ports.insert(0, name + '_addr')
            din_ports.insert(0, name + '_din')
            mask_ports.insert(0, name + '_mask')
            dout_bottom = i * word_width
            dout_top = dout_bottom + word_width - 1
            dout_range = str(dout_top) + ' downto ' + str(dout_bottom)
            self.append(name + '_dout <= ' + pname +
                        '_dout_vec(' + dout_range + ');')
            self.append(name + '_ready <= ' + pname + '_ready' + vec + ';')
            self.append(pname + '_re' + vec + ' <= ' + name + '_re' + ';')
            self.append(pname + '_we' + vec + ' <= ' + name + '_we' + ';')

        self.append(pname + '_addr_vec <= ' + '&'.join(addr_ports) + ';')
        self.append(pname + '_din_vec <= ' + '&'.join(din_ports) + ';')
        self.append(pname + '_mask_vec <= ' + '&'.join(mask_ports) + ';')
        port_count = len(addr_ports)
        addr_width = self.get_addr_width(word_size)
        self.append('main_arbiter : entity work.arbiter')
        self.enter()
        self.append('generic map(')
        self.enter()
        self.append('PORT_COUNT => ' + str(port_count) + ',')
        self.append('ADDR_WIDTH => ' + str(addr_width) + ',')
        self.append('WORD_WIDTH => ' + str(word_width))
        self.leave()
        self.append(')')
        self.append('port map (')
        self.enter()
        self.append('clk => clk,')
        self.append('rst => rst,')
        self.append('addr => ' + pname + '_addr_vec,')
        self.append('din => ' + pname + '_din_vec,')
        self.append('dout => ' + pname + '_dout_vec,')
        self.append('re => ' + pname + '_re_vec,')
        self.append('we => ' + pname + '_we_vec,')
        self.append('mask => ' + pname + '_mask_vec,')
        self.append('ready => ' + pname + '_ready_vec,')
        self.append('maddr => ' + pname + '_addr,')
        self.append('mout => ' + pname + '_dout,')
        self.append('min => ' + pname + '_din,')
        self.append('mre => ' + pname + '_re,')
        self.append('mwe => ' + pname + '_we,')
        self.append('mmask => ' + pname + '_mask,')
        self.append('mready => ' + pname + '_ready')
        self.leave()
        self.append(');')
        self.leave()

    def _emit_upstream_ports(self, ml):
        for m in ml.active_memories():
            if isinstance(m, FIFO):
                name = "fifo" + str(m.index)
            else:
                name = "subsystem" + str(m.index)
            word_size = m.get_word_size()
            word_width = word_size * 8
            addr_width = self.get_addr_width(word_size)
            prefix = 'std_logic_vector('
            suffix = ' downto 0);'
            word_str = prefix + str(word_width - 1) + suffix
            addr_str = prefix + str(addr_width - 1) + suffix
            mask_str = prefix + str((word_width // 8) - 1) + suffix
            self.append(name + '_addr' + ' : in ' + addr_str)
            self.append(name + '_in' + ' : in ' + word_str)
            self.append(name + '_out' + ' : out ' + word_str)
            self.append(name + '_re' + ' : in std_logic;')
            self.append(name + '_we' + ' : in std_logic;')
            self.append(name + '_mask' + ' : in ' + mask_str)
            self.append(name + '_ready' + ' : out std_logic;')

    def _connect_upstream_ports(self, ml):
        byte_offset = 0
        main_word_size = ml.main_memory.get_word_size()
        for m in ml.active_memories():
            word_size = m.get_word_size()
            addr_width = self.get_addr_width(word_size)
            if isinstance(m, FIFO):
                sname = "fifo" + str(m.index)
            else:
                sname = "subsystem" + str(m.index)
            name = m.get_id()
            word_offset = byte_offset // word_size
            self.append(name + '_addr <= std_logic_vector(unsigned(' +
                        sname + '_addr) + to_unsigned(' + str(word_offset) +
                        ', ' + str(addr_width) + '));')
            self.append(name + '_din <= ' + sname + '_in;')
            self.append(sname + '_out <= ' + name + '_dout;')
            self.append(name + '_re <= ' + sname + '_re;')
            self.append(name + '_we <= ' + sname + '_we;')
            self.append(name + '_mask <= ' + sname + '_mask;')
            self.append(sname + '_ready <= ' + name + '_ready;')
            byte_offset += m.total_size()
