

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

    def generate_next(self, word_size, mem):
        """Generate VHDL for the next memory in the subsystem.

        Returns the base name for the signals to connect.
        """
        name = mem.get_id()
        mem.generate(self)
        next_word_size = mem.get_word_size()
        if next_word_size == word_size:
            return name
        in_word_width = word_size * 8
        in_addr_width = self.get_addr_width(word_size)
        out_word_width = next_word_size * 8
        out_addr_width = self.get_addr_width(next_word_size)
        adapted = name + 'b'
        self.declare_signals(adapted, next_word_size)
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
        self.add_code('addr => ' + name + '_addr,')
        self.add_code('din => ' + name + '_din,')
        self.add_code('dout => ' + name + '_dout,')
        self.add_code('re => ' + name + '_re,')
        self.add_code('we => ' + name + '_we,')
        self.add_code('mask => ' + name + '_mask,')
        self.add_code('ready => ' + name + '_ready,')
        self.add_code('maddr => ' + adapted + '_addr,')
        self.add_code('min => ' + adapted + '_dout,')
        self.add_code('mout => ' + adapted + '_din,')
        self.add_code('mre => ' + adapted + '_re,')
        self.add_code('mwe => ' + adapted + '_we,')
        self.add_code('mmask => ' + adapted + '_mask,')
        self.add_code('mready => ' + adapted + '_ready')
        self.leave()
        self.add_code(');')
        self.leave()
        return adapted

    def create_arbiter(self, word_size, memories):
        assert(False)

    def generate(self, ml):
        """Generate VHDL for the specified memory list."""
        self.result = ''

        # Generate subsystems.
        names = []
        memories = list(ml.all_memories())
        for m in memories:
            self.enter()
            names.append(m.generate(self))
            self.leave()
            assert(self.indent == 0)

        # Create the arbiter if necessary.
        if len(memories) == 1:
            sname = names[0]
        else:
            word_size = ml.main_memory.get_word_size()
            sname = self.create_arbiter(word_size, memories)

        # Generate the top level.
        self.append("library ieee;")
        self.append("use ieee.std_logic_1164.all;")
        self.append("use ieee.numeric_std.all;")
        self.append("entity mem is")
        self.enter()
        self.append("port (")
        self.enter()
        self.append("clk : in std_logic;")
        self.append("rst : in std_logic;")
        ports = ml.main_memory.get_ports(self.machine)
        for i, p in enumerate(ports):
            pname = "port" + str(i)
            addr_range = str(p.addr_width - 1) + ' downto 0'
            word_range = str(p.word_size * 8 - 1) + ' downto 0'
            mask_top = str(p.word_size - 1)
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
        word_size = ml.main_memory.get_word_size()
        word_width = word_size * 8
        addr_width = self.get_addr_width(word_size)
        self.append('addr : in std_logic_vector(' + str(addr_width - 1) +
                    ' downto 0);')
        self.append('din : in std_logic_vector(' + str(word_width - 1) +
                    ' downto 0);')
        self.append('dout : out std_logic_vector(' + str(word_width - 1) +
                    ' downto 0);')
        self.append('re : in std_logic;')
        self.append('we : in std_logic;')
        self.append('mask : in std_logic_vector(' +
                    str((word_width // 8) - 1) + ' downto 0);')
        self.append('ready : out std_logic')
        self.leave()
        self.append(");")
        self.leave()
        self.append("end mem;")
        self.append("architecture rtl of mem is")
        self.result += self.sigs
        self.append("begin")
        self.result += self.code
        self.enter()
        self.append(sname + "_addr <= addr;")
        self.append(sname + "_din <= din;")
        self.append("dout <= " + sname + "_dout;")
        self.append(sname + "_re <= re;")
        self.append(sname + "_we <= we;")
        self.append(sname + "_mask <= mask;")
        self.append("ready <= " + sname + "_ready;")
        for i, p in enumerate(ports):
            pname = "port" + str(i)
            oname = p.name
            self.append(pname + "_addr <= " + oname + "_addr;")
            self.append(pname + "_dout <= " + oname + "_din;")
            self.append(oname + "_dout <= " + pname + "_din;")
            self.append(pname + "_re <= " + oname + "_re;")
            self.append(pname + "_we <= " + oname + "_we;")
            self.append(pname + "_mask <= " + oname + "_mask;")
            self.append(oname + "_ready <= " + pname + "_ready;")
        self.leave()
        self.append("end rtl;")
        assert(self.indent == 0)
        return self.result
