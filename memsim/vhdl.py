
class VHDLGenerator(object):
    """Class used to generate VHDL code."""

    def __init__(self):
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
        self.code += "  " * self.indent + s + '\n'

    def add_sig(self, s):
        """Add to the signal section."""
        self.sigs += "  " * self.indent + s + '\n'

    def append(self, s):
        """Append to the result (clients should use add_code or add_sig)."""
        self.result += "  " * self.indent + s + '\n'

    def declare_signal(self, name, width_str=None):
        """Declare a signal."""
        if width_str is None:
            ts = "std_logic"
        else:
            ts = "std_logic_vector(" + width_str + " - 1 downto 0)"
        self.add_sig("signal " + name + " : " + ts + ";")

    def declare_signals(self, name, word_size):
        """Declare the signals for communication between componeents."""
        word_width = word_size * 8
        self.declare_signal(name + "_addr", "ADDR_WIDTH")
        self.declare_signal(name + "_din", str(word_width))
        self.declare_signal(name + "_dout", str(word_width))
        self.declare_signal(name + "_re")
        self.declare_signal(name + "_we")
        self.declare_signal(name + "_mask", str(word_width // 8))
        self.declare_signal(name + "_ready")

    def generate(self, mach, mem):
        """Generate VHDL for the specified machine and memory."""
        self.result = ''
        self.enter()
        mem.generate(self, mach)
        self.leave()
        assert(self.indent == 0)
        addr_width = mach.addr_bits - mach.word_bits
        self.append("library ieee;")
        self.append("use ieee.std_logic_1164.all;")
        self.append("use ieee.numeric_std.all;")
        self.append("entity mem is")
        self.enter()
        self.append("generic (")
        self.enter()
        self.append("ADDR_WIDTH : in natural := " + str(addr_width) + ";")
        self.append("WORD_WIDTH : in natural := " + str(mach.word_size * 8))
        self.leave()
        self.append(");")
        self.append("port (")
        self.enter()
        self.append("clk : in std_logic;")
        self.append("rst : in std_logic;")
        ports = mem.get_ports(mach)
        for p in xrange(len(ports)):
            pname = "port" + str(p)
            addr_top = str(ports[p].addr_width - 1)
            word_top = str(ports[p].word_size * 8 - 1)
            mask_top = str(ports[p].word_size - 1)
            self.append(pname + "_addr : out " +
                        "std_logic_vector(" + addr_top + " downto 0);")
            self.append(pname + "_din : in " +
                        "std_logic_vector(" + word_top + " downto 0);")
            self.append(pname + "_dout : out " +
                        "std_logic_vector(" + word_top + " downto 0);")
            self.append(pname + "_re : out std_logic;")
            self.append(pname + "_we : out std_logic;")
            self.append(pname + "_mask : out " +
                        "std_logic_vector(" + mask_top + " downto 0);")
            self.append(pname + "_ready : in std_logic;")
        self.append("addr : in std_logic_vector(ADDR_WIDTH - 1 downto 0);")
        self.append("din : in std_logic_vector(WORD_WIDTH - 1 downto 0);")
        self.append("dout : out std_logic_vector(WORD_WIDTH - 1 downto 0);")
        self.append("re : in std_logic;")
        self.append("we : in std_logic;")
        self.append("mask : in std_logic_vector((WORD_WIDTH / 8) - 1 " +
                    "downto 0);")
        self.append("ready : out std_logic")
        self.leave()
        self.append(");")
        self.leave()
        self.append("end mem;")
        self.append("architecture rtl of mem is")
        self.result += self.sigs
        self.append("begin")
        self.result += self.code
        self.enter()
        sname = mem.get_id()
        self.append(sname + "_addr <= addr;")
        self.append(sname + "_din <= din;")
        self.append("dout <= " + sname + "_dout;")
        self.append(sname + "_re <= re;")
        self.append(sname + "_we <= we;")
        self.append(sname + "_mask <= mask;")
        self.append("ready <= " + sname + "_ready;")
        for p in xrange(len(ports)):
            pname = "port" + str(p)
            oname = ports[p].name
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
