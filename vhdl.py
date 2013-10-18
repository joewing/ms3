
class VHDL:
   """Class used to generate VHDL code."""

   def __init__(self):
      self.sigs = ''
      self.code = ''
      self.indent = 0

   def enter(self):
      self.indent += 1

   def leave(self):
      assert(self.indent > 0)
      self.indent -= 1

   def add_code(self, s):
      self.code += "  " * self.indent + s + '\n'

   def add_sig(self, s):
      self.sigs += "  " * self.indent + s + '\n'

   def generate(self, mach, mem):
      """Generate VHDL for the specified machine and memory."""
      self.enter()
      mem.generate(self, mach)
      self.leave()
      assert(self.indent == 0)
      result  = "library ieee;\n"
      result += "use ieee.std_logic_1164.all;\n"
      result += "use ieee.numeric_std.all;\n"
      result += "entity memory is\n"
      result += "  generic (\n"
      result += "    ADDR_WIDTH : in natural := " + str(mach.addr_bits) + ";\n"
      result += "    WORD_WIDTH : in natural := "
      result += str(mach.word_size * 8) + ";\n"
      result += "  );\n"
      result += "  port (\n"
      result += "    clk   : in  std_logic;\n"
      result += "    rst   : in  std_logic;\n"
      result += "    addr  : in  std_logic_vector(ADDR_WIDTH - 1 downto 0);\n"
      result += "    din   : in  std_logic_vector(WORD_WIDTH - 1 downto 0);\n"
      result += "    dout  : out std_logic_vector(WORD_WIDTH - 1 downto 0);\n"
      result += "    re    : in  std_logic;\n"
      result += "    we    ; in  std_logic;\n"
      result += "    mask  : in  std_logic_vector((WORD_WIDTH / 8) - 1 "
      result +=                                   "downto 0);\n"
      result += "    ready : out std_logic;\n"
      result += "  );\n"
      result += "end memory;\n"
      result += "architecture rtl of memory is\n"
      result += self.sigs
      result += "begin\n"
      result += self.code
      result += "end rtl;\n"
      return result

