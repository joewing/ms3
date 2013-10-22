-- Input: (cache (line_size 32)(line_count 16)(associativity 16)(latency 3)(policy plru)(write_back false)(memory (ram (latency 10)(word_size 8)(word_count 0))))
-- Simplified: (cache (line_size 32)(line_count 16)(associativity 16)(latency 3)(policy plru)(write_back false)(memory (ram (latency 10)(word_size 8)(word_count 0))))
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity mem is
   generic (
      ADDR_WIDTH : in natural := 29;
      WORD_WIDTH : in natural := 64
   );
   port (
      clk     : in  std_logic;
      rst     : in  std_logic;
      port0_addr : out std_logic_vector(ADDR_WIDTH - 1 downto 0);
      port0_din  : in  std_logic_vector(64 - 1 downto 0);
      port0_dout : out std_logic_vector(64 - 1 downto 0);
      port0_re : out std_logic;
      port0_we : out std_logic;
      port0_mask : out std_logic_vector(7 downto 0);
      port0_ready : in std_logic;
      addr    : in  std_logic_vector(ADDR_WIDTH - 1 downto 0);
      din     : in  std_logic_vector(WORD_WIDTH - 1 downto 0);
      dout    : out std_logic_vector(WORD_WIDTH - 1 downto 0);
      re      : in  std_logic;
      we      : in  std_logic;
      mask    : in  std_logic_vector((WORD_WIDTH / 8) - 1 downto 0);
      ready   : out std_logic
   );
end mem;

architecture mem_arch of mem is
   signal m0_addr : std_logic_vector(ADDR_WIDTH - 1 downto 0);
   signal m0_din : std_logic_vector(63 downto 0);
   signal m0_dout : std_logic_vector(63 downto 0);
   signal m0_re : std_logic;
   signal m0_we : std_logic;
   signal m0_mask : std_logic_vector(7 downto 0);
   signal m0_ready : std_logic;
   signal p0_addr : std_logic_vector(ADDR_WIDTH - 1 downto 0);
   signal p0_din : std_logic_vector(63 downto 0);
   signal p0_dout : std_logic_vector(63 downto 0);
   signal p0_re : std_logic;
   signal p0_we : std_logic;
   signal p0_mask : std_logic_vector(7 downto 0);
   signal p0_ready : std_logic;
   signal m1_addr : std_logic_vector(ADDR_WIDTH - 1 downto 0);
   signal m1_din : std_logic_vector(63 downto 0);
   signal m1_dout : std_logic_vector(63 downto 0);
   signal m1_re : std_logic;
   signal m1_we : std_logic;
   signal m1_mask : std_logic_vector(7 downto 0);
   signal m1_ready : std_logic;
begin
p0_addr <= m0_addr;
p0_din <= m0_din;
m0_dout <= p0_dout;
p0_re <= m0_re;
p0_we <= m0_we;
p0_mask <= m0_mask;
m0_ready <= p0_ready;
m1_inst : entity work.cache
   generic map (
      ADDR_WIDTH      => ADDR_WIDTH,
      WORD_WIDTH      => 64,
      LINE_SIZE_BITS  => 2,
      LINE_COUNT_BITS => 0,
      ASSOC_BITS      => 4,
      REPLACEMENT     => 3,
      WRITE_POLICY    => 1
   )
   port map (
      clk      => clk,
      rst      => rst,
      addr     => m1_addr,
      din      => m1_din,
      dout     => m1_dout,
      re       => m1_re,
      we       => m1_we,
      mask     => m1_mask,
      ready    => m1_ready,
      maddr    => m0_addr,
      min      => m0_dout,
      mout     => m0_din,
      mre      => m0_re,
      mwe      => m0_we,
      mmask    => m0_mask,
      mready   => m0_ready
   );
   m1_addr <= addr;
   m1_din <= din;
   dout <= m1_dout;
   m1_re <= re;
   m1_we <= we;
   m1_mask <= mask;
   ready <= m1_ready;
port0_addr <= p0_addr;
port0_dout <= p0_din;
p0_dout <= port0_din;
port0_re <= p0_re;
port0_we <= p0_we;
port0_mask <= p0_mask;
p0_ready <= port0_ready;
end mem_arch;

