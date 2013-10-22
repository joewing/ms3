
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity cache_syn is
   port (
      clk      : in std_logic;
      rst      : in std_logic;
      addr     : in std_logic_vector(32 - 1 downto 0);
      din      : in std_logic_vector(32 - 1 downto 0);
      dout     : out std_logic_vector(32 - 1 downto 0);
      re       : in std_logic;
      we       : in std_logic;
      ready    : out std_logic;
      maddr    : out std_logic_vector(32 - 1 downto 0);
      mout     : out std_logic_vector(32 - 1 downto 0);
      min      : in std_logic_vector(32 - 1 downto 0);
      mre      : out std_logic;
      mwe      : out std_logic;
      mready   : in std_logic
   );
end cache_syn;

architecture cache_syn_arch of cache_syn is

begin

   c1 : entity work.cache
      generic map (
         ADDR_WIDTH        => 32,
         WORD_WIDTH        => 32,
         LINE_SIZE_BITS    => 1,
         LINE_COUNT_BITS   => 8,
         ASSOC_BITS        => 2
      )
      port map (
         clk      => clk,
         rst      => rst,
         addr     => addr,
         din      => din,
         dout     => dout,
         re       => re,
         we       => we,
         ready    => ready,
         maddr    => maddr,
         mout     => mout,
         min      => min,
         mre      => mre,
         mwe      => mwe,
         mready   => mready
      );

end cache_syn_arch;
