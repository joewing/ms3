library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity fifo is
    generic (
        WIDTH       : natural := 8;
        ADDR_WIDTH  : natural := 32;
        DEPTH       : natural := 1      -- Must be a power of 2.
    );
    port (
        clk         : in  std_logic;
        rst         : in  std_logic;
        din         : in  std_logic_vector(WIDTH - 1 downto 0);
        dout        : out std_logic_vector(WIDTH - 1 downto 0);
        re          : in  std_logic;
        we          : in  std_logic;
        avail       : out std_logic;
        full        : out std_logic;
        mem_addr    : out std_logic_vector(ADDR_WIDTH - 1 downto 0);
        mem_in      : in  std_logic_vector(WIDTH - 1 downto 0);
        mem_out     : out std_logic_vector(WIDTH - 1 downto 0);
        mem_mask    : out std_logic_vector(WIDTH / 8 - 1 downto 0);
        mem_re      : out std_logic;
        mem_we      : out std_logic;
        mem_ready   : in  std_logic
    );
end fifo;

architecture rtl of fifo is

    signal read_ptr         : natural;
    signal write_ptr        : natural;
    signal count            : natural;
    signal started          : std_logic;
    signal read_pending     : std_logic;
    signal write_pending    : std_logic;
    signal read_outstanding : std_logic;

begin

    gen_register : if DEPTH = 1 generate
        process(clk)
        begin
            if rising_edge(clk) then
                if rst = '1' then
                    count <= 0;
                else
                    if count = 0 and we = '1' then
                        dout <= din;
                        count <= 1;
                    end if;
                    if count = 1 and re = '1' then
                        count <= 0;
                    end if;
                end if;
            end if;
        end process;
        full <= '0' when count = 0 else '1';
        avail <= '0' when count = 0 else '1';
        mem_re <= '0';
        mem_we <= '0';
        mem_addr <= (others => 'X');
        mem_out <= (others => 'X');
        mem_mask <= (others => 'X');
    end generate;

    gen_fifo : if DEPTH > 1 generate
        process(clk)
        begin
            if rising_edge(clk) then
                mem_re <= '0';
                mem_we <= '0';
                started <= '0';
                if rst = '1' then
                    read_ptr <= 0;
                    write_ptr <= 0;
                    count <= 0;
                    read_pending <= '0';
                    write_pending <= '0';
                    read_outstanding <= '0';
                else
                    if mem_ready = '1' and started = '0' then
                        if read_outstanding = '1' then
                            read_outstanding <= '0';
                            read_pending <= '1';
                            dout <= mem_in;
                        end if;
                        if count /= 0 and read_pending = '0'
                            and read_outstanding = '0' then
                            mem_re <= '1';
                            started <= '1';
                            mem_addr <= std_logic_vector(
                                to_unsigned(read_ptr, ADDR_WIDTH));
                            read_ptr <= (read_ptr + 1) mod DEPTH;
                            count <= count - 1;
                            read_outstanding <= '1';
                        elsif count /= DEPTH and write_pending = '1' then
                            mem_we <= '1';
                            started <= '1';
                            mem_addr <= std_logic_vector(
                                to_unsigned(write_ptr, ADDR_WIDTH));
                            write_ptr <= (write_ptr + 1) mod DEPTH;
                            count <= count + 1;
                            write_pending <= '0';
                        end if;
                    end if;
                    if we = '1' and write_pending = '0' then
                        mem_out <= din;
                        write_pending <= '1';
                    end if;
                    if re = '1' and read_pending = '0' then
                        read_pending <= '0';
                    end if;
                end if;
            end if;
        end process;
        mem_mask <= (others => '1');
    end generate;

end rtl;
