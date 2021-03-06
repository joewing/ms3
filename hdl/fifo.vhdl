library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
entity bram_fifo is
    generic (
        WIDTH       : natural := 8;
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
        full        : out std_logic
    );
end bram_fifo;

architecture rtl of bram_fifo is

    subtype item_type is std_logic_vector(WIDTH - 1 downto 0);
    type item_array is array(0 to DEPTH - 1) of item_type;

    signal data             : item_array;
    signal read_ptr         : natural;
    signal write_ptr        : natural;
    signal count            : natural;
    signal read_ready       : std_logic;

begin

    avail <= read_ready;
    full <= '1' when count = DEPTH else '0';

    process(clk)
        variable do_read : boolean;
        variable do_write : boolean;
    begin
        if rising_edge(clk) then
            do_read := read_ready = '0' and count /= 0;
            do_write := we = '1' and count /= DEPTH;
            if rst = '1' then
                write_ptr <= 0;
                read_ptr <= 0;
                count <= 0;
                read_ready <= '0';
            else
                if do_write and do_read then
                    data(write_ptr) <= din;
                    dout <= data(read_ptr);
                    write_ptr <= (write_ptr + 1) mod DEPTH;
                    read_ptr <= (read_ptr + 1) mod DEPTH;
                elsif do_write then
                    data(write_ptr) <= din;
                    write_ptr <= (write_ptr + 1) mod DEPTH;
                    count <= count + 1;
                elsif do_read then
                    dout <= data(read_ptr);
                    read_ptr <= (read_ptr + 1) mod DEPTH;
                    count <= count - 1;
                end if;
                if do_read then
                    read_ready <= '1';
                else
                    read_ready <= read_ready and not re;
                end if;
            end if;
        end if;
    end process;

end rtl;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
entity fifo is
    generic (
        WIDTH       : natural := 8;
        ADDR_WIDTH  : natural := 32;
        BRAM        : boolean := false; -- Set if implemented in BRAM.
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

    gen_bram_fifo : if DEPTH > 1 and BRAM generate
        bf : entity work.bram_fifo
            generic map (
                WIDTH => WIDTH,
                DEPTH => DEPTH
            )
            port map (
                clk => clk,
                rst => rst,
                din => din,
                dout => dout,
                re => re,
                we => we,
                avail => avail,
                full => full
            );
        mem_re <= '0';
        mem_we <= '0';
        mem_addr <= (others => 'X');
        mem_out <= (others => 'X');
        mem_mask <= (others => 'X');
    end generate;

    gen_fifo : if DEPTH > 1 and not BRAM generate
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
                        elsif count /= DEPTH and write_pending = '1'
                            and read_outstanding = '0' then
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
                    if re = '1' and read_pending = '1' then
                        read_pending <= '0';
                    end if;
                end if;
            end if;
        end process;
        full <= write_pending;
        avail <= read_pending;
        mem_mask <= (others => '1');
    end generate;

end rtl;
