library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity fifotb is
end entity;

architecture tb_arch of fifotb is

    procedure cycle(signal clk : out std_logic) is
    begin
        clk <= '1';
        wait for 10 ns;
        clk <= '0';
        wait for 10 ns;
    end cycle;

    signal clk      : std_logic    := '0';
    signal rst      : std_logic    := '1';
    signal cycles   : integer      := 0;

    signal din      : std_logic_vector(31 downto 0);
    signal dout     : std_logic_vector(31 downto 0);
    signal re       : std_logic;
    signal we       : std_logic;
    signal avail    : std_logic;
    signal full     : std_logic;

begin

    bf : entity work.fifo
        generic map (
            WIDTH           => 32,
            ADDR_WIDTH      => 4,
            BRAM            => true,
            DEPTH           => 16
        )
        port map (
            clk         => clk,
            rst         => rst,
            din         => din,
            dout        => dout,
            re          => re,
            we          => we,
            avail       => avail,
            full        => full,
            mem_addr    => open,
            mem_in      => (others => 'X'),
            mem_out     => open,
            mem_mask    => open,
            mem_re      => open,
            mem_we      => open,
            mem_ready   => 'X'
        );

    process
    begin

        -- Reset
        din <= (others => 'X');
        re <= '0';
        we <= '0';
        cycle(clk);
        rst <= '0';
        cycle(clk);

        assert full = '0' report "full after reset" severity failure;
        assert avail = '0' report "avail after reset" severity failure;

        -- Insert an item.
        din <= x"00000001";
        we <= '1';
        cycle(clk);
        we <= '0';
        cycle(clk);
        cycle(clk);
        assert avail = '1' report "not avail after insert" severity failure;
        assert full = '0' report "full after insert" severity failure;
        assert dout = x"00000001" report "bad read" severity failure;

        -- Remove the item.
        re <= '1';
        cycle(clk);
        re <= '0';
        assert avail = '0' report "not avail after remove" severity failure;
        assert full = '0' report "full after remove" severity failure;

        -- Fill the FIFO.
        for i in 0 to 15 loop
            we <= '1';
            din <= std_logic_vector(to_unsigned(i, 32));
            cycle(clk);
            we <= '0';
            cycle(clk);
            assert full = '0' report "full after insert" severity failure;
        end loop;
        we <= '1';
        din <= x"00000010";
        cycle(clk);
        we <= '0';
        assert full = '1' report "not full 1" severity failure;
        cycle(clk);
        assert full = '1' report "not full 2" severity failure;
        cycle(clk);
        assert full = '1' report "not full 3" severity failure;

        -- Drain the FIFO.
        for i in 0 to 16 loop
            assert avail = '1' report "not available" severity failure;
            assert unsigned(dout) = to_unsigned(i, 32)
                report "unexpected output" severity failure;
            re <= '1';
            cycle(clk);
            re <= '0';
            cycle(clk);
            cycle(clk);
        end loop;

        cycle(clk);
        report "cycles: " & integer'image(cycles);
        wait;

    end process;

    process(clk)
    begin
        if rising_edge(clk) then
            if rst = '1' then
                cycles <= 0;
            else
                cycles <= cycles + 1;
            end if;
        end if;
    end process;

end tb_arch;
