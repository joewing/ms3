library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity adaptertb1 is
end entity;

architecture tb of adaptertb1 is

    signal clk      : std_logic    := '0';
    signal rst      : std_logic    := '1';
    signal cycles   : integer      := 0;

    constant IN_ADDR_WIDTH  : integer := 32;
    constant IN_WORD_WIDTH  : integer := 32;
    constant OUT_ADDR_WIDTH : integer := 33;
    constant OUT_WORD_WIDTH : integer := 16;

    signal mem_addr     : std_logic_vector(IN_ADDR_WIDTH - 1 downto 0);
    signal mem_din      : std_logic_vector(IN_WORD_WIDTH - 1 downto 0);
    signal mem_dout     : std_logic_vector(IN_WORD_WIDTH - 1 downto 0);
    signal mem_mask     : std_logic_vector(IN_WORD_WIDTH / 8 - 1 downto 0);
    signal mem_re       : std_logic;
    signal mem_we       : std_logic;
    signal mem_ready    : std_logic;

    signal ram_addr     : std_logic_vector(OUT_ADDR_WIDTH - 1 downto 0);
    signal ram_in       : std_logic_vector(OUT_WORD_WIDTH - 1 downto 0);
    signal ram_out      : std_logic_vector(OUT_WORD_WIDTH - 1 downto 0);
    signal ram_mask     : std_logic_vector(OUT_WORD_WIDTH / 8 - 1 downto 0);
    signal ram_re       : std_logic;
    signal ram_we       : std_logic;
    signal ram_ready    : std_logic;

    procedure cycle(signal clk : out std_logic) is
    begin
        clk <= '1';
        wait for 10 ns;
        clk <= '0';
        wait for 10 ns;
    end cycle;

    procedure wait_ready(signal clk : out std_logic;
                         signal rdy : in std_logic) is
    begin
        while rdy = '0' loop
            cycle(clk);
        end loop;
    end wait_ready;

    procedure update(signal clk : out std_logic;
                          signal ena : out std_logic;
                          signal rdy : in std_logic) is
    begin
        wait_ready(clk, rdy);
        ena <= '1';
        cycle(clk);
        ena <= '0';
        cycle(clk);
        wait_ready(clk, rdy);
    end update;

begin

    ram1 : entity work.ram
        generic map (
            ADDR_WIDTH  => OUT_ADDR_WIDTH,
            WORD_WIDTH  => OUT_WORD_WIDTH,
            SIZE        => 65536,
            LATENCY     => 8,
            BURST       => 0
        )
        port map (
            clk     => clk,
            rst     => rst,
            addr    => ram_addr,
            din     => ram_in,
            dout    => ram_out,
            re      => ram_re,
            we      => ram_we,
            mask    => ram_mask,
            ready   => ram_ready
        );

    dut : entity work.adapter
        generic map (
            IN_ADDR_WIDTH   => IN_ADDR_WIDTH,
            IN_WORD_WIDTH   => IN_WORD_WIDTH,
            OUT_ADDR_WIDTH  => OUT_ADDR_WIDTH,
            OUT_WORD_WIDTH  => OUT_WORD_WIDTH
        )
        port map (
            clk     => clk,
            rst     => rst,

            addr    => mem_addr,
            din     => mem_din,
            dout    => mem_dout,
            re      => mem_re,
            we      => mem_we,
            mask    => mem_mask,
            ready   => mem_ready,

            maddr   => ram_addr,
            mout    => ram_in,
            min     => ram_out,
            mre     => ram_re,
            mwe     => ram_we,
            mmask   => ram_mask,
            mready  => ram_ready
        );

    process
    begin

        -- Reset
        mem_addr <= (others => 'X');
        mem_we    <= '0';
        mem_re    <= '0';
        mem_mask <= (others => '1');
        mem_din  <= (others => 'X');
        cycle(clk);
        rst <= '0';

        assert mem_ready = '1' report "not ready" severity failure;

        -- Write 0
        mem_addr <= x"00000000";
        mem_din  <= x"FFFFFFFF";
        update(clk, mem_we, mem_ready);

        -- Write 1
        mem_addr <= x"00000001";
        mem_din <= x"01234567";
        update(clk, mem_we, mem_ready);

        -- Read 0
        mem_addr <= x"00000000";
        update(clk, mem_re, mem_ready);
        assert mem_dout = x"FFFFFFFF"
            report "read failed" severity failure;

        -- Read 1
        mem_addr <= x"00000001";
        update(clk, mem_re, mem_ready);
        assert mem_dout = x"01234567"
            report "read failed" severity failure;

        -- Masked write.
        mem_mask <= "0010";
        mem_addr <= x"00000000";
        mem_din <= x"00000000";
        mem_we <= '1';
        cycle(clk);
        mem_we <= '0';
        mem_mask <= "1111";
        cycle(clk);
        wait_ready(clk, mem_ready);

        -- Read.
        mem_addr <= x"00000000";
        update(clk, mem_re, mem_ready);
        assert mem_dout = x"FFFF00FF"
            report "read failed" severity failure;

        wait_ready(clk, mem_ready);
        report "cycles: " & integer'image(cycles);
        wait;

    end process;

    process(clk)
    begin
        if rising_edge(clk) and rst = '0' then
            if ram_re = '1' then
                assert mem_ready = '0'
                    report "read when not ready" severity failure;
                assert mem_we = '0'
                    report "read and write" severity failure;
            elsif ram_we = '1' then
                assert mem_ready = '0'
                    report "write when not ready" severity failure;
            end if;
        end if;
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

end tb;
