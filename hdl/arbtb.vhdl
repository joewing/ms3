library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity arbtb is
end entity;

architecture tb of arbtb is

    signal clk      : std_logic    := '0';
    signal rst      : std_logic    := '1';
    signal cycles   : integer      := 0;

    constant ADDR_WIDTH : integer := 32;
    constant WORD_WIDTH : integer := 32;

    signal mem1_addr    : std_logic_vector(ADDR_WIDTH - 1 downto 0);
    signal mem1_din     : std_logic_vector(WORD_WIDTH - 1 downto 0);
    signal mem1_dout    : std_logic_vector(WORD_WIDTH - 1 downto 0);
    signal mem1_re      : std_logic;
    signal mem1_we      : std_logic;
    signal mem1_mask    : std_logic_vector((WORD_WIDTH / 8) - 1 downto 0);
    signal mem1_ready   : std_logic;

    signal mem2_addr    : std_logic_vector(ADDR_WIDTH - 1 downto 0);
    signal mem2_din     : std_logic_vector(WORD_WIDTH - 1 downto 0);
    signal mem2_dout    : std_logic_vector(WORD_WIDTH - 1 downto 0);
    signal mem2_re      : std_logic;
    signal mem2_we      : std_logic;
    signal mem2_mask    : std_logic_vector((WORD_WIDTH / 8) - 1 downto 0);
    signal mem2_ready   : std_logic;

    signal ram_addr     : std_logic_vector(ADDR_WIDTH - 1 downto 0);
    signal ram_din      : std_logic_vector(WORD_WIDTH - 1 downto 0);
    signal ram_dout     : std_logic_vector(WORD_WIDTH - 1 downto 0);
    signal ram_re       : std_logic;
    signal ram_we       : std_logic;
    signal ram_mask     : std_logic_vector((WORD_WIDTH / 8) - 1 downto 0);
    signal ram_ready    : std_logic;

    signal arb_addr  : std_logic_vector(2 * ADDR_WIDTH - 1 downto 0);
    signal arb_in    : std_logic_vector(2 * WORD_WIDTH - 1 downto 0);
    signal arb_out   : std_logic_vector(2 * WORD_WIDTH - 1 downto 0);
    signal arb_re    : std_logic_vector(1 downto 0);
    signal arb_we    : std_logic_vector(1 downto 0);
    signal arb_mask  : std_logic_vector(2 * WORD_WIDTH / 8 - 1 downto 0);
    signal arb_ready : std_logic_vector(1 downto 0);

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

    procedure update2(signal clk  : out std_logic;
                      signal en1  : out std_logic;
                      signal en2  : out std_logic;
                      signal rdy1 : in  std_logic;
                      signal rdy2 : in  std_logic) is
    begin
        wait_ready(clk, rdy1);
        wait_ready(clk, rdy2);
        en1 <= '1';
        en2 <= '1';
        cycle(clk);
        en1 <= '0';
        en2 <= '0';
        cycle(clk);
        wait_ready(clk, rdy1);
        wait_ready(clk, rdy2);
    end update2;

    function hex_string(bv : std_logic_vector) return string is
        constant len    : natural := ((bv'length + 3) / 4) * 4;
        variable full   : std_logic_vector(len - 1 downto 0);
        variable temp   : std_logic_vector(3 downto 0);
        variable result : string(len / 4 - 1 downto 0);
    begin
        full := std_logic_vector(resize(unsigned(bv), len));
        for i in result'right to result'left loop
            temp := full(3 downto 0);
            case temp is
                when "0000" => result(i) := '0';
                when "0001" => result(i) := '1';
                when "0010" => result(i) := '2';
                when "0011" => result(i) := '3';
                when "0100" => result(i) := '4';
                when "0101" => result(i) := '4';
                when "0110" => result(i) := '6';
                when "0111" => result(i) := '7';
                when "1000" => result(i) := '8';
                when "1001" => result(i) := '9';
                when "1010" => result(i) := 'a';
                when "1011" => result(i) := 'b';
                when "1100" => result(i) := 'c';
                when "1101" => result(i) := 'd';
                when "1110" => result(i) := 'e';
                when "1111" => result(i) := 'f';
                when others => result(i) := '?';
            end case;
            full := "0000" & full(full'length - 1 downto 4);
        end loop;
        return result;
    end hex_string;

begin

    ram1 : entity work.ram
        generic map (
            ADDR_WIDTH  => ADDR_WIDTH,
            WORD_WIDTH  => WORD_WIDTH,
            SIZE        => 65536,
            LATENCY     => 8,
            BURST       => 0
        )
        port map (
            clk         => clk,
            rst         => rst,
            addr        => ram_addr,
            din         => ram_din,
            dout        => ram_dout,
            re          => ram_re,
            we          => ram_we,
            mask        => ram_mask,
            ready       => ram_ready
        );

    arb : entity work.arbiter
        generic map (
            ADDR_WIDTH => ADDR_WIDTH,
            WORD_WIDTH => WORD_WIDTH,
            PORT_COUNT => 2
        )
        port map (
            clk  => clk,
            rst  => rst,
            addr => arb_addr,
            din  => arb_in,
            dout => arb_out,
            re   => arb_re,
            we   => arb_we,
            ready => arb_ready,
            mask => arb_mask,
            maddr => ram_addr,
            mout  => ram_din,
            mmask => ram_mask,
            min   => ram_dout,
            mre   => ram_re,
            mwe   => ram_we,
            mready => ram_ready
        );
    arb_addr  <= mem2_addr & mem1_addr;
    arb_in    <= mem2_din & mem1_din;
    mem1_dout <= arb_out(WORD_WIDTH - 1 downto 0);
    mem2_dout <= arb_out(2 * WORD_WIDTH - 1 downto WORD_WIDTH);
    arb_re    <= mem2_re & mem1_re;
    arb_we    <= mem2_we & mem1_we;
    arb_mask  <= mem2_mask & mem1_mask;
    mem1_ready <= arb_ready(0);
    mem2_ready <= arb_ready(1);

    process
    begin

        -- Reset
        mem1_addr <= (others => 'X');
        mem1_we    <= '0';
        mem1_re    <= '0';
        mem1_mask <= (others => '1');
        mem1_din  <= (others => 'X');
        mem2_addr <= (others => 'X');
        mem2_we    <= '0';
        mem2_re    <= '0';
        mem2_mask <= (others => '1');
        mem2_din  <= (others => 'X');
        cycle(clk);
        rst <= '0';

        assert ram_ready = '1' report "not ready (main)" severity failure;
        assert mem1_ready = '1' report "not ready 1" severity failure;
        assert mem2_ready = '1' report "not ready 2" severity failure;

        -- Write a value to mem1.
        mem1_addr <= x"00000000";
        mem1_din  <= x"FFFFFFFF";
        update(clk, mem1_we, mem1_ready);

        mem1_addr <= x"00000000";
        update(clk, mem1_re, mem1_ready);
        assert mem1_dout = x"FFFFFFFF"
            report "read failed" severity failure;

        -- Write a value to mem2.
        mem2_addr <= x"00000001";
        mem2_din  <= x"88888888";
        update(clk, mem2_we, mem2_ready);

        mem1_addr <= x"00000000";
        update(clk, mem1_re, mem1_ready);
        assert mem1_dout = x"FFFFFFFF"
            report "read failed" severity failure;
        mem2_addr <= x"00000001";
        update(clk, mem2_re, mem2_ready);
        assert mem2_dout = x"88888888"
            report "read failed" severity failure;

        -- Write a value to both.
        mem1_addr <= x"00000002";
        mem1_din  <= x"22222222";
        mem2_addr <= x"00000003";
        mem2_din  <= x"33333333";
        update2(clk, mem1_we, mem2_we,
                mem1_ready, mem2_ready);

        mem1_addr <= x"00000002";
        mem2_addr <= x"00000003";
        mem1_re <= '1';
        mem2_re <= '1';
        cycle(clk);
        mem1_re <= '0';
        mem2_re <= '0';
        cycle(clk);
        while mem1_ready = '0' or mem2_ready = '0' loop
            if mem1_ready = '1' then
                assert mem1_dout = x"22222222"
                    report "read failed" severity failure;
            end if;
            if mem2_ready = '1' then
                assert mem2_dout = x"33333333"
                    report "read failed" severity failure;
            end if;
            cycle(clk);
        end loop;
        assert mem1_dout = x"22222222"
            report "read failed" severity failure;
        assert mem2_dout = x"33333333"
            report "read failed" severity failure;

        -- Read and write.
        mem1_addr <= x"00000000";
        mem2_addr <= x"00000001";
        mem2_din  <= x"11111111";
        update2(clk, mem1_re, mem2_we,
                mem1_ready, mem2_ready);
        assert mem1_dout = x"FFFFFFFF"
            report "read failed" severity failure;

        mem2_addr <= x"00000001";
        mem2_re <= '1';
        cycle(clk);
        mem2_re <= '0';
        cycle(clk);
        while mem2_ready = '0' loop
            cycle(clk);
        end loop;
        assert mem2_dout = x"11111111"
            report "read failed" severity failure;

        -- Read followed by read.
        mem1_addr <= x"00000002";
        mem1_re <= '1';
        cycle(clk);
        mem1_re <= '0';
        mem2_addr <= x"00000001";
        mem2_re <= '1';
        cycle(clk);
        mem2_re <= '0';
        cycle(clk);
        assert mem2_ready = '0'
            report "ready too soon" severity failure;
        while mem1_ready = '0' or mem2_ready = '0' loop
            if mem1_ready = '1' then
                assert mem1_dout = x"22222222"
                    report "read failed" severity failure;
            end if;
            if mem2_ready = '1' then
                assert mem2_dout = x"11111111"
                    report "read failed" severity failure;
            end if;
            cycle(clk);
        end loop;
        assert mem1_dout = x"22222222"
            report "read failed" severity failure;
        assert mem2_dout = x"11111111"
            report "read failed" severity failure;

        -- Read followed by read in the other order.
        mem2_addr <= x"00000002";
        mem2_re <= '1';
        cycle(clk);
        mem2_re <= '0';
        cycle(clk);
        assert mem2_ready = '0'
            report "ready too soon" severity failure;
        mem1_addr <= x"00000001";
        mem1_re <= '1';
        cycle(clk);
        mem1_re <= '0';
        cycle(clk);
        assert mem1_ready = '0'
            report "ready too soon" severity failure;
        while mem1_ready = '0' or mem2_ready = '0' loop
            if mem1_ready = '1' then
                assert mem1_dout = x"11111111"
                    report "read failed" severity failure;
            end if;
            if mem2_ready = '1' then
                assert mem2_dout = x"22222222"
                    report "read failed" severity failure;
            end if;
            cycle(clk);
        end loop;
        assert mem2_ready = '1'
            report "not ready" severity failure;
        assert mem2_dout = x"22222222"
            report "read failed" severity failure;
        assert mem1_dout = x"11111111"
            report "read failed" severity failure;

        -- Write followed by read.
        mem1_addr <= x"00000005";
        mem1_din <= x"55555555";
        mem1_we <= '1';
        cycle(clk);
        mem1_we <= '0';
        mem2_addr <= x"00000002";
        mem2_re <= '1';
        cycle(clk);
        mem2_re <= '0';
        assert mem1_ready = '0'
            report "ready too soon" severity failure;
        cycle(clk);
        assert mem2_ready = '0'
            report "ready too soon" severity failure;
        while mem1_ready = '0' loop
            if mem2_ready = '1' then
                assert mem2_dout = x"22222222"
                    report "read failed" severity failure;
            end if;
            cycle(clk);
        end loop;
        mem1_addr <= x"00000005";
        mem1_re <= '1';
        cycle(clk);
        mem1_re <= '0';
        cycle(clk);
        while mem1_ready = '0' loop
            cycle(clk);
        end loop;
        assert mem1_dout = x"55555555"
            report "read failed" severity failure;
        while mem2_ready = '0' loop
            cycle(clk);
        end loop;
        assert mem2_dout = x"22222222"
            report "read failed" severity failure;

        -- Write followed by write.
        mem1_addr <= x"00000006";
        mem1_din <= x"66666666";
        mem1_we <= '1';
        cycle(clk);
        mem1_we <= '0';
        mem2_addr <= x"00000007";
        mem2_din <= x"77777777";
        mem2_we <= '1';
        cycle(clk);
        mem2_we <= '0';
        cycle(clk);
        while mem1_ready = '0' or mem2_ready = '0' loop
            cycle(clk);
        end loop;
        mem1_addr <= x"00000007";
        mem1_re <= '1';
        mem2_addr <= x"00000006";
        mem2_re <= '1';
        cycle(clk);
        mem1_re <= '0';
        mem2_re <= '0';
        cycle(clk);
        while mem1_ready = '0' or mem2_ready = '0' loop
            cycle(clk);
        end loop;
        assert mem1_dout = x"77777777"
            report "read failed" severity failure;
        assert mem2_dout = x"66666666"
            report "read failed" severity failure;

        wait_ready(clk, mem1_ready);
        wait_ready(clk, mem2_ready);
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

end tb;
