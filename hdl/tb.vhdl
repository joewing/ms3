library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity tb is
end entity;

architecture tb_arch of tb is

    signal clk      : std_logic    := '0';
    signal rst      : std_logic    := '1';
    signal cycles   : integer      := 0;

    constant ADDR_WIDTH : integer := 30;
    constant WORD_WIDTH : integer := 32;

    signal mem_addr     : std_logic_vector(ADDR_WIDTH + 2 - 1 downto 0);
    signal mem_din      : std_logic_vector(WORD_WIDTH - 1 downto 0);
    signal mem_dout     : std_logic_vector(WORD_WIDTH - 1 downto 0);
    signal mem_re       : std_logic;
    signal mem_we       : std_logic;
    signal mem_mask     : std_logic_vector((WORD_WIDTH / 8) - 1 downto 0);
    signal mem_ready    : std_logic;

    signal ram_addr     : std_logic_vector(ADDR_WIDTH - 1 downto 0);
    signal ram_din      : std_logic_vector(WORD_WIDTH - 1 downto 0);
    signal ram_dout     : std_logic_vector(WORD_WIDTH - 1 downto 0);
    signal ram_re       : std_logic;
    signal ram_we       : std_logic;
    signal ram_mask     : std_logic_vector((WORD_WIDTH / 8) - 1 downto 0);
    signal ram_full     : std_logic;
    signal ram_avail    : std_logic;

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
            LATENCY     => 10,
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
            full        => ram_full,
            avail       => ram_avail
        );

    mem1 : entity work.mem
        port map (
            clk             => clk,
            rst             => rst,
            port0_addr      => ram_addr,
            port0_din       => ram_dout,
            port0_dout      => ram_din,
            port0_re        => ram_re,
            port0_we        => ram_we,
            port0_mask      => ram_mask,
            port0_full      => ram_full,
            port0_avail     => ram_avail,
            subsystem0_addr => mem_addr(ADDR_WIDTH - 1 downto 0),
            subsystem0_in   => mem_din,
            subsystem0_out  => mem_dout,
            subsystem0_re   => mem_re,
            subsystem0_we   => mem_we,
            subsystem0_mask => mem_mask,
            subsystem0_ready=> mem_ready
        );

    process
    begin

        -- Reset
        mem_addr <= (others => '0');
        mem_we    <= '0';
        mem_re    <= '0';
        mem_mask <= (others => '1');
        mem_din  <= (others => '0');
        cycle(clk);
        rst <= '0';

        assert mem_ready = '1' report "not ready" severity failure;

        -- Write a value.
        mem_addr <= x"00000000";
        mem_din  <= x"FFFFFFFF";
        update(clk, mem_we, mem_ready);

        -- Write a value.
        mem_addr <= x"00000001";
        mem_din <= x"01234567";
        update(clk, mem_we, mem_ready);

        -- Make sure the write took (hit).
        mem_addr <= x"00000001";
        update(clk, mem_re, mem_ready);
        assert mem_dout = x"01234567"
            report "read failed" severity failure;

        -- Read another value (cold miss).
        mem_addr <= x"00000000";
        update(clk, mem_re, mem_ready);
        assert mem_dout = x"FFFFFFFF"
            report "read failed" severity failure;

        -- Write another value (conflict).
        mem_addr <= x"00000100";
        mem_din  <= x"00000321";
        update(clk, mem_we, mem_ready);

        -- Read the value (hit).
        mem_addr <= x"00000100";
        update(clk, mem_re, mem_ready);
        assert mem_dout = x"00000321"
            report "read failed" severity failure;

        -- Read the previous value.
        mem_addr <= x"00000001";
        update(clk, mem_re, mem_ready);
        assert mem_dout = x"01234567"
            report "read failed" severity failure;

        -- Another write.
        mem_addr <= x"00000101";
        mem_din  <= x"00000abc";
        update(clk, mem_we, mem_ready);

        -- Some reads.
        mem_addr <= x"00000101";
        update(clk, mem_re, mem_ready);
        assert mem_dout = x"00000abc"
            report "read failed" severity failure;
        mem_addr <= x"00000001";
        update(clk, mem_re, mem_ready);
        assert mem_dout = x"01234567"
            report "read failed" severity failure;
        mem_addr <= x"00000100";
        update(clk, mem_re, mem_ready);
        assert mem_dout = x"00000321"
            report "read failed" severity failure;

        -- Overwite.
        mem_addr <= x"00000001";
        mem_din  <= x"55555555";
        update(clk, mem_we, mem_ready);

        -- More reads.
        mem_addr <= x"00000001";
        update(clk, mem_re, mem_ready);
        assert mem_dout = x"55555555"
            report "read failed" severity failure;
        mem_addr <= x"00000101";
        update(clk, mem_re, mem_ready);
        assert mem_dout = x"00000abc"
            report "read failed" severity failure;
        mem_addr <= x"00000100";
        update(clk, mem_re, mem_ready);
        assert mem_dout = x"00000321"
            report "read failed" severity failure;

        -- Test a partial write.
        mem_addr <= x"00000001";
        mem_din  <= x"00000000";
        mem_mask <= "1100";
        update(clk, mem_we, mem_ready);

        -- Test some reads.
        mem_mask <= (others => '1');
        mem_addr <= x"00000001";
        update(clk, mem_re, mem_ready);
        assert mem_dout = x"00005555"
            report "read failed" severity failure;
        mem_addr <= x"00000101";
        update(clk, mem_re, mem_ready);
        assert mem_dout = x"00000abc"
            report "read failed" severity failure;
        mem_addr <= x"00000100";
        update(clk, mem_re, mem_ready);
        assert mem_dout = x"00000321"
            report "read failed" severity failure;

        -- Test some reads/writes.
        for j in 2 to 20 loop
            for i in 1 to j loop
                mem_addr <= std_logic_vector(to_unsigned(i, ADDR_WIDTH + 2));
                mem_din  <= std_logic_vector(to_unsigned(i, WORD_WIDTH));
                update(clk, mem_we, mem_ready);
            end loop;
            for i in 1 to j loop
                mem_addr <= std_logic_vector(to_unsigned(i, ADDR_WIDTH + 2));
                update(clk, mem_re, mem_ready);
                assert unsigned(mem_dout) = i
                    report "failed at j=" & natural'image(j) severity failure;
            end loop;
        end loop;

        wait_ready(clk, mem_ready);
        report "cycles: " & integer'image(cycles);
        wait;

    end process;

    process(clk)
    begin
        if rising_edge(clk) and rst = '0' then
            if ram_re = '1' then
                assert mem_we = '0'
                    report "read and write" severity failure;
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

end tb_arch;
