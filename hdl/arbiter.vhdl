library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity arbiter is
    generic (
        ADDR_WIDTH  : in natural    := 32;
        WORD_WIDTH  : in natural    := 32;
        PORT_COUNT  : in natural    := 1
    );
    port (
        clk     : in std_logic;
        rst     : in std_logic;

        addr    : in  std_logic_vector(PORT_COUNT * ADDR_WIDTH - 1 downto 0);
        din     : in  std_logic_vector(PORT_COUNT * WORD_WIDTH - 1 downto 0);
        dout    : out std_logic_vector(PORT_COUNT * WORD_WIDTH - 1 downto 0);
        re      : in  std_logic_vector(PORT_COUNT - 1 downto 0);
        we      : in  std_logic_vector(PORT_COUNT - 1 downto 0);
        mask    : in  std_logic_vector(PORT_COUNT * (WORD_WIDTH / 8) - 1
                                        downto 0);
        ready   : out std_logic_vector(PORT_COUNT - 1 downto 0);

        maddr   : out   std_logic_vector(ADDR_WIDTH - 1 downto 0);
        mout    : out   std_logic_vector(WORD_WIDTH - 1 downto 0);
        min     : in    std_logic_vector(WORD_WIDTH - 1 downto 0);
        mre     : out   std_logic;
        mwe     : out   std_logic;
        mmask   : out   std_logic_vector((WORD_WIDTH / 8) - 1 downto 0);
        mready  : in    std_logic
    );
end arbiter;

architecture rtl of arbiter is

    constant MASK_WIDTH         : natural := WORD_WIDTH / 8;
    constant TOTAL_ADDR_WIDTH   : natural := PORT_COUNT * ADDR_WIDTH;
    constant TOTAL_WORD_WIDTH   : natural := PORT_COUNT * WORD_WIDTH;
    constant TOTAL_MASK_WIDTH   : natural := PORT_COUNT * MASK_WIDTH;

    signal active       : natural;  -- Active port (PORT_COUNT for none).
    signal pending      : std_logic;

    signal addr_buffer  : std_logic_vector(TOTAL_ADDR_WIDTH - 1 downto 0);
    signal data_buffer  : std_logic_vector(TOTAL_WORD_WIDTH - 1 downto 0);
    signal mask_buffer  : std_logic_vector(TOTAL_MASK_WIDTH - 1 downto 0);
    signal re_buffer    : std_logic_vector(PORT_COUNT - 1 downto 0);
    signal we_buffer    : std_logic_vector(PORT_COUNT - 1 downto 0);

begin

    -- Register signals from the memory subsystems.
    process(clk)
        variable addr_top       : natural;
        variable addr_bottom    : natural;
        variable word_top       : natural;
        variable word_bottom    : natural;
        variable mask_top       : natural;
        variable mask_bottom    : natural;
        variable next_pending   : std_logic;
    begin
        if rising_edge(clk) then
            if rst = '1' then
                re_buffer <= (others => '0');
                we_buffer <= (others => '0');
            else
                for i in 0 to PORT_COUNT - 1 loop
                    addr_bottom := i * ADDR_WIDTH;
                    addr_top    := addr_bottom + ADDR_WIDTH - 1;
                    word_bottom := i * WORD_WIDTH;
                    word_top    := word_bottom + WORD_WIDTH - 1;
                    mask_bottom := i * MASK_WIDTH;
                    mask_top    := mask_bottom + MASK_WIDTH - 1;
                    if active = i then
                        re_buffer(i) <= re(i);
                        we_buffer(i) <= we(i);
                    else
                        re_buffer(i) <= re(i) or re_buffer(i);
                        we_buffer(i) <= we(i) or we_buffer(i);
                    end if;
                    if re(i) = '1' or we(i) = '1' then
                        addr_buffer(addr_top downto addr_bottom)
                            <= addr(addr_top downto addr_bottom);
                        data_buffer(word_top downto word_bottom)
                            <= din(word_top downto word_bottom);
                        mask_buffer(mask_top downto mask_bottom)
                            <= mask(mask_top downto mask_bottom);
                    end if;
                end loop;
            end if;
        end if;
    end process;

    -- Determine which port should be connected.
    process(clk)
    begin
        if rising_edge(clk) then
            if rst = '1' then
                active <= PORT_COUNT;
                pending <= '0';
            elsif mready = '1' and pending = '0' then
                active <= PORT_COUNT;
                for i in 0 to PORT_COUNT - 1 loop
                    if re(i) = '1' or re_buffer(i) = '1' then
                        active <= i;
                        pending <= '1';
                    elsif we(i) = '1' or we_buffer(i) = '1' then
                        active <= i;
                        pending <= '1';
                    end if;
                end loop;
            else
                pending <= '0';
            end if;
        end if;
    end process;

    -- Set the output.
    process(clk)
        variable word_top       : natural;
        variable word_bottom    : natural;
    begin
        if rising_edge(clk) then
            if rst = '1' then
                ready <= (others => '1');
            else
                for i in 0 to PORT_COUNT - 1 loop
                    word_bottom := i * WORD_WIDTH;
                    word_top    := word_bottom + WORD_WIDTH - 1;
                    if re_buffer(i) = '1' or we_buffer(i) = '1' then
                        ready(i) <= '0';
                    elsif re(i) = '1' or we(i) = '1' then
                        ready(i) <= '0';
                    elsif active = i then
                        ready(i) <= mready;
                        dout(word_top downto word_bottom) <= min;
                    else
                        ready(i) <= '1';
                    end if;
                end loop;
            end if;
        end if;
    end process;

    -- Drive the memory port.
    process(active, addr_buffer, data_buffer,
            re_buffer, we_buffer, mask_buffer)
        variable addr_top       : natural;
        variable addr_bottom    : natural;
        variable word_top       : natural;
        variable word_bottom    : natural;
        variable mask_top       : natural;
        variable mask_bottom    : natural;
    begin
        maddr   <= (others => 'Z');
        mout    <= (others => 'Z');
        mre     <= '0';
        mwe     <= '0';
        mmask   <= (others => 'Z');
        for i in 0 to PORT_COUNT - 1 loop
            addr_bottom := i * ADDR_WIDTH;
            addr_top    := addr_bottom + ADDR_WIDTH - 1;
            word_bottom := i * WORD_WIDTH;
            word_top    := word_bottom + WORD_WIDTH - 1;
            mask_bottom := i * MASK_WIDTH;
            mask_top    := mask_bottom + MASK_WIDTH - 1;
            if active = i then
                maddr   <= addr_buffer(addr_top downto addr_bottom);
                mout    <= data_buffer(word_top downto word_bottom);
                mre     <= re_buffer(i);
                mwe     <= we_buffer(i);
                mmask   <= mask_buffer(mask_top downto mask_bottom);
            end if;
        end loop;
    end process;

end rtl;
