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

        -- Subsystem memory ports.
        addr    : in  std_logic_vector(PORT_COUNT * ADDR_WIDTH - 1 downto 0);
        din     : in  std_logic_vector(PORT_COUNT * WORD_WIDTH - 1 downto 0);
        dout    : out std_logic_vector(PORT_COUNT * WORD_WIDTH - 1 downto 0);
        re      : in  std_logic_vector(PORT_COUNT - 1 downto 0);
        we      : in  std_logic_vector(PORT_COUNT - 1 downto 0);
        mask    : in  std_logic_vector(PORT_COUNT * (WORD_WIDTH / 8) - 1
                                        downto 0);
        ready   : out std_logic_vector(PORT_COUNT - 1 downto 0);

        -- Main memory command port.
        maddr   : out   std_logic_vector(ADDR_WIDTH - 1 downto 0);
        mwe     : out   std_logic;
        mre     : out   std_logic;
        mfull   : in    std_logic;

        -- Main memory write request port.
        wdata   : out   std_logic_vector(WORD_WIDTH - 1 downto 0);
        wmask   : out   std_logic_vector((WORD_WIDTH / 8) - 1 downto 0);

        -- Main memory read response port.
        rdata   : in    std_logic_vector(WORD_WIDTH - 1 downto 0);
        ravail  : in    std_logic
    );
end arbiter;

architecture rtl of arbiter is

    constant MASK_WIDTH         : natural := WORD_WIDTH / 8;
    constant TOTAL_ADDR_WIDTH   : natural := PORT_COUNT * ADDR_WIDTH;
    constant TOTAL_WORD_WIDTH   : natural := PORT_COUNT * WORD_WIDTH;
    constant TOTAL_MASK_WIDTH   : natural := PORT_COUNT * MASK_WIDTH;

    -- Mapping from request to port number.
    type read_array is array(0 to PORT_COUNT - 1) of natural;
    signal outstanding          : read_array;
    signal outstanding_wptr     : natural;
    signal outstanding_rptr     : natural;

    -- Next active request (PORT_COUNT for none).
    signal next_active      : natural;
    signal next_is_read     : boolean;

    -- Buffers for subsystem requests.
    signal addr_buffer  : std_logic_vector(TOTAL_ADDR_WIDTH - 1 downto 0);
    signal data_buffer  : std_logic_vector(TOTAL_WORD_WIDTH - 1 downto 0);
    signal mask_buffer  : std_logic_vector(TOTAL_MASK_WIDTH - 1 downto 0);
    signal re_buffer    : std_logic_vector(PORT_COUNT - 1 downto 0);
    signal we_buffer    : std_logic_vector(PORT_COUNT - 1 downto 0);

    -- Outstanding reads.
    signal re_pending   : std_logic_vector(PORT_COUNT - 1 downto 0);

begin

    -- Determine the next request.
    process(re, we, re_buffer, we_buffer, mfull)
    begin
        next_active <= PORT_COUNT;
        next_is_read <= false;
        if mfull = '0' then
            for i in 0 to PORT_COUNT - 1 loop
                if re_buffer(i) = '1' or we_buffer(i) = '1' then
                    next_active <= i;
                    next_is_read <= re_buffer(i) = '1';
                end if;
            end loop;
        end if;
    end process;

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
            for i in 0 to PORT_COUNT - 1 loop
                addr_bottom := i * ADDR_WIDTH;
                addr_top    := addr_bottom + ADDR_WIDTH - 1;
                word_bottom := i * WORD_WIDTH;
                word_top    := word_bottom + WORD_WIDTH - 1;
                mask_bottom := i * MASK_WIDTH;
                mask_top    := mask_bottom + MASK_WIDTH - 1;
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
    end process;

    -- Track pending requests.
    process(clk)
    begin
        if rising_edge(clk) then
            if rst = '1' then
                re_buffer <= (others => '0');
                we_buffer <= (others => '0');
            else
                for i in 0 to PORT_COUNT - 1 loop
                    if next_active = i then
                        re_buffer(i) <= '0';
                        we_buffer(i) <= '0';
                    else
                        we_buffer(i) <= we(i) or we_buffer(i);
                        re_buffer(i) <= re(i) or re_buffer(i);
                    end if;
                end loop;
            end if;
        end if;
    end process;

    -- Serialize memory requests.
    process(clk)
        variable addr_top       : natural;
        variable addr_bottom    : natural;
        variable word_top       : natural;
        variable word_bottom    : natural;
        variable mask_top       : natural;
        variable mask_bottom    : natural;
    begin
        if rising_edge(clk) then
            mre <= '0';
            mwe <= '0';
            if rst = '0' and mfull = '0' then
                for i in 0 to PORT_COUNT - 1 loop
                    addr_bottom := i * ADDR_WIDTH;
                    addr_top    := addr_bottom + ADDR_WIDTH - 1;
                    word_bottom := i * WORD_WIDTH;
                    word_top    := word_bottom + WORD_WIDTH - 1;
                    mask_bottom := i * MASK_WIDTH;
                    mask_top    := mask_bottom + MASK_WIDTH - 1;
                    if next_active = i then
                        maddr <= addr_buffer(addr_top downto addr_bottom);
                        wdata <= data_buffer(word_top downto word_bottom);
                        wmask <= mask_buffer(mask_top downto mask_bottom);
                        mwe <= we_buffer(i);
                        mre <= re_buffer(i);
                    end if;
                end loop;
            end if;
        end if;
    end process;

    -- Keep track of outstanding read requests.
    process(clk)
    begin
        if rising_edge(clk) then
            if rst = '1' then
            else
                for i in 0 to PORT_COUNT - 1 loop
                end loop;
            end if;
        end if;
    end process;

    -- Handle outstanding reads.
    process(clk)
        variable word_top       : natural;
        variable word_bottom    : natural;
        variable next_response  : natural;
    begin
        if rising_edge(clk) then
            if rst = '1' then
                outstanding_rptr <= 0;
                outstanding_wptr <= 0;
                re_pending <= (others => '0');
            else
                next_response := outstanding(outstanding_rptr);
                for i in 0 to PORT_COUNT - 1 loop
                    word_bottom := i * WORD_WIDTH;
                    word_top    := word_bottom + WORD_WIDTH - 1;
                    if next_active = i and next_is_read then

                        -- Outstanding read started.
                        outstanding(outstanding_wptr) <= i;
                        re_pending(i) <= '1';
                        if outstanding_wptr = PORT_COUNT - 1 then
                            outstanding_wptr <= 0;
                        else
                            outstanding_wptr <= outstanding_wptr + 1;
                        end if;

                    end if;
                    if ravail = '1' and i = next_response then

                        -- Outstanding read finished.
                        dout(word_top downto word_bottom) <= rdata;
                        re_pending(i) <= '0';
                        if outstanding_rptr = PORT_COUNT - 1 then
                            outstanding_rptr <= 0;
                        else
                            outstanding_rptr <= outstanding_rptr + 1;
                        end if;

                    end if;
                end loop;
            end if;
        end if;
    end process;

    -- Drive the ready signals.
    ready <= not (re_buffer or we_buffer or re_pending);

end rtl;
