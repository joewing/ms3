library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity adapter is
    generic (
        IN_ADDR_WIDTH   : in natural    := 32;
        IN_WORD_WIDTH   : in natural    := 32;
        OUT_ADDR_WIDTH  : in natural    := 32;
        OUT_WORD_WIDTH  : in natural    := 32
    );
    port (
        clk     : in  std_logic;
        rst     : in  std_logic;

        addr    : in  std_logic_vector(IN_ADDR_WIDTH - 1 downto 0);
        din     : in  std_logic_vector(IN_WORD_WIDTH - 1 downto 0);
        dout    : out std_logic_vector(IN_WORD_WIDTH - 1 downto 0);
        re      : in  std_logic;
        we      : in  std_logic;
        mask    : in  std_logic_vector((IN_WORD_WIDTH / 8) - 1 downto 0);
        ready   : out std_logic;

        maddr   : out std_logic_vector(OUT_ADDR_WIDTH - 1 downto 0);
        mout    : out std_logic_vector(OUT_WORD_WIDTH - 1 downto 0);
        min     : in  std_logic_vector(OUT_WORD_WIDTH - 1 downto 0);
        mre     : out std_logic;
        mwe     : out std_logic;
        mmask   : out std_logic_vector((OUT_WORD_WIDTH / 8) - 1 downto 0);
        mready  : in  std_logic
    );
end adapter;

architecture rtl of adapter is

    constant IN_MASK_BITS   : natural := IN_WORD_WIDTH / 8;
    constant OUT_MASK_BITS  : natural := OUT_WORD_WIDTH / 8;
    constant STATE_COUNT    : natural := IN_WORD_WIDTH / OUT_WORD_WIDTH;
    constant LAST_STATE     : integer := STATE_COUNT - 1;
    constant STATE_BITS     : integer := IN_WORD_WIDTH - OUT_WORD_WIDTH;

    signal state        : natural;
    signal next_state   : natural;
    signal do_read      : std_logic;
    signal do_write     : std_logic;
    signal in_buf       : std_logic_vector(IN_WORD_WIDTH - 1 downto 0);
    signal in_mask      : std_logic_vector((IN_WORD_WIDTH / 8) - 1 downto 0);

begin

    -- The case with the input and output have the same width.
    same : if IN_WORD_WIDTH = OUT_WORD_WIDTH generate
        maddr   <= addr;
        mout    <= din;
        dout    <= min;
        mre     <= re;
        mwe     <= we;
        mmask   <= mask;
        ready   <= mready;
    end generate;

    -- The downstream memory is narrower (m < d).
    narrow : if IN_WORD_WIDTH > OUT_WORD_WIDTH generate

        -- We need to turn each access into multiple accesses.
        next_state <= state + 1;
        process(clk)
        begin
            if rising_edge(clk) then
                if rst = '1' then
                    state       <= LAST_STATE;
                    do_read     <= '0';
                    do_write    <= '0';
                elsif re = '1' or we = '1' then
                    in_mask     <= mask;
                    do_read     <= re;
                    do_write    <= we;
                    state       <= 0;
                elsif (do_read = '1' or do_write = '1') and mready = '1' then
                    if state = LAST_STATE - 1 then
                        do_read <= '0';
                        do_write <= '0';
                    end if;
                    state <= next_state;
                end if;
            end if;
        end process;

        -- Assign mre and mwe.
        mre <= re or (do_read and mready);
        mwe <= we or (do_write and mready);

        -- Assign in_buf.
        -- On reads, this is assigned piece-wise from min.
        -- On writes, this is assigned all-at-once from din.
        process(clk)
            variable top    : natural;
            variable bottom : natural;
        begin
            if rising_edge(clk) then
                if we = '1' then
                    in_buf <= din;
                elsif do_read = '1' and mready = '1' then
                    for i in 0 to LAST_STATE - 1 loop
                        bottom  := i * OUT_WORD_WIDTH;
                        top     := bottom + OUT_WORD_WIDTH - 1;
                        if state - 1 = i then
                            in_buf(top downto bottom) <= min;
                        end if;
                    end loop;
                end if;
            end if;
        end process;

        -- Assign dout.
        -- High bits are assigned directly from min.
        -- The rest are assigned from in_buf.
        dout(IN_WORD_WIDTH - 1 downto IN_WORD_WIDTH - OUT_WORD_WIDTH) <= min;
        dout(IN_WORD_WIDTH - OUT_WORD_WIDTH - 1 downto 0)
            <= in_buf(IN_WORD_WIDTH - OUT_WORD_WIDTH - 1 downto 0);

        -- Assign mout.
        -- This is assigned based on state.  In the first state, it
        -- is assigned directly from the low bits of din, otherwise, it is
        -- assigned from in_buf.
        process(state, din, in_buf)
            variable top    : natural;
            variable bottom : natural;
        begin
            if state = 0 then
                mout <= din(OUT_WORD_WIDTH - 1 downto 0);
            end if;
            for i in 1 to LAST_STATE loop
                bottom  := i * OUT_WORD_WIDTH;
                top     := bottom + OUT_WORD_WIDTH - 1;
                if state = i then
                    mout <= in_buf(top downto bottom);
                end if;
            end loop;
        end process;

        -- Assign mmask.
        -- This is assigned based on state like mout.
        process(state, next_state, mask, in_mask, mready)
            variable top    : natural;
            variable bottom : natural;
        begin
            if state = LAST_STATE then
                mmask <= mask(OUT_MASK_BITS - 1 downto 0);
            else
                for i in 1 to LAST_STATE - 1 loop
                    bottom  := i * OUT_MASK_BITS;
                    top     := bottom + OUT_MASK_BITS - 1;
                    if state = i and mready = '0' then
                        mmask <= in_mask(top downto bottom);
                    elsif next_state = i and mready = '1' then
                        mmask <= in_mask(top downto bottom);
                    end if;
                end loop;
            end if;
        end process;

        -- Assign maddr.
        process(state, next_state, addr, mready)
            constant bottom : natural := OUT_ADDR_WIDTH - IN_ADDR_WIDTH;
            constant top    : natural := bottom + IN_ADDR_WIDTH - 1;
        begin
            maddr(top downto bottom) <= addr(IN_ADDR_WIDTH - 1 downto 0);
            if mready = '1' then
                if state = LAST_STATE then
                    maddr(bottom - 1 downto 0) <= (others => '0');
                else
                    maddr(bottom - 1 downto 0)
                        <= std_logic_vector(to_unsigned(next_state, bottom));
                end if;
            else
                maddr(bottom - 1 downto 0)
                    <= std_logic_vector(to_unsigned(state, bottom));
            end if;
        end process;

        -- Assign ready.
        ready <= mready when state = LAST_STATE else '0';

    end generate;

    -- The downstream memory is wider (m > d).
    wide : if IN_WORD_WIDTH < OUT_WORD_WIDTH generate

        -- We need to convert each access into a partial access.
        -- Note that no state is needed.

        -- Assign mre, mwe, and ready.
        mre <= re;
        mwe <= we;
        ready <= mready;

        -- Assign maddr.
        -- maddr is not as wide, so we take the most significant bits.
        maddr <= addr(IN_ADDR_WIDTH - 1 downto IN_ADDR_WIDTH - OUT_ADDR_WIDTH);

        -- Assign mmask.
        -- mmask is wider than mask, so we insert zeros based on addr.
        process(addr, mask)
            constant bits   : natural := IN_ADDR_WIDTH - OUT_ADDR_WIDTH;
            constant bound  : natural := (2 ** bits) - 1;
            variable top    : natural;
            variable bottom : natural;
        begin
            for i in 0 to bound loop
                bottom  := i * (IN_WORD_WIDTH / 8);
                top     := bottom + (IN_WORD_WIDTH / 8) - 1;
                if to_integer(unsigned(addr(bits - 1 downto 0))) = i then
                    mmask(top downto bottom) <= mask;
                else
                    mmask(top downto bottom) <= (others => '0');
                end if;
            end loop;
        end process;

        -- Assign dout and mout.
        -- dout is not as wide as min, so we select the bits based on addr.
        process(addr, min, din)
            constant bits   : natural := IN_ADDR_WIDTH - OUT_ADDR_WIDTH;
            constant bound  : natural := (2 ** bits) - 1;
            variable bottom : natural;
            variable top    : natural;
        begin
            for i in 0 to bound loop
                bottom  := i * IN_WORD_WIDTH;
                top     := bottom + IN_WORD_WIDTH - 1;
                if to_integer(unsigned(addr(bits - 1 downto 0))) = i then
                    dout <= min(top downto bottom);
                    mout(top downto bottom) <= din;
                end if;
            end loop;
        end process;

    end generate;

end rtl;
