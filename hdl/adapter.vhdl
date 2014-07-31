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
    constant WORD_COUNT     : integer := STATE_COUNT - 1;
    constant STATE_BITS     : integer := IN_WORD_WIDTH - OUT_WORD_WIDTH;

    signal state        : natural;
    signal current      : natural;
    signal pending      : std_logic;
    signal start        : std_logic;
    signal do_read      : std_logic;
    signal do_write     : std_logic;
    signal in_buf       : std_logic_vector(IN_WORD_WIDTH - 1 downto 0);
    signal in_mask      : std_logic_vector((IN_WORD_WIDTH / 8) - 1 downto 0);
    signal raddr        : std_logic_vector(IN_ADDR_WIDTH - 1 downto 0);

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
        process(clk)
        begin
            if rising_edge(clk) then
                if rst = '1' then
                    state       <= WORD_COUNT;
                    do_read     <= '0';
                    do_write    <= '0';
                    pending     <= '0';
                elsif re = '1' or we = '1' then
                    in_mask     <= mask;
                    do_read     <= re;
                    do_write    <= we;
                    state       <= 0;
                    pending     <= '1';
                elsif start = '1' then
                    if state = WORD_COUNT - 1 then
                        do_read     <= '0';
                        do_write    <= '0';
                    end if;
                    state <= state + 1;
                    pending <= '1';
                else
                    pending <= '0';
                end if;
            end if;
        end process;

        -- Determine when to start the next access.
        start <= (do_read or do_write) and mready and not pending;

        -- Determine the current word.
        process(state, mready, re, we)
        begin
            if re = '1' or we = '1' then
                current <= 0;
            elsif mready = '1' then
                current <= state + 1;
            else
                current <= state;
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
                    for i in 0 to WORD_COUNT loop
                        bottom  := i * OUT_WORD_WIDTH;
                        top     := bottom + OUT_WORD_WIDTH - 1;
                        if state = i then
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

        -- Assign mout and mmask.
        -- This is assigned based on state.  In the first state, we
        -- assign directly from the low bits of the input, otherwise,
        -- we assign from the buffer.
        process(in_buf, in_mask, current, din, mask, we)
            variable word_top       : natural;
            variable word_bottom    : natural;
            variable mask_top       : natural;
            variable mask_bottom    : natural;
        begin
            mout <= din(OUT_WORD_WIDTH - 1 downto 0);
            mmask <= mask(OUT_MASK_BITS - 1 downto 0);
            for i in 1 to WORD_COUNT loop
                word_bottom     := i * OUT_WORD_WIDTH;
                word_top        := word_bottom + OUT_WORD_WIDTH - 1;
                mask_bottom     := WORD_COUNT * OUT_MASK_BITS;
                mask_top        := mask_bottom + OUT_MASK_BITS - 1;
                if current = i then
                    mout <= in_buf(word_top downto word_bottom);
                    mmask <= in_mask(mask_top downto mask_bottom);
                end if;
            end loop;
        end process;

        -- Assign maddr.
        process(current, addr, mready)
            constant bottom : natural := OUT_ADDR_WIDTH - IN_ADDR_WIDTH;
            constant top    : natural := bottom + IN_ADDR_WIDTH - 1;
        begin
            maddr(top downto bottom) <= addr(IN_ADDR_WIDTH - 1 downto 0);
            maddr(bottom - 1 downto 0)
                <= std_logic_vector(to_unsigned(current, bottom));
        end process;

        -- Assign ready.
        ready <= mready and not (do_read or do_write);

    end generate;

    -- The downstream memory is wider (m > d).
    wide : if IN_WORD_WIDTH < OUT_WORD_WIDTH generate

        -- We need to convert each access into a partial access.
        -- Note that no state is needed.

        -- Assign mre, mwe, and ready.
        mre <= re;
        mwe <= we;
        ready <= mready;

        -- Buffer the address.
        -- This is needed for reads since the address doesn't need
        -- to remain valid after re is asserted.
        process(clk)
        begin
            if rising_edge(clk) then
                if re = '1' then
                    raddr <= addr;
                end if;
            end if;
        end process;

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
            mmask <= (others => '0');
            for i in 0 to bound loop
                bottom  := i * IN_MASK_BITS;
                top     := bottom + IN_MASK_BITS - 1;
                if to_integer(unsigned(addr(bits - 1 downto 0))) = i then
                    mmask(top downto bottom) <= mask;
                end if;
            end loop;
        end process;

        -- Assign dout and mout.
        -- dout is not as wide as min, so we select the bits based on addr.
        process(raddr, min, din)
            constant bits   : natural := IN_ADDR_WIDTH - OUT_ADDR_WIDTH;
            constant bound  : natural := (2 ** bits) - 1;
            variable bottom : natural;
            variable top    : natural;
        begin
            for i in 0 to bound loop
                bottom  := i * IN_WORD_WIDTH;
                top     := bottom + IN_WORD_WIDTH - 1;
                if to_integer(unsigned(raddr(bits - 1 downto 0))) = i then
                    dout <= min(top downto bottom);
                end if;
                mout(top downto bottom) <= din;
            end loop;
        end process;

    end generate;

end rtl;
