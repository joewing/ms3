library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity adapter is
    generic (
        IN_ADDR_WIDTH   : in natural    := 32;
        IN_DATA_WIDTH   : in natural    := 32;
        OUT_ADDR_WIDTH  : in natural    := 32;
        OUT_DATA_WIDTH  : in natural    := 32
    );
    port (
        clk     : in  std_logic;
        rst     : in  std_logic;

        addr    : in  std_logic_vector(IN_ADDR_WIDTH - 1 downto 0);
        din     : in  std_logic_vector(IN_DATA_WIDTH - 1 downto 0);
        dout    : out std_logic_vector(IN_DATA_WIDTH - 1 downto 0);
        re      : in  std_logic;
        we      : in  std_logic;
        mask    : in  std_logic_vector((IN_DATA_WIDTH / 8) - 1 downto 0);
        ready   : out std_logic;

        maddr   : out std_logic_vector(OUT_ADDR_WIDTH - 1 downto 0);
        mout    : out std_logic_vector(OUT_DATA_WIDTH - 1 downto 0);
        min     : in  std_logic_vector(OUT_DATA_WIDTH - 1 downto 0);
        mre     : out std_logic;
        mwe     : out std_logic;
        mmask   : out std_logic_vector((OUT_DATA_WIDTH / 8) - 1 downto 0);
        mready  : in  std_logic
    );
end adapter;

architecture rtl of adapter is

    constant IN_MASK_BITS   : natural := IN_DATA_WIDTH / 8;
    constant OUT_MASK_BITS  : natural := OUT_DATA_WIDTH / 8;
    constant NARROW_COUNT   : natural := IN_DATA_WIDTH / OUT_DATA_WIDTH;
    constant WIDE_COUNT     : natural := OUT_DATA_WIDTH / IN_DATA_WIDTH;

    signal state    : natural;
    signal do_read  : std_logic;
    signal do_write : std_logic;
    signal in_buf   : std_logic_vector(IN_DATA_WIDTH - 1 downto 0);
    signal in_mask  : std_logic_vector((IN_DATA_WIDTH / 8) - 1 downto 0);

begin

    -- The case with the input and output have the same width.
    same : if IN_DATA_WIDTH = OUT_DATA_WIDTH generate
        maddr   <= addr;
        mout    <= din;
        dout    <= min;
        mre     <= re;
        mwe     <= we;
        mmask   <= mask;
        ready   <= mready;
    end generate;

    -- The downstream memory is narrower (m < d).
    narrow : if IN_DATA_WIDTH > OUT_DATA_WIDTH generate

        -- We need to turn each access into multiple accesses.
        process(clk)
        begin
            if rising_edge(clk) then
                if rst = '1' then
                    state       <= NARROW_COUNT;
                    do_read     <= '0';
                    do_write    <= '0';
                elsif re = '1' or we = '1' then
                    in_mask     <= mask;
                    do_read     <= re;
                    do_write    <= we;
                    state       <= 0;
                elsif state < NARROW_COUNT then
                    if mready = '1' then
                        state       <= state + 1;
                    end if;
                else
                    do_read     <= '0';
                    do_write    <= '0';
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
                if re = '1' then
                    in_buf(OUT_DATA_WIDTH - 1 downto 0) <= min;
                elsif we = '1' then
                    in_buf <= din;
                elsif do_read = '1' then
                    for i in 1 to NARROW_COUNT - 1 loop
                        bottom  := i * OUT_DATA_WIDTH;
                        top     := bottom + OUT_DATA_WIDTH - 1;
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
        dout(IN_DATA_WIDTH - 1 downto IN_DATA_WIDTH - OUT_DATA_WIDTH) <= min;
        dout(IN_DATA_WIDTH - OUT_DATA_WIDTH - 1 downto 0)
            <= in_buf(IN_DATA_WIDTH - OUT_DATA_WIDTH - 1 downto 0);

        -- Assign mout.
        -- This is assigned based on state.  In the first state, it
        -- is assigned directly from the low bits of din, otherwise, it is
        -- assigned from in_buf.
        process(state, din, in_buf)
            variable top    : natural;
            variable bottom : natural;
        begin
            if state = 0 then
                mout <= din(OUT_DATA_WIDTH - 1 downto 0);
            end if;
            for i in 1 to NARROW_COUNT - 1 loop
                bottom  := i * OUT_DATA_WIDTH;
                top     := bottom + OUT_DATA_WIDTH - 1;
                if state = i then
                    mout <= in_buf(top downto bottom);
                end if;
            end loop;
        end process;

        -- Assign mmask.
        -- This is assigned based on state like mout.
        process(state, mask, in_mask)
            variable top    : natural;
            variable bottom : natural;
        begin
            if state = 0 then
                mmask <= in_mask(OUT_MASK_BITS - 1 downto 0);
            end if;
            for i in 1 to NARROW_COUNT - 1 loop
                bottom  := i * OUT_MASK_BITS;
                top     := bottom + OUT_MASK_BITS - 1;
                if state = i then
                    mmask <= in_mask(top downto bottom);
                end if;
            end loop;
        end process;

        -- Assign maddr.
        process(addr, state)
            variable sum : unsigned(OUT_ADDR_WIDTH - 1 downto 0);
        begin
            if state = NARROW_COUNT then
                maddr <= addr;
            else
                sum := unsigned(addr) + to_unsigned(state, OUT_ADDR_WIDTH);
                maddr <= std_logic_vector(sum);
            end if;
        end process;

        -- Assign ready.
        ready <= mready when state = NARROW_COUNT else '0';

    end generate;

    -- The downstream memory is wider (m > d).
    wide : if IN_DATA_WIDTH < OUT_DATA_WIDTH generate

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
                bottom  := i * (IN_DATA_WIDTH / 8);
                top     := bottom + (IN_DATA_WIDTH / 8) - 1;
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
                bottom  := i * IN_DATA_WIDTH;
                top     := bottom + IN_DATA_WIDTH - 1;
                if to_integer(unsigned(addr(bits - 1 downto 0))) = i then
                    dout <= min(top downto bottom);
                    mout(top downto bottom) <= din;
                end if;
            end loop;
        end process;

    end generate;

end rtl;
