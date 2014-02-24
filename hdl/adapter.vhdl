library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity adapter is
    generic (
        IN_ADDR_WIDTH   : in natural    := 32;
        IN_DATA_WIDTH   : in natural    := 32;
        OUT_ADDR_WIDTH  : in natural    := 32;  -- >= IN_ADDR_WIDTH
        OUT_DATA_WIDTH  : in natural    := 32   -- <= IN_DATA_WIDTH
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
    constant MULTIPLIER     : natural := IN_DATA_WIDTH / OUT_DATA_WIDTH;

    signal state    : natural;
    signal in_addr  : std_logic_vector(IN_ADDR_WIDTH - 1 downto 0);
    signal do_read  : std_logic;
    signal do_write : std_logic;
    signal buf      : std_logic_vector(IN_DATA_WIDTH - 1 downto 0);
    signal mask_buf : std_logic_vector((IN_DATA_WIDTH / 8) - 1 downto 0);

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
    -- Note that the case where the downstream is wider is not supported.
    narrow : if IN_DATA_WIDTH > OUT_DATA_WIDTH generate

        -- We need to turn each access into multiple accesses.
        process(clk)
        begin
            if rising_edge(clk) then
                if rst = '1' then
                    state       <= MULTIPLIER;
                    do_read     <= '0';
                    do_write    <= '0';
                elsif re = '1' or we = '1' then
                    mask_buf    <= mask;
                    do_read     <= re;
                    do_write    <= we;
                    state       <= 0;
                elsif state < MULTIPLIER then
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

        -- Assign buf.
        -- On reads, this is assigned piece-wise from min.
        -- On writes, this is assigned all-at-once from din.
        process(clk)
            variable top    : natural;
            variable bottom : natural;
        begin
            if rising_edge(clk) then
                if re = '1' then
                    buf(OUT_DATA_WIDTH - 1 downto 0) <= min;
                elsif we = '1' then
                    buf <= din;
                elsif do_read = '1' then
                    for i in 1 to MULTIPLIER - 1 loop
                        bottom  := i * OUT_DATA_WIDTH;
                        top     := bottom + OUT_DATA_WIDTH - 1;
                        if state = i then
                            buf(top downto bottom) <= min;
                        end if;
                    end loop;
                end if;
            end if;
        end process;

        -- Assign dout.
        -- High bits are assigned directly from min.
        -- The rest are assigned from out_buffer.
        dout(IN_DATA_WIDTH - 1 downto IN_DATA_WIDTH - OUT_DATA_WIDTH) <= min;
        dout(IN_DATA_WIDTH - OUT_DATA_WIDTH - 1 downto 0)
            <= buf(IN_DATA_WIDTH - OUT_DATA_WIDTH - 1 downto 0);

        -- Assign mout.
        -- This is assigned based on state.  In the first state, it
        -- is assigned directly from the low bits of din, otherwise, it is
        -- assigned from buf.
        process(state, din, buf)
            variable top    : natural;
            variable bottom : natural;
        begin
            if state = 0 then
                mout <= din(OUT_DATA_WIDTH - 1 downto 0);
            end if;
            for i in 1 to MULTIPLIER - 1 loop
                bottom  := i * OUT_DATA_WIDTH;
                top     := bottom + OUT_DATA_WIDTH - 1;
                if state = i then
                    mout <= buf(top downto bottom);
                end if;
            end loop;
        end process;

        -- Assign mmask.
        -- This is assigned based on state like mout.
        process(state, mask, mask_buf)
            variable top    : natural;
            variable bottom : natural;
        begin
            if state = 0 then
                mmask <= mask_buf(OUT_MASK_BITS - 1 downto 0);
            end if;
            for i in 1 to MULTIPLIER - 1 loop
                bottom  := i * OUT_MASK_BITS;
                top     := bottom + OUT_MASK_BITS - 1;
                if state = i then
                    mmask <= mask_buf(top downto bottom);
                end if;
            end loop;
        end process;

        -- Assign maddr.
        process(addr, state)
            variable sum : unsigned(OUT_ADDR_WIDTH - 1 downto 0);
        begin
            if state = MULTIPLIER then
                maddr <= addr;
            else
                sum := unsigned(addr) + to_unsigned(state, OUT_ADDR_WIDTH);
                maddr <= std_logic_vector(sum);
            end if;
        end process;

        -- Assign ready.
        ready <= mready when state = MULTIPLIER else '0';

    end generate;

end rtl;
