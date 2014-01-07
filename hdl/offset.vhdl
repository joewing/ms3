
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity offset is
    generic (
        ADDR_WIDTH  : in natural := 32;
        WORD_WIDTH  : in natural := 32;
        VALUE       : in integer := 0
    );
    port (
        clk     : in  std_logic;
        rst     : in  std_logic;
        addr    : in  std_logic_vector(ADDR_WIDTH - 1 downto 0);
        din     : in  std_logic_vector(WORD_WIDTH - 1 downto 0);
        dout    : out std_logic_vector(WORD_WIDTH - 1 downto 0);
        re      : in  std_logic;
        we      : in  std_logic;
        mask    : in  std_logic_vector((WORD_WIDTH / 8) - 1 downto 0);
        ready   : out std_logic;
        maddr   : out std_logic_vector(ADDR_WIDTH - 1 downto 0);
        mout    : out std_logic_vector(WORD_WIDTH - 1 downto 0);
        min     : in  std_logic_vector(WORD_WIDTH - 1 downto 0);
        mre     : out std_logic;
        mwe     : out std_logic;
        mmask   : out std_logic_vector((WORD_WIDTH / 8) - 1 downto 0);
        mready  : in  std_logic
    );
end offset;

architecture offset_arch of offset is

    constant WORD_BYTES : natural := WORD_WIDTH / 8;
    constant BOFFSET    : natural := VALUE mod WORD_BYTES;
    constant ZERO_MASK  : std_logic_vector(WORD_BYTES - 1 downto 0)
                        := (others => '0');

    signal maska        : std_logic_vector(WORD_BYTES - 1 downto 0);
    signal maskb        : std_logic_vector(WORD_BYTES - 1 downto 0);
    signal first_word   : std_logic;
    signal split_access : std_logic;
    signal rre          : std_logic;
    signal rwe          : std_logic;
    signal saved        : std_logic_vector(WORD_WIDTH - 1 downto 0);
    signal state        : natural;
    signal next_state   : natural;

begin

    -- Handle the simple case when the offset is a multiple of the word size.
    word_offset : if BOFFSET = 0 generate

        process(addr)
            variable woffset : signed(ADDR_WIDTH - 1 downto 0);
        begin
            woffset  := to_signed(VALUE / WORD_WIDTH, ADDR_WIDTH);
            maddr     <= std_logic_vector(signed(addr) + woffset);
        end process;

        dout      <= min;
        mout      <= din;
        mre        <= re;
        mwe        <= we;
        mmask     <= mask;
        ready     <= mready;

    end generate;

    -- Handle the case when the offset is not a multiple of the word size.
    -- This can cause a single access to turn into multiple accesses.
    byte_offset : if BOFFSET /= 0 generate

        -- Get the shifted mask used for byte offsets.
        process(mask)
            variable shifted : std_logic_vector(2 * WORD_BYTES - 1 downto 0);
        begin
            shifted := (others => '0');
            for b in 0 to WORD_BYTES - 1 loop
                shifted(b + BOFFSET) := mask(b);
            end loop;
            maska <= shifted(WORD_BYTES - 1 downto 0);
            maskb <= shifted(2 * WORD_BYTES - 1 downto WORD_BYTES);
        end process;

        -- Determine if this is a split access.
        split_access <= '1' when maska /= ZERO_MASK and maskb /= ZERO_MASK
                            else '0';

        -- Determine the next state.
        process(state, re, we, mready, split_access)
        begin
            next_state <= state;
            case state is
                when 1 =>       -- First access.
                    if mready = '1' then
                        if split_access = '1' then
                            next_state <= 2;
                        else
                            next_state <= 0;
                        end if;
                    end if;
                when 2 =>       -- Second access.
                    if mready = '1' then
                        next_state <= 0;
                    end if;
                when others =>  -- Idle.
                    if re = '1' or we = '1' then
                        next_state <= 1;
                    end if;
            end case;
        end process;

        -- State machine.
        process(clk)
        begin
            if rising_edge(clk) then
                if rst = '1' then
                    state <= 0;
                else
                    state <= next_state;
                end if;
            end if;
        end process;

        -- Register read/write enable.
        process(clk)
        begin
            if rising_edge(clk) then
                if state = 0 then
                    rre <= re;
                    rwe <= we;
                end if;
            end if;
        end process;

        -- Determine if we should be accessing the first word.
        process(next_state, maska, mready)
        begin
            if next_state = 0 and maska /= ZERO_MASK then
                first_word <= '1';
            elsif next_state = 1 and maska /= ZERO_MASK then
                first_word <= '1';
            else
                first_word <= '0';
            end if;
        end process;

        -- Drive maddr and mmask (address and mask for the next memory).
        process(addr, maska, maskb, first_word)
            variable next_addr : std_logic_vector(ADDR_WIDTH - 1 downto 0);
            variable woffset   : signed(ADDR_WIDTH - 1 downto 0);
        begin
            next_addr := std_logic_vector(unsigned(addr) + 1);
            woffset := to_signed(VALUE / WORD_WIDTH, ADDR_WIDTH);
            if first_word = '1' then
                maddr <= std_logic_vector(signed(addr) + woffset);
                mmask <= maska;
            else
                maddr <= std_logic_vector(signed(next_addr) + woffset);
                mmask <= maskb;
            end if;
        end process;

        -- Drive mre and mwe (read/write enable to the next memory).
        process(state, re, we, mready)
        begin
            mre <= '0';
            mwe <= '0';
            if state = 0 then
                mre <= re;
                mwe <= we;
            elsif state = 1 and mready = '1' and maskb /= ZERO_MASK then
                mre <= rre;
                mwe <= rwe;
            end if;
        end process;

        -- Drive ready.
        ready <= mready when next_state = 0 else '0';

        -- Drive mout (data to the next memory).
        process(din, first_word)
            variable start : integer;
        begin
            mout <= (others => 'Z');
            for b in 0 to WORD_BYTES - 1 loop
                if first_word = '1' then
                    start := (b + BOFFSET) * 8;
                    if start < WORD_WIDTH then
                        mout(start + 7 downto start)
                            <= din(b * 8 + 7 downto b * 8);
                    end if;
                else
                    start := (b + BOFFSET) * 8 - WORD_WIDTH;
                    if start >= 0 then
                        mout(start + 7 downto start)
                            <= din(b * 8 + 7 downto b * 8);
                    end if;
                end if;
            end loop;
        end process;

        -- Save the output of a read.
        -- This is used for driving dout.
        process(clk)
        begin
            if rising_edge(clk) then
                if state = 1 then
                    saved <= min;
                end if;
            end if;
        end process;

        -- Drive dout.
        process(state, maska, min)
            variable start : integer;
        begin
            dout <= (others => 'Z');
            for b in 0 to WORD_BYTES - 1 loop
                if b >= WORD_BYTES - BOFFSET then
                    start := (b + BOFFSET - WORD_BYTES) * 8;
                    dout(b * 8 + 7 downto b * 8)
                        <= min(start + 7 downto start);
                else
                    start := (b + BOFFSET) * 8;
                    dout(b * 8 + 7 downto b * 8)
                        <= saved(start + 7 downto start);
                end if;
            end loop;
        end process;

    end generate;

end offset_arch;
