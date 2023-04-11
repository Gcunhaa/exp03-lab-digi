library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_bit.all;

entity receptor_exp2 is
    port (
        clk, reset: in bit;
        serial_in: in bit;
        display: out std_logic_vector(7 downto 0)
    );
end entity;

architecture receptor_Arch of receptor_exp2 is
    component receptor_UC is
        port (
            -- Controle GLobal
            clk, reset: in bit;
            -- Sinais de condição
            erro: in bit;
            contador: in bit_vector(3 downto 0);
            -- Sinais de controle
            reset_b: out bit;
            en: out bit;
            up: out bit;
            -- Entrada
            dado: in bit
        );
    end component;

    component contador is
        port (
            clk, reset: in bit;
            en: in bit;
   
            dado: out bit_vector(3 downto 0)
        );
    end component;

    component paridade is
        port (
            dado: in bit_vector(8 downto 0);
   
            erro: out bit
        );
    end component;

    component amostrador is
        port (
            clk: in bit;
            dado_in: in bit;
   
            dado_out: out bit
        );
    end component;

    component disp_mem is
        port(
            clk: in bit;
            up: in bit;
            dado: in bit_vector(7 downto 0);
   
            display: out bit_vector(7 downto 0)
        );
    end component;

    component dado_reg is
        port (
            clk, reset: in bit;
            en, dado: in bit;
            dado_out: out bit_vector(8 downto 0)
        );
    end component;
   
    component Clock_Divider is
    port (
        clk, reset: in bit;
        clock_out: out bit
    );
end component;
   
    component ClockD is
    port (
        clk, reset: in bit;
        clock_out: out bit
    );
end component;
   
    component display_dec is
  port (
          input: in   std_logic_vector(7 downto 0); -- ASCII 8 bits
          output: out std_logic_vector(7 downto 0)  -- ponto + abcdefg
        );
    end component;

component note is
port (
i: in std_logic_vector (7 downto 0);
o: out std_logic_vector (7 downto 0)
);
end component;

    signal reset_b: bit;
    signal dado, erro, en, up: bit;
    signal cont: bit_vector(3 downto 0);
    signal regD: bit_vector(8 downto 0);
    signal displayD: bit_vector(7 downto 0);
    signal clkd1, clkd2, nclk: bit;
    signal display_F: bit_vector(7 downto 0);
    signal display_not: std_logic_vector(7 downto 0);
signal i: std_logic_vector(7 downto 0);
    signal o: std_logic_vector(7 downto 0);

begin

    nclk <= not clkd2;
   
    CD: Clock_Divider port map(clk, reset, clkd1);
    CD1: ClockD port map(clkd1, reset, clkd2);
N: note port map(i,o);
    AN: amostrador port map(clkd1, serial_in, dado);
    UC: receptor_UC port map(nclk, reset, erro, cont, reset_b, en, up, dado);
    DA: dado_reg port map(clkd2, reset_b, en, dado, regD);
    CO: contador port map(clkd2, reset_b, en, cont);
    PA: paridade port map(regD, erro);
    MD: disp_mem port map(clkd2, up, displayD, display_F);
   
    D: display_dec port map(to_stdlogicvector(display_F), display_not);
    i <= display_not;
display <= o;

    displayD <= regD(8 downto 1);
end architecture;

library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_bit.all;

entity receptor_UC is
    port (
        -- Controle GLobal
        clk, reset: in bit;
        -- Sinais de condição
        erro: in bit;
        contador: in bit_vector(3 downto 0);
        -- Sinais de controle
        reset_b: out bit;
        en: out bit;
        up: out bit;
        -- Entrada
        dado: in bit
    );
end receptor_UC;

architecture UC_Arch of receptor_UC is
    type stat_t is (espera_s, captura_s, paridade_s, up_display_s, nup_display_s);
    signal next_state, current_state: stat_t;
begin
    -- Maquina de estados finita
    process(clk, reset)
    begin
        if reset = '1' then
            current_state <= espera_s;
        elsif clk = '1' and clk'EVENT then
            current_state <= next_state;
        end if;
    end process;

    -- Lógica do próximo estado
    next_state <=
        espera_s when (current_state = up_display_s) or (current_state = nup_display_s) else
        captura_s when (current_state = espera_s and dado = '0') or (current_state = captura_s and unsigned(contador) < 9) else
        paridade_s when (current_state = captura_s and unsigned(contador) >= 9) else
        up_display_s when (current_state = paridade_s and erro = '0') else
        nup_display_s when (current_state = paridade_s and erro = '1');

    reset_b <= '1' when (current_state = espera_s) else '0';
    en <= '1' when (current_state = captura_s) else '0';
    up <= '1' when (current_state = up_display_s) else '0';
       
end architecture;


library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_bit.all;

entity contador is
    port (
        clk, reset: in bit;
        en: in bit;

        dado: out bit_vector(3 downto 0)
    );
end contador;

architecture contador_Arch of contador is
    signal valor: unsigned(3 downto 0) := "0000";
signal valor1: unsigned(3 downto 0);
begin
dado <= bit_vector(valor1);

    process(clk, reset)
    begin
 if reset = '1' then
            valor <= "0000";
        end if;
        if  en = '1' then
            valor <= valor + 1;
        end if;
    end process;
valor1 <= valor;
   
end architecture;


library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_bit.all;

entity paridade is
    port (
        dado: in bit_vector(8 downto 0);

        erro: out bit
    );
end paridade;

architecture paridade_Arch of paridade is
begin
    erro <= dado(0) xor dado(1) xor dado(2) xor dado(3) xor dado(4) xor dado(5) xor dado(6) xor dado(7) xor dado(8);
end architecture;


library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_bit.all;

entity amostrador is
    port (
        clk: in bit;
        dado_in: in bit;

        dado_out: out bit
    );
end amostrador;

architecture amostrador_Arch of amostrador is
    signal shift_reg: bit_vector(3 downto 0) := "1111";
begin

    process(clk)
    begin
        if clk = '1' and clk'EVENT then
            shift_reg <= dado_in & shift_reg(3 downto 1);
        end if;
    end process;

    dado_out <= shift_reg(2);
end architecture;


library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_bit.all;

entity disp_mem is
    port(
        clk: in bit;
        up: in bit;
        dado: in bit_vector(7 downto 0);

        display: out bit_vector(7 downto 0)
    );
end disp_mem;

architecture disp_mem_Arch of disp_mem is
    signal reg: bit_vector(7 downto 0);
begin
    process(clk)
    begin
        if clk = '1' and clk'EVENT then
            if up = '1' then
                reg <= dado;
            end if;
        end if;
    end process;

    display <= reg;
end architecture;


library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_bit.all;

entity dado_reg is
    port (
        clk, reset: in bit;
        en, dado: in bit;
        dado_out: out bit_vector(8 downto 0)
    );
end dado_reg;

architecture dado_reg_Arch of dado_reg is
    signal shift_reg: bit_vector(8 downto 0);
begin
    process(clk)
    begin
        if clk = '1' and clk'EVENT then
            if reset = '1' then
                shift_reg <= "000000000";
            elsif en = '1' then
            shift_reg <= dado & shift_reg(8 downto 1);
            end if;
        end if;
    end process;

    dado_out <= shift_reg;
end architecture;

library IEEE;
use IEEE.numeric_std.ALL;

entity Clock_Divider is
    port (
        clk, reset: in bit;
        clock_out: out bit
    );
end Clock_Divider;

architecture bhv of Clock_Divider is
    signal count: integer := 1;
    signal tmp: bit := '0';
begin
    process(clk, reset)
    begin
        if (reset = '1') then
            count <= 1;
            tmp <= '0';
        elsif (clk'event and clk = '1') then
            count <= count + 1;
            if (count = 31250) then
                tmp <= NOT tmp;
                count <= 1;
            end if;
        end if;
        clock_out <= tmp;
    end process;
end bhv;

entity ClockD is
    port (
        clk, reset: in bit;
        clock_out: out bit
    );
end ClockD;

architecture bhv2 of ClockD is
    signal count: integer := 1;
    signal tmp: bit := '0';
begin
    process(clk, reset)
    begin
        if (reset = '1') then
            count <= 1;
            tmp <= '0';
        elsif (clk'event and clk = '1') then
            count <= count + 1;
            if (count = 4) then
                tmp <= NOT tmp;
                count <= 1;
            end if;
        end if;
        clock_out <= tmp;
    end process;
end bhv2;

library ieee;
use ieee.std_logic_1164.all;
entity note is
port (
i: in std_logic_vector (7 downto 0);
o: out std_logic_vector (7 downto 0)
);
end note;

architecture a of note is
begin
o(0) <= i(0) xor '1';
o(1) <= i(1) xor '1';
o(2) <= i(2) xor '1';
o(3) <= i(3) xor '1';
o(4) <= i(4) xor '1';
o(5) <= i(5) xor '1';
o(6) <= i(6) xor '1';
o(7) <= i(7) xor '1';
end a;

library ieee;
use ieee.std_logic_1164.all;



entity display_dec is
  port (
    input: in   std_logic_vector(7 downto 0); -- ASCII 8 bits
    output: out std_logic_vector(7 downto 0)  -- ponto + abcdefg
  );
end display_dec;

architecture comb of display_dec is
begin
    with input select output <=
        "00000000" when "00100000", -- (space)
        "10000110" when "00100001", -- !
        "00100010" when "00100010", -- "
        "01111110" when "00100011", -- #
        "01101101" when "00100100", -- $
        "11010010" when "00100101", -- %
        "01000110" when "00100110", -- &
        "00100000" when "00100111", -- '
        "00101001" when "00101000", -- (
        "00001011" when "00101001", -- )
        "00100001" when "00101010", -- *
        "01110000" when "00101011", -- +
        "00010000" when "00101100", -- ,
        "01000000" when "00101101", -- -
        "10000000" when "00101110", -- .
        "01010010" when "00101111", -- /
        "00111111" when "00110000", -- 0
        "00000110" when "00110001", -- 1
        "01011011" when "00110010", -- 2
        "01001111" when "00110011", -- 3
        "01100110" when "00110100", -- 4
        "01101101" when "00110101", -- 5
        "01111101" when "00110110", -- 6
        "00000111" when "00110111", -- 7
        "01111111" when "00111000", -- 8
        "01101111" when "00111001", -- 9
        "00001001" when "00111010", -- :
        "00001101" when "00111011", -- ;
        "01100001" when "00111100", -- <
        "01001000" when "00111101", -- =
        "01000011" when "00111110", -- >
        "11010011" when "00111111", -- ?
        "01011111" when "01000000", -- @
        "01110111" when "01000001", -- A
        "01111100" when "01000010", -- B
        "00111001" when "01000011", -- C
        "01011110" when "01000100", -- D
        "01111001" when "01000101", -- E
        "01110001" when "01000110", -- F
        "00111101" when "01000111", -- G
        "01110110" when "01001000", -- H
        "00110000" when "01001001", -- I
        "00011110" when "01001010", -- J
        "01110101" when "01001011", -- K
        "00111000" when "01001100", -- L
        "00010101" when "01001101", -- M
        "00110111" when "01001110", -- N
        "00111111" when "01001111", -- O
        "01110011" when "01010000", -- P
        "01101011" when "01010001", -- Q
        "00110011" when "01010010", -- R
        "01101101" when "01010011", -- S
        "01111000" when "01010100", -- T
        "00111110" when "01010101", -- U
        "00111110" when "01010110", -- V
        "00101010" when "01010111", -- W
        "01110110" when "01011000", -- X
        "01101110" when "01011001", -- Y
        "01011011" when "01011010", -- Z
        "00111001" when "01011011", -- [
        "01100100" when "01011100", -- \
        "00001111" when "01011101", -- ]
        "00100011" when "01011110", -- ^
        "00001000" when "01011111", -- _
        "00000010" when "01100000", -- `
        "01011111" when "01100001", -- a
        "01111100" when "01100010", -- b
        "01011000" when "01100011", -- c
        "01011110" when "01100100", -- d
        "01111011" when "01100101", -- e
        "01110001" when "01100110", -- f
        "01101111" when "01100111", -- g
        "01110100" when "01101000", -- h
        "00010000" when "01101001", -- i
        "00001100" when "01101010", -- j
        "01110101" when "01101011", -- k
        "00110000" when "01101100", -- l
        "00010100" when "01101101", -- m
        "01010100" when "01101110", -- n
        "01011100" when "01101111", -- o
        "01110011" when "01110000", -- p
        "01100111" when "01110001", -- q
        "01010000" when "01110010", -- r
        "01101101" when "01110011", -- s
        "01111000" when "01110100", -- t
        "00011100" when "01110101", -- u
        "00011100" when "01110110", -- v
        "00010100" when "01110111", -- w
        "01110110" when "01111000", -- x
        "01101110" when "01111001", -- y
        "01011011" when "01111010", -- z
        "01000110" when "01111011", -- {
        "00110000" when "01111100", -- |
        "01110000" when "01111101", -- }
        "00000001" when "01111110", -- ~
        "00000000" when "01111111", -- (del)
        "00000000" when others;

end architecture;