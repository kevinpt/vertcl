
library ieee;
use ieee.numeric_std.all;


package vt_bitwise is
  subtype bitwise is integer;
  
  constant BITWISE_SIZE : natural := 32;
  
  function "and" (l, r : bitwise) return bitwise;
  function "or" (l, r : bitwise) return bitwise;
  function "xor" (l, r : bitwise) return bitwise;
  function "not" (r : bitwise) return bitwise;
  function shift_left(l : bitwise; s : natural) return bitwise;
  function shift_right(l : bitwise; s : natural) return bitwise;
end package;

package body vt_bitwise is

  function "and" (l, r : bitwise) return bitwise is
    variable sl, sr : signed(BITWISE_SIZE-1 downto 0);
  begin
    sl := to_signed(l, sl'length);
    sr := to_signed(r, sr'length);
    
    return to_integer(sl and sr);
  end function;

  function "or" (l, r : bitwise) return bitwise is
    variable sl, sr : signed(BITWISE_SIZE-1 downto 0);
  begin
    sl := to_signed(l, sl'length);
    sr := to_signed(r, sr'length);
    
    return to_integer(sl or sr);
  end function;

  function "xor" (l, r : bitwise) return bitwise is
    variable sl, sr : signed(BITWISE_SIZE-1 downto 0);
  begin
    sl := to_signed(l, sl'length);
    sr := to_signed(r, sr'length);
    
    return to_integer(sl xor sr);
  end function;

  function "not" (r : bitwise) return bitwise is
    variable sr : signed(BITWISE_SIZE-1 downto 0);
  begin
    sr := to_signed(r, sr'length);
    
    return to_integer(not sr);
  end function;
  
  
  function shift_left(l : bitwise; s : natural) return bitwise is
    variable sl : signed(BITWISE_SIZE-1 downto 0);
  begin
    sl := to_signed(l, sl'length);
    return to_integer(shift_left(sl, s));
  end function;

  function shift_right(l : bitwise; s : natural) return bitwise is
    variable sl : signed(BITWISE_SIZE-1 downto 0);
  begin
    sl := to_signed(l, sl'length);
    return to_integer(shift_right(sl, s));
  end function;


end package body;
