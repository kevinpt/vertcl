-------------------------------------------------------------------
--            ___ __      __       _______     _  ___            --
--           |  _|\ \    / /      |__   __|   | ||_  |           --
--           | |   \ \  / /___  _ __ | |  ___ | |  | |           --
--           | |    \ \/ // _ \| '__|| | / __|| |  | |           --
--           | |     \  /|  __/| |   | || (__ | |  | |           --
--           | |_     \/  \___||_|   |_| \___||_| _| |           --
--           |___|                               |___|           --
--                                                               --
--            - ---==## VHDL Tcl interpreter ##==--- -           --
-------------------------------------------------------------------
--# vt_expr_lexer.vhdl - Expression lexer for the expr command
--# Freely available from http://code.google.com/p/vertcl
--#
--# Copyright © 2015 Kevin Thibedeau
--# (kevin 'period' thibedeau 'at' gmail 'punto' com)
--#
--# This source is released under the terms of the MIT license.
--# See the LICENSE file for more details.
-------------------------------------------------------------------
use std.textio.all;

library extras;
use extras.strings_unbounded.unbounded_string;
use extras.text_buffering.all;

library vertcl;
use vertcl.vt_lexer.to_hex_number;

package vt_expr_lexer is

  alias char is extras.characters_handling;
  alias SU is extras.strings_unbounded;


  type vt_etoken_kind is (
    ETOK_UNKNOWN, ETOK_EOB, ETOK_identifier, ETOK_integer, ETOK_float,
    ETOK_lparen, ETOK_rparen, ETOK_comma,

    -- NOTE: Operator tokens must be grouped by class so that the precedence
    -- tables can be constructed in the parser using ranges of this type as
    -- an index.

    -- Unary only operators
    ETOK_bit_not, ETOK_not,

    -- Unary and binary operators
    ETOK_minus, ETOK_plus,

    -- Binary operators
    ETOK_mul, ETOK_div, ETOK_mod, ETOK_pow,
    ETOK_eq, ETOK_ne, ETOK_lt, ETOK_gt, ETOK_le, ETOK_ge,
    ETOK_and, ETOK_or,
    
    ETOK_bit_and, ETOK_bit_or, ETOK_bit_xor
  );

  subtype vt_etoken_unary is vt_etoken_kind range ETOK_bit_not to ETOK_plus;
  subtype vt_etoken_binary is vt_etoken_kind range ETOK_minus to ETOK_bit_xor;

  type vt_etoken is record
    kind  : vt_etoken_kind;   -- Identify contents of token
    data  : unbounded_string; -- Any non-numeric value for the token
    int   : integer;          -- Integer token value when kind = ETOK_integer
    float : real;             -- Real token value then kind = ETOK_float
  end record;

  -- Set longest identifier size for internal buffer string
  constant ELEX_MAX_IDENTIFIER_SIZE : natural := 256;

  type expr_lex;
  type expr_lex_acc is access expr_lex;
  type expr_lex is record
    --buf : text_buffer;                            -- Expression string being lexed  -- FIXME: This doesn't need to be a tbuffer
    --cur_line : unbounded_string;                  -- Current line of text
    expr : unbounded_string;                      -- Expression to evaluate

    ch   : character;                             -- Current character
    tstr : string(1 to ELEX_MAX_IDENTIFIER_SIZE); -- Temporary buffer string

    tok : vt_etoken;                              -- Token object built by lexer, functions as a buffer in lookahead operations
    valid_token : boolean;                        -- Indicates if the tok is valid
  end record;


  -- ## Initialize expression lexer
  procedure elexer_init( expr : inout unbounded_string; ELO : inout expr_lex_acc );

  -- ## Get next token from lexer
  -- #  If the token is used you must call consume_token() to invalidate the buffered tok
  procedure next_token( ELO : inout expr_lex_acc; tok : out vt_etoken );

  -- ## Mark the buffered tok object as invalid
  procedure consume_token( ELO : inout expr_lex_acc );

  -- ## Allocate a new copy of a token
  procedure copy_token( variable source : in vt_etoken; variable dest : inout vt_etoken );

  -- ## Free the data string from a token
  procedure free( tok : inout vt_etoken );

  -- ## Deallocate an expression lexer
  procedure free( ELO : inout expr_lex_acc );

  -- ## Convert a token to a string representation
  procedure to_unbounded_string( variable tok : in vt_etoken; dest : out unbounded_string);

  -- ## Convert a numeric token to a real
  procedure to_real( variable tok : in vt_etoken; dest : out real );
end package;


--library extras;
--use extras.characters_handling;

package body vt_expr_lexer is

-- PRIVATE procedures:
-- ===================

  -- // Validate test conditions with standard error message format
  procedure assert_true(test : boolean; msg : string; severity_lvl : severity_level;
    variable ELO : in expr_lex_acc) is
  begin
    if not test then
      assert false report "EXPR LEXER: " & msg
        severity severity_lvl;
    end if;
  end procedure;


  -- // Get the next character from a text buffer
  -- FIXME: Remove tbuffer support
  procedure get_next_char( ELO : inout expr_lex_acc ) is
--    variable at_end : boolean;
  begin
--    if ELO.cur_line'length > 0 then -- The line isn't empty
--      read(ELO.cur_line, ELO.ch);

--    else -- the line is consumed
--      endbuffer(ELO.buf, at_end);
--      if not at_end then
--        deallocate(ELO.cur_line);
--        nextline(ELO.buf, ELO.cur_line);
--        ELO.ch := LF;
--      else
--        ELO.ch := NUL;
--      end if;
--    end if;
    if ELO.expr'length > 0 then -- The line isn't empty
      read(ELO.expr, ELO.ch);
    else
      ELO.ch := NUL;
    end if;
  end procedure;


  -- // Return true if character is valid for an identifier name
--  function is_identchar( ch : character ) return boolean is
--  begin
--    --return char.is_graphic(ch);

-- -- FIXME: fix these chars

--    if char.is_alphanumeric(ch) or ch = '$' or ch = '_' or ch = '-' or ch = '.' then
--      return true;
--    else
--      return false;
--    end if;
--  end function;

  alias maps is extras.strings_maps; 

  constant IDENTIFIER_CHAR_SET : maps.character_set := (
    '0'                         to  '9'                        => true,
    'A'                         to  'Z'                        => true,
    'a'                         to  'z'                        => true,
    others  =>  false
  );

  -- // Return true if character is valid for an identifier name
  function is_identchar( ch : character ) return boolean is
  begin
    return maps.is_in(ch, IDENTIFIER_CHAR_SET);
  end function;


  -- // Convert a number character to an integer. Assumes ch is a digit.
  function to_number( ch : character ) return integer is -- FIXME: move to common package
  begin
    return character'pos(ch) - character'pos('0');
  end function;


  constant OCTAL_CHAR_SET : maps.character_set := (
    '0'                         to  '7'                        => true,
    others  =>  false
  );

  -- // Return true if character is valid for an octal digit
  function is_octal_digit( ch : character ) return boolean is
  begin
    return maps.is_in(ch, OCTAL_CHAR_SET);
  end function;

-- PUBLIC procedures:
-- ==================

  -- ## Initialize expression lexer
  procedure elexer_init( expr : inout unbounded_string; ELO : inout expr_lex_acc ) is
  begin
    ELO := new expr_lex;

    SU.copy(expr, ELO.expr);
--    ELO.buf := buf;
--    deallocate(ELO.cur_line);
--    nextline(ELO.buf, ELO.cur_line); -- Load the first line

--    assert_true(ELO.cur_line /= null, "Empty buffer", failure, ELO);

    get_next_char(ELO); -- Read the first character

    free(ELO.tok);
    ELO.tok.kind := ETOK_UNKNOWN;
    ELO.valid_token := false;
  end procedure;


  -- ## Get next token from lexer
  -- #  If the token is used you must call consume_token() to invalidate the buffered tok
  procedure next_token( ELO : inout expr_lex_acc; tok : out vt_etoken ) is

    type elex_state is ( START, IDENTIFIER, SYMBOL, SYMBOL_STAR,
      SYMBOL_EQ, SYMBOL_EXCLAM, SYMBOL_LT, SYMBOL_GT, SYMBOL_AND, SYMBOL_OR,
      BASED_LIT, HEX_LIT, BINARY_LIT, OCTAL_LIT, INTEGER_LIT, FLOAT_LIT, FLOAT_EXPONENT, EOB );

    variable lex_st      : elex_state := START;
    variable active      : boolean := true;
    variable tstr_copy   : unbounded_string;
    variable tslen       : natural := 0;
    variable tval        : integer := 0;
    variable tfloat      : real;
    variable frac_digits : natural;
    variable at_end      : boolean;
    variable esign       : integer;
  begin

    if ELO.valid_token = false then -- prepare the next token
      ELO.tok.data := null;
      ELO.tok.int := 0;
      ELO.tok.float := 0.0;

      while active = true loop
        case lex_st is
          when START =>
            case ELO.ch is
              when ' ' | HT | CR | VT | FF => -- Whitespace
                null;

              -- Arithmetic ops
              when '(' | ')' | '+' | '-' | '/' | ',' | '~' | '^' =>
                ELO.tstr(1) := ELO.ch;
                tslen := 1;
                lex_st := SYMBOL;

              -- Two character operators require an intermediate state to disambiguate:
              when '*' =>
                lex_st := SYMBOL_STAR;   -- Handle * and **
                
              -- Relational ops

              when '=' =>
                lex_st := SYMBOL_EQ;     -- Handle ==

              when '!' =>
                lex_st := SYMBOL_EXCLAM; -- Handle !=

              when '<' =>
                lex_st := SYMBOL_LT;     -- Handle < and <=

              when '>' =>
                lex_st := SYMBOL_GT;     -- Handle > and >=
                
              when '&' =>
                lex_st := SYMBOL_AND;

              when '|' =>
                lex_st := SYMBOL_OR;


              when NUL =>
                lex_st := EOB;           -- End of buffer
                
              -- Numeric literals
                
              when '.' =>
                tfloat := 0.0;
                frac_digits := 0;
                lex_st := FLOAT_LIT;
                
              when '0' =>
                tval := 0;
                lex_st := BASED_LIT;

              when others =>
                if char.is_digit(ELO.ch) then -- Integer
                  tval := to_number(ELO.ch);
                  lex_st := INTEGER_LIT;

                elsif is_identchar(ELO.ch) then -- Identifier
                  ELO.tstr(1) := ELO.ch;
                  tslen := 1;
                  lex_st := IDENTIFIER;

                else
                  assert_true(false, "Invalid character '" & ELO.ch & "'", error, ELO);
                  active := false;
                end if;
            end case;

          when SYMBOL =>
            case ELO.tstr(1) is
              when '(' => ELO.tok.kind := ETOK_lparen;
              when ')' => ELO.tok.kind := ETOK_rparen;
              when '+' => ELO.tok.kind := ETOK_plus;
              when '-' => ELO.tok.kind := ETOK_minus;
              when '/' => ELO.tok.kind := ETOK_div;
              when ',' => ELO.tok.kind := ETOK_comma;
              when '~' => ELO.tok.kind := ETOK_bit_not;
              when '^' => ELO.tok.kind := ETOK_bit_xor;
              when others => ELO.tok.kind := ETOK_UNKNOWN;
            end case;
            ELO.valid_token := true;
            active := false;

          when SYMBOL_STAR =>
            if ELO.ch = '*' then    -- Found "**", exponentiation operator
              get_next_char(ELO); -- Consume this character
              ELO.tok.kind := ETOK_pow;
            else -- single '*' mul operator
              ELO.tok.kind := ETOK_mul;
            end if;
            ELO.valid_token := true;
            active := false;

          when SYMBOL_EQ =>
            if ELO.ch = '=' then    -- Found "==" equality operator
              get_next_char(ELO); -- Consume this character
              ELO.tok.kind := ETOK_eq;
              ELO.valid_token := true;
            else -- Bare '=', invalid token
              assert_true(false, "Expr lexer: Invalid character '" & ELO.ch & "'", error, ELO);
            end if;
            active := false;

          when SYMBOL_EXCLAM =>
            if ELO.ch = '=' then    -- Found "!=" inequality operator
              get_next_char(ELO); -- Consume this character
              ELO.tok.kind := ETOK_ne;
              ELO.valid_token := true;
            else -- Bare '!', invalid token
              assert_true(false, "Expr lexer: Invalid character '" & ELO.ch & "'", error, ELO);
            end if;
            active := false;

          when SYMBOL_LT =>
            if ELO.ch = '=' then    -- '<=' operator
              get_next_char(ELO); -- Consume this character
              ELO.tok.kind := ETOK_le;
            else -- Single '<' less-than operator
              ELO.tok.kind := ETOK_lt;
            end if;
            ELO.valid_token := true;
            active := false;

          when SYMBOL_GT =>
            if ELO.ch = '=' then    -- '>=' operator
              get_next_char(ELO); -- Consume this character
              ELO.tok.kind := ETOK_ge;
            else -- Single '>' greater-than operator
              ELO.tok.kind := ETOK_gt;
            end if;
            ELO.valid_token := true;
            active := false;
            
          when SYMBOL_AND =>
            if ELO.ch = '&' then    -- '&&' operator
              get_next_char(ELO); -- Consume this character
              ELO.tok.kind := ETOK_and;
            else -- Single '&' bit-and operator
              ELO.tok.kind := ETOK_bit_and;
            end if;
            ELO.valid_token := true;
            active := false;

          when SYMBOL_OR =>
            if ELO.ch = '|' then    -- '||' operator
              get_next_char(ELO); -- Consume this character
              ELO.tok.kind := ETOK_or;
            else -- Single '|' bit-or operator
              ELO.tok.kind := ETOK_bit_or;
            end if;
            ELO.valid_token := true;
            active := false;
            
          when BASED_LIT =>
            if ELO.ch = 'x' or ELO.ch = 'X' then -- Hex
              lex_st := HEX_LIT;
            elsif ELO.ch = 'b' or ELO.ch = 'B' then -- Binary
              lex_st := BINARY_LIT;
            elsif ELO.ch = 'o' then -- Octal
              lex_st := OCTAL_LIT;
            elsif ELO.ch = '.' then -- Float
              tfloat := 0.0;
              frac_digits := 0;
              lex_st := FLOAT_LIT;
            elsif is_octal_digit(ELO.ch) then -- Assume Octal
              tval := to_number(ELO.ch);
              lex_st := OCTAL_LIT;
            else -- End of number 0
              ELO.tok.kind := ETOK_integer;
              ELO.tok.int := tval;
              ELO.valid_token := true;
              active := false;
            end if;
            
          when HEX_LIT =>
            if char.is_hexadecimal_digit(ELO.ch) then
              tval := tval*16 + to_hex_number(ELO.ch);
            else -- Done
              ELO.tok.kind := ETOK_integer;
              ELO.tok.int := tval;
              ELO.valid_token := true;
              active := false;
            end if;

          when BINARY_LIT =>
            if ELO.ch = '0' or ELO.ch = '1' then
              tval := tval*2 + to_number(ELO.ch);
            else -- Done
              ELO.tok.kind := ETOK_integer;
              ELO.tok.int := tval;
              ELO.valid_token := true;
              active := false;
            end if;

          when OCTAL_LIT =>
            if is_octal_digit(ELO.ch) then
              tval := tval*8 + to_number(ELO.ch);
            else -- Done
              ELO.tok.kind := ETOK_integer;
              ELO.tok.int := tval;
              ELO.valid_token := true;
              active := false;
            end if;


          when INTEGER_LIT =>
            if char.is_digit(ELO.ch) then
              tval := (tval * 10) + to_number(ELO.ch);
            elsif ELO.ch = '.' then -- Real number
              tfloat := real(tval);
              frac_digits := 0;
              lex_st := FLOAT_LIT;
            else
              ELO.tok.kind := ETOK_integer;
              ELO.tok.int := tval;
              ELO.valid_token := true;
              active := false;
            end if;

          when FLOAT_LIT =>
            if char.is_digit(ELO.ch) then
              tfloat := (tfloat * 10.0) + real(to_number(ELO.ch));
              frac_digits := frac_digits + 1;
            elsif ELO.ch = 'e' or ELO.ch = 'E' then
              tfloat := tfloat / 10.0**frac_digits;
              esign := 1;
              tval := 0;
              lex_st := FLOAT_EXPONENT;
            else
              tfloat := tfloat / 10.0**frac_digits;
              ELO.tok.kind  := ETOK_float;
              ELO.tok.float := tfloat;
              ELO.valid_token := true;
              active := false;
            end if;

          when FLOAT_EXPONENT =>
            if ELO.ch = '-' then
              esign := -1;
            elsif ELO.ch = '+' then
              esign := 1;
            elsif char.is_digit(ELO.ch) then
              tval := tval*10 + to_number(ELO.ch);
            else -- Exponent complete
              tval := tval * esign;
              tfloat := tfloat * 10.0**tval;
              ELO.tok.kind := ETOK_float;
              ELO.tok.float := tfloat;
              ELO.valid_token := true;
              active := false;
            end if;

          when IDENTIFIER =>
            if is_identchar(ELO.ch) then
              tslen := tslen + 1;
              ELO.tstr(tslen) := ELO.ch;
            else -- The identifier ended. Return it as a token.
              ELO.tok.kind := ETOK_identifier;
              SU.copy(ELO.tstr, ELO.tok.data, tslen);
              ELO.valid_token := true;
              active := false;
            end if;


          when EOB =>
            ELO.tok.kind := ETOK_EOB;
            ELO.valid_token := true;
            active := false;

          when others =>
            ELO.tok.kind := ETOK_UNKNOWN;
            ELO.valid_token := true;
            active := false;
        end case;

        if active then -- Still processing a token
          get_next_char(ELO);
        end if;
      end loop;

--      report "EXPR: new token: " & vt_etoken_kind'image(ELO.tok.kind) & " " & integer'image(ELO.tok.int);
    end if;

    tok := ELO.tok;
  end procedure;

  -- ## Mark the buffered tok object as invalid
  procedure consume_token( ELO : inout expr_lex_acc ) is
  begin
    if ELO.valid_token then
      ELO.tok.data := null;
      ELO.tok.kind := ETOK_UNKNOWN;
      ELO.valid_token := false;
    end if;
  end procedure;

  -- ## Allocate a new copy of a token
  procedure copy_token( variable source : in vt_etoken; variable dest : inout vt_etoken ) is
  begin
    dest := source;
    if source.data /= null then
      SU.copy(source.data, dest.data);
    end if;

  end procedure;


  -- ## Free the data string from a token
  procedure free( tok : inout vt_etoken ) is
  begin
    SU.free(tok.data);
  end procedure;


  -- ## Deallocate an expression lexer
  procedure free( ELO : inout expr_lex_acc ) is
  begin
    SU.free(ELO.expr);
    --deallocate(ELO.cur_line);
    free(ELO.tok);
    deallocate(ELO);
  end procedure;

  -- ## Convert a token to a string representation
  procedure to_unbounded_string( variable tok : in vt_etoken; dest : out unbounded_string) is
    variable tstr : unbounded_string;
  begin
    case tok.kind is
      when ETOK_identifier => SU.copy(tok.data,tstr);
      when ETOK_integer    => tstr := SU.to_unbounded_string(integer'image(tok.int));
      when ETOK_float      => tstr := SU.to_unbounded_string(real'image(tok.float));
      when others          => tstr := SU.to_unbounded_string("$" & vt_etoken_kind'image(tok.kind) & "$");
    end case;

    dest := tstr;
  end procedure;

  -- ## Convert a numeric token to a real
  procedure to_real( variable tok : in vt_etoken; dest : out real ) is
  begin
    if tok.kind = ETOK_float then
      dest := tok.float;
    else
      dest := real(tok.int);
    end if;
  end procedure;

end package body;
