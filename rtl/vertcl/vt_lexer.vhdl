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
--# vt_lexer.vhdl - VerTcl lexer
--# Freely available from http://code.google.com/p/vertcl
--#
--# Copyright © 2015 Kevin Thibedeau
--# (kevin 'period' thibedeau 'at' gmail 'punto' com)
--#
--# This source is released under the terms of the MIT license.
--# See the LICENSE file for more details.
-------------------------------------------------------------------
use std.textio.read;

library extras;
use extras.strings_unbounded.unbounded_string;
use extras.text_buffering.all;

package vt_lexer is

  alias char is extras.characters_handling;
  alias SU is extras.strings_unbounded;


  type vt_token_kind is (
    TOK_UNKNOWN, TOK_string, TOK_integer, TOK_float, TOK_hex_number,
    TOK_group_begin, TOK_group_end, TOK_subst_begin, TOK_subst_end,
    TOK_string_seg, TOK_string_seg_end, TOK_splat,
    TOK_EOL, TOK_EOB
  );

  type vt_token is record
    kind  : vt_token_kind;    -- Identify contents of token
    data  : unbounded_string; -- Any non-numeric value for the token
    value : integer;          -- Integer token value when kind = ETOK_integer. Used as line number for other tokens
    float : real;             -- Real token value then kind = ETOK_float
  end record;

  -- Set longest identifier size for internal buffer string
  constant VTLEX_MAX_IDENTIFIER_SIZE : natural := 256;

  type vt_lex;
  type vt_lex_acc is access vt_lex;
  type vt_lex is record
    buf              : text_buffer;      -- Command list being lexed
    cur_line         : unbounded_string; -- Current line to tokenize
    line_num         : natural;          -- Current line number
    --start_line       : natural;          -- Start line number for ??? FIXME

    multi_line       : boolean;          -- Previous line(s) ended with "\"
    permit_comment   : boolean;          -- Current context permits # to be interpreted as the start of a comment
    segmented_string : boolean;          -- Lexing a quoted string containing command substitution
    subst_depth      : integer;          -- Current depth of substitutions in a segmented string

    ch               : character;        -- Current character
    tstr             : string(1 to VTLEX_MAX_IDENTIFIER_SIZE); -- Temporary buffer string

    tok              : vt_token;         -- Token object built by lexer, functions as a buffer in lookahead operations
    valid_token      : boolean;          -- Indicates if the tok is valid
  end record;

  -- ## Convert a number character to an integer. Assumes ch is a digit.
  function to_number( ch : character ) return integer;
  
  -- ## Convert a hex number character to an integer. Assumes ch is a hex digit.
  function to_hex_number( ch : character ) return integer;

  -- ## Initialize the lexer state record
  procedure vt_lexer_init( variable buf : in text_buffer; VLO : inout vt_lex_acc );

  -- ## Produce the next token from the script
  procedure next_token( VLO : inout vt_lex_acc; tok : out vt_token );
  
  -- ## Mark the buffered tok object as invalid
  procedure consume_token( VLO : inout vt_lex_acc );
  
  -- ## Allocate a new copy of a token
  procedure copy_token( variable source : in vt_token; variable dest : inout vt_token );

  -- ## Free the data string from a token
  procedure free( tok : inout vt_token );
  
  -- ## Free internal dynamic data for the lexer
  procedure free( VLO : inout vt_lex_acc );

  -- ## Convert a token to a string representation
  procedure to_unbounded_string( variable tok : in vt_token; dest : out unbounded_string);
  
end package;


library extras;
use extras.characters_handling;
use extras.strings_maps;

package body vt_lexer is


-- PRIVATE procedures:
-- ===================

  -- // Validate test conditions with standard error message format
  procedure assert_true(test : boolean; msg : string; severity_lvl : severity_level;
    variable VLO : in vt_lex_acc) is
  begin
    if not test then
      assert false report "LEXER: " & msg & LF
        & "(At line " & integer'image(VLO.line_num) & ")"
        severity severity_lvl;
    end if;
  end procedure;


  -- // Get the next character from a text buffer
  procedure get_next_char( VLO : inout vt_lex_acc ) is
    variable at_end : boolean;
  begin
    if VLO.cur_line'length > 0 then -- The line isn't empty
      read(VLO.cur_line, VLO.ch);

    else -- The line is consumed; Get the next line
      endbuffer(VLO.buf, at_end);
      if not at_end then
        deallocate(VLO.cur_line);
        VLO.line_num := VLO.buf.cur_line_num;
        nextline(VLO.buf, VLO.cur_line);

        VLO.ch := LF;
      else
        VLO.ch := NUL;
      end if;
    end if;
  end procedure;


  -- // Look at the next character in the current line buffer. Returns NUL if buffer is empty.
  procedure peek_next_char( VLO : inout vt_lex_acc; ch : out character ) is
  begin
    if VLO.cur_line'length > 0 then
      ch := VLO.cur_line(1);
    else
      ch := NUL;
    end if;
  end procedure;


  alias maps is extras.strings_maps; 

  constant IDENTIFIER_CHAR_SET : maps.character_set := (
    '!'                         to  '!'                        => true,
    '$'                         to  '/'                        => true,
    '0'                         to  '9'                        => true,
    ':'                         to  ':'                        => true,
    '<'                         to  '@'                        => true,
    'A'                         to  'Z'                        => true,
    'a'                         to  'z'                        => true,
    '\'                         to  '\'                        => true,
    '^'                         to  '`'                        => true,
    '|'                         to  '|'                        => true,
    '~'                         to  '~'                        => true,
    others  =>  false
  );

  -- // Return true if character is valid for an identifier name
  function is_identchar( ch : character ) return boolean is
  begin
    return maps.is_in(ch, IDENTIFIER_CHAR_SET);
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


  constant INTEGER_TERMINATOR : maps.character_set := (
    ' '|']'|'}'|';'|HT|LF|NUL => true,
    others => false
  );

-- PUBLIC procedures:
-- ==================

  -- ## Convert a number character to an integer. Assumes ch is a digit.
  function to_number( ch : character ) return integer is
  begin
    return character'pos(ch) - character'pos('0');
  end function;

  -- ## Convert a hex number character to an integer. Assumes ch is a hex digit.
  function to_hex_number( ch : character ) return integer is
  begin
    if character'pos(ch) >= character'pos('A') and character'pos(ch) <= character'pos('F') then
      return character'pos(ch) - character'pos('A') + 10;
    elsif character'pos(ch) >= character'pos('a') and character'pos(ch) <= character'pos('f') then
      return character'pos(ch) - character'pos('a') + 10;
    else
      return character'pos(ch) - character'pos('0');
    end if;
  end function;


  -- ## Initialize the lexer state record
  procedure vt_lexer_init( variable buf : in text_buffer; VLO : inout vt_lex_acc ) is
  begin
    VLO := new vt_lex;

    VLO.buf := buf;

    deallocate(VLO.cur_line);
    VLO.line_num := 1;
    nextline(VLO.buf, VLO.cur_line); -- Load the first line

    assert_true(VLO.cur_line /= null, "Empty buffer", failure, VLO);

--    VLO.start_line := 0;
    VLO.multi_line := false;
    VLO.permit_comment := true;

    get_next_char(VLO); -- Read the first character

    free(VLO.tok);
    VLO.tok.kind := TOK_UNKNOWN;
    VLO.valid_token := false;
  end procedure;


  -- ## Produce the next token from the script
  procedure next_token( VLO : inout vt_lex_acc; tok : out vt_token ) is

    type lexer_state is ( START, IDENTIFIER, STRING_LIT, GROUP_BEGIN, GROUP_END,
      SUBST_BEGIN, SUBST_END, INTEGER_LIT, FLOAT_LIT, FLOAT_EXPONENT, BASED_LIT, HEX_LIT, BINARY_LIT, OCTAL_LIT,
      COMMENT, EOL, EOB );

    variable lex_st    : lexer_state := START; -- FSM state
    variable active    : boolean := true;      -- True when a token is being built

    variable tslen     : natural := 0;         -- Length of buffered temp string in VLO.tstr

    variable tval      : integer := 0;         -- Temp integer value
    variable tfloat    : real;                 -- Temp real value
    variable frac_digits : natural;            -- Value of floating point literal fraction digits after decimal point
    variable sign      : integer := 1;         -- Sign of integer and float literals and exponents

    variable op_char   : boolean := false;     -- Identify if the previous char was a unary + or -
    variable lit_chars : natural := 0;         -- Count of chars after seeinh a + or -
    variable continued_comment : boolean;      -- Flag to handle multi-line comments
    variable next_ch   : character;            -- Peek at next char to detect terminal "\"
    variable found_splat : boolean;            -- Indicate the  presence of {*} operator
    variable prev_escape : boolean;            -- Flag to identify if previous char was an escape "\"

    variable at_end    : boolean;              -- Identify end of buffer

--    variable timg      : unbounded_string; -- FIXME: DBG remove
  begin

    if VLO.valid_token = false then -- Prepare the next token
      VLO.tok.kind := TOK_UNKNOWN;
      VLO.tok.data := null;
      VLO.tok.value := 0;
      VLO.tok.float := 0.0;
      while active = true loop
        case lex_st is
          when START =>
          
            if VLO.segmented_string and VLO.subst_depth = 0 then -- Resume segmented string
              if VLO.ch /= '"' then
                VLO.tstr(1) := VLO.ch;
                tslen := 1;
                lex_st := STRING_LIT;
              else  -- End of segmented string literal
                get_next_char(VLO); -- Skip to next character
                VLO.tok.kind := TOK_string_seg_end;

                VLO.segmented_string := false;
                SU.copy(VLO.tstr, VLO.tok.data, tslen);
                VLO.valid_token := true;
                active := false;
              end if;
            else -- Determine next state based on the character
          
              case VLO.ch is
                when LF | ';' =>
                  if not VLO.multi_line then
                    VLO.tstr(1) := VLO.ch;
                    tslen := 1;
                    lex_st := EOL;
                  else -- Cancel multi-line status
                    VLO.multi_line := false;
                  end if;

                when ' ' | HT | CR | VT | FF => -- Whitespace
                  -- Just consume the whitespace

                when '#' =>
                  if VLO.permit_comment then
                    lex_st := COMMENT;
                  else -- Not a special char
                    VLO.tstr(1) := VLO.ch;
                    tslen := 1;
                    lex_st := IDENTIFIER;
                  end if;

                when '"' => -- Start of string literal (or ending quote of segmented string)
                  tslen := 0;
                  lex_st := STRING_LIT;

                  if not VLO.segmented_string then -- Start of string
                    lex_st := STRING_LIT;

                  else -- End of segmented string literal
                    get_next_char(VLO); -- Skip to next character
                    VLO.tok.kind := TOK_string_seg_end;

                    VLO.segmented_string := false;
                    SU.copy(VLO.tstr, VLO.tok.data, tslen);
                    VLO.valid_token := true;
                    active := false;
                  end if;

                when '+' =>
                  VLO.tstr(1) := VLO.ch;
                  tslen := 1;
                  op_char := true;
                  lex_st := INTEGER_LIT;

                when '-' =>
                  VLO.tstr(1) := VLO.ch;
                  tslen := 1;
                  sign := -1;
                  op_char := true;
                  lex_st := INTEGER_LIT;


                when '\' => -- Line continuation or char escape
                  peek_next_char(VLO, next_ch);
                  if next_ch /= NUL then -- An escaped char
                    VLO.tstr(1) := VLO.ch;
                    tslen := 1;
                    prev_escape := true;
                    lex_st := IDENTIFIER;
                  else -- A line continuation
                    VLO.multi_line := true;
                  end if;

                when '{' =>
                  lex_st := GROUP_BEGIN;

                when '}' =>
                  lex_st := GROUP_END;

                when '[' =>
                  lex_st := SUBST_BEGIN;

                when ']' =>
                  lex_st := SUBST_END;

                when NUL =>
                  lex_st := EOB;
                  
                when '0' =>
                  tval := 0;
                  lex_st := BASED_LIT;

                when others =>
                  if char.is_digit(VLO.ch) then
                    VLO.tstr(1) := VLO.ch;
                    tslen := 1;
                    tval := to_number(VLO.ch);
                    lex_st := INTEGER_LIT;

                  elsif is_identchar(VLO.ch) or VLO.segmented_string then
                    VLO.tstr(1) := VLO.ch;
                    tslen := 1;
                    lex_st := IDENTIFIER;

                  else
                    assert_true(false, "Invalid character '" & VLO.ch, failure, VLO);
                    active := false;
                  end if;
              end case;
            end if;

          when IDENTIFIER => -- Unquoted string
            if is_identchar(VLO.ch) or prev_escape then -- Append to identifier
              tslen := tslen + 1;
              VLO.tstr(tslen) := VLO.ch;
              if not prev_escape and VLO.ch = '\' then
                prev_escape := true;
              else
                prev_escape := false;
              end if;
            else -- The identifier ended. Return it as a token.
--              report "######### END IDENTIFIER: >" & VLO.ch & "<";
              VLO.tok.kind := TOK_string;
              SU.copy(VLO.tstr, VLO.tok.data, tslen);
              VLO.valid_token := true;
              active := false;
            end if;

          when STRING_LIT =>
            assert_true(VLO.ch /= NUL, "Unexpected end of buffer while reading string", failure, VLO);

            if VLO.ch = '[' and not prev_escape then -- Subst command in string
              VLO.segmented_string := true;
              VLO.subst_depth := -1; -- Can't use 0 as it signals the end of substitution
              -- Emit current string segment
              VLO.tok.kind := TOK_string_seg;
              SU.copy(VLO.tstr, VLO.tok.data, tslen);
              VLO.valid_token := true;
              active := false;

            elsif VLO.ch /= '"' or prev_escape then -- Append char to string
              tslen := tslen + 1;
              VLO.tstr(tslen) := VLO.ch;
              if not prev_escape and VLO.ch = '\' then
                prev_escape := true;
              else
                prev_escape := false;
              end if;
            else -- End of string literal
              get_next_char(VLO); -- Skip to next character
              if VLO.segmented_string then
                VLO.tok.kind := TOK_string_seg_end;
              else
                VLO.tok.kind := TOK_string;
              end if;

              VLO.segmented_string := false;
              SU.copy(VLO.tstr, VLO.tok.data, tslen);
              VLO.valid_token := true;
              active := false;
            end if;
            
          when BASED_LIT =>
            if VLO.ch = 'x' or VLO.ch = 'X' then -- Hex
              lex_st := HEX_LIT;
            elsif VLO.ch = 'b' or VLO.ch = 'B' then -- Binary
              lex_st := BINARY_LIT;
            elsif VLO.ch = 'o' then -- Octal
              lex_st := OCTAL_LIT;
            elsif VLO.ch = '.' then -- Float
              tfloat := 0.0;
              frac_digits := 0;
              lex_st := FLOAT_LIT;
            elsif is_octal_digit(VLO.ch) then -- Assume Octal
              tval := to_number(VLO.ch);
              lex_st := OCTAL_LIT;
            elsif INTEGER_TERMINATOR(VLO.ch) then -- End of number, build an integer (for 0)
              VLO.tok.kind := TOK_integer;
              VLO.tok.value := tval * sign;
              VLO.valid_token := true;
              active := false;
            else
              assert_true(false, "Lexer: Invalid based literal character '" & VLO.ch & "'", error, VLO);
              active := false;
            end if;
            
          when HEX_LIT =>
            if char.is_hexadecimal_digit(VLO.ch) then
              tval := tval*16 + to_hex_number(VLO.ch);
            else -- Done
              VLO.tok.kind := TOK_integer;
              VLO.tok.value := tval * sign;
              VLO.valid_token := true;
              active := false;
            end if;

          when BINARY_LIT =>
            if VLO.ch = '0' or VLO.ch = '1' then
              tval := tval*2 + to_number(VLO.ch);
            else -- Done
              VLO.tok.kind := TOK_integer;
              VLO.tok.value := tval * sign;
              VLO.valid_token := true;
              active := false;
            end if;

          when OCTAL_LIT =>
            if is_octal_digit(VLO.ch) then
              tval := tval*8 + to_number(VLO.ch);
            else -- Done
              VLO.tok.kind := TOK_integer;
              VLO.tok.value := tval * sign;
              VLO.valid_token := true;
              active := false;
            end if;


          when INTEGER_LIT =>
            tslen := tslen + 1;
            VLO.tstr(tslen) := VLO.ch;

            if char.is_digit(VLO.ch) then
              tval := tval*10 + to_number(VLO.ch);
              lit_chars := lit_chars + 1;
            elsif VLO.ch = '.' then -- Real number
              tfloat := real(tval);
              frac_digits := 0;
              lex_st := FLOAT_LIT;
              lit_chars := lit_chars + 1;
            elsif (VLO.ch = 'x' or VLO.ch = 'X') and tval = 0 then -- Hex literal
              lex_st := HEX_LIT;

            else -- Not part of a number
              
              if INTEGER_TERMINATOR(VLO.ch) then -- End of number, build an integer
                if op_char and lit_chars = 0 then -- We only saw a unary +/- previously, treat as a string
                  SU.copy(VLO.tstr, VLO.tok.data, tslen-1);
                  VLO.tok.kind := TOK_string;
                else
                  VLO.tok.kind := TOK_integer;
                  VLO.tok.value := tval * sign;
                end if;
                
                VLO.valid_token := true;
                active := false;
              else -- Non-whitespace or end quote, treat as a string and keep lexing
                lex_st := IDENTIFIER;
              end if;

            end if;

          when FLOAT_LIT =>
            if char.is_digit(VLO.ch) then
              tfloat := tfloat*10.0 + real(to_number(VLO.ch));
              frac_digits := frac_digits + 1;
            elsif VLO.ch = 'e' or VLO.ch = 'E' then
              tfloat := real(sign) * tfloat / 10.0**frac_digits;
              sign := 1;
              tval := 0;
              lex_st := FLOAT_EXPONENT;
            else
              tfloat := real(sign) * tfloat / 10.0**frac_digits;
              VLO.tok.kind := TOK_float;
              VLO.tok.float := tfloat;
              VLO.valid_token := true;
              active := false;
            end if;

          when FLOAT_EXPONENT =>
            if VLO.ch = '-' then
              sign := -1;
            elsif VLO.ch = '+' then
              sign := 1;
            elsif char.is_digit(VLO.ch) then
              tval := tval*10 + to_number(VLO.ch);
            else -- Exponent complete
              tval := tval * sign;
              tfloat := tfloat * 10.0**tval;
              VLO.tok.kind := TOK_float;
              VLO.tok.float := tfloat;
              VLO.valid_token := true;
              active := false;
            end if;

--          when HEX_LIT =>
--            if char.is_hexadecimal_digit(VLO.ch) then
--              tval := tval*16 + to_hex_number(VLO.ch);
--            else
--              VLO.tok.kind := TOK_integer;
--              VLO.tok.value := tval * sign;
--              VLO.valid_token := true;
--              active := false;
--            end if;

          when GROUP_BEGIN =>
            -- Check if this is a splat operator
            found_splat := false;
            if VLO.ch = '*' then -- Possible splat
              peek_next_char(VLO, next_ch);
              if next_ch = '}' then -- This is a splat (or a list of "*")
                get_next_char(VLO); -- Consume '*'
                get_next_char(VLO); -- Consume '}'
                found_splat := true;
              end if;
            end if;

            if not found_splat then
              VLO.tok.kind := TOK_group_begin;
              VLO.permit_comment := true;
            else
              VLO.tok.kind := TOK_splat;
            end if;
            VLO.valid_token := true;
            active := false;

          when GROUP_END =>
            VLO.tok.kind := TOK_group_end;
            VLO.valid_token := true;
            active := false;

          when SUBST_BEGIN =>
            if VLO.subst_depth >= 0 then
              VLO.subst_depth := VLO.subst_depth + 1;
            else -- Special case for handling start of substitution in a segmented string
              VLO.subst_depth := 1;
            end if;
            VLO.tok.kind := TOK_subst_begin;
            VLO.valid_token := true;
            active := false;

          when SUBST_END =>
            VLO.subst_depth := VLO.subst_depth - 1;
            VLO.tok.kind := TOK_subst_end;
            VLO.valid_token := true;
            active := false;


          when COMMENT =>
            continued_comment := true;
            while continued_comment loop
              -- Check if the current line ends with a '\'
              if VLO.cur_line'length = 0 or VLO.cur_line(VLO.cur_line'high) /= '\' then
                continued_comment := false;
              end if;

              -- Get next line from buffer if it exists
              endbuffer(VLO.buf, at_end);

              exit when at_end;

              deallocate(VLO.cur_line);
              VLO.line_num := VLO.buf.cur_line_num;
              nextline(VLO.buf, VLO.cur_line);

            end loop;

            if not at_end then
              VLO.tstr(1) := LF;
              tslen := 1;
              lex_st := EOL;
            else
              lex_st := EOB;
            end if;

          when EOL =>
            VLO.tok.kind := TOK_EOL;
            --report "@@@@@@@@@ EOL char: " & character'image(VLO.tstr(1)) & " " & integer'image(tslen);
            SU.copy(VLO.tstr, VLO.tok.data, tslen);
            --VLO.tok.data := new string'(";");
            --report ">>>>>>>>> copy done: " & character'image(VLO.tok.data(1)) & "  " & integer'image(VLO.line_num);
            VLO.permit_comment := true;
            VLO.valid_token := true;
            active := false;

          when EOB =>
            VLO.tok.kind := TOK_EOB;
            VLO.valid_token := true;
            active := false;

          when others =>
            VLO.tok.kind := TOK_UNKNOWN;
            VLO.valid_token := true;
            active := false;

        end case;

        if VLO.valid_token and VLO.tok.kind /= TOK_integer then -- Use integer field for line number
          VLO.tok.value := VLO.line_num;
        end if;

        if VLO.valid_token and VLO.tok.kind /= TOK_EOL
            and VLO.tok.kind /= TOK_group_begin then -- Stop processing '#' as a comment
          VLO.permit_comment := false;
        end if;

        if active = true then -- Still processing a token
          get_next_char(VLO);
        end if;

      end loop;
    end if;

--    to_unbounded_string(VLO.tok, timg);
--    report "%%%%%%%%%%%%%%%%%%%% NEW TOK: " & vt_token_kind'image(VLO.tok.kind) & "  " & timg.all;
--    deallocate(timg);

    tok := VLO.tok;
  end procedure;


  -- ## Mark the buffered tok object as invalid
  procedure consume_token( VLO : inout vt_lex_acc ) is
  begin
    if VLO.valid_token then
      VLO.tok.data := null;
      VLO.tok.kind := TOK_UNKNOWN;
      VLO.valid_token := false;
    end if;
  end procedure;

  -- ## Allocate a new copy of a token  
  procedure copy_token( variable source : in vt_token; variable dest : inout vt_token ) is
  begin
    dest := source;
    if source.data /= null then
      SU.copy(source.data, dest.data);
    end if;
  end procedure;


  -- ## Free the data string from a token
  -- #  Tokens themselves are never dynamically allocated so we don't free the
  -- #  token object.
  procedure free( tok : inout vt_token ) is
  begin
    SU.free(tok.data);
  end procedure;


  -- ## Free internal dynamic data for the lexer
  procedure free( VLO : inout vt_lex_acc ) is
  begin
    deallocate(VLO.cur_line);
    free(VLO.tok);
    deallocate(VLO);
  end procedure;


  -- ## Convert a token to a string representation
  procedure to_unbounded_string( variable tok : in vt_token; dest : out unbounded_string) is
    variable tstr : unbounded_string;
  begin
    case tok.kind is
      when TOK_integer => tstr := SU.to_unbounded_string(integer'image(tok.value));
      when TOK_float => tstr := SU.to_unbounded_string(real'image(tok.float));
      when TOK_string => SU.copy(tok.data, tstr);
      when TOK_EOL => SU.copy(tok.data, tstr);
      when others => tstr := SU.to_unbounded_string("$" & vt_token_kind'image(tok.kind) & "$");
    end case;

    dest := tstr;
  end procedure;


  -- ## Convert a token to a real value
  procedure to_real( variable tok : in vt_token; dest : out real ) is
  begin
    if tok.data = null then -- Integer
      dest := real(tok.value);
    else
      dest := real'value(tok.data.all);
    end if;
  end procedure;

end package body;
