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
--# vt_expr_interpreter.vhdl - Expression evaluator for the expr command
--# Freely available from http://code.google.com/p/vertcl
--#
--# Copyright © 2015 Kevin Thibedeau
--# (kevin 'period' thibedeau 'at' gmail 'punto' com)
--#
--# This source is released under the terms of the MIT license.
--# See the LICENSE file for more details.
-------------------------------------------------------------------
library extras;
use extras.strings_unbounded.all;

library vertcl;
use vertcl.vt_expr_parser.all;

package vt_expr_interpreter is

  -- Internal expression functions
  type ecommand_id is (
    ECMD_UNKNOWN,
    ECMD_abs,
    ECMD_acos,
    ECMD_asin,
    ECMD_atan,
    ECMD_atan2,
    ECMD_bool,
    ECMD_ceil,
    ECMD_cos,
    ECMD_cosh,
    ECMD_double,
    ECMD_exp,
    ECMD_floor,
    ECMD_fmod,
    ECMD_hypot,
    ECMD_int,
    ECMD_log,
    ECMD_log10,
    ECMD_max,
    ECMD_min,
    ECMD_pow,
    ECMD_rand,
    ECMD_round,
    ECMD_sin,
    ECMD_sinh,
    ECMD_sqrt,
    ECMD_srand,
    ECMD_tan,
    ECMD_tanh
  );

  subtype builtin_expr_commands is ecommand_id range ECMD_abs to ECMD_tanh;


  type ecommand_info;
  type ecommand_info_acc is access ecommand_info;
  type ecommand_info is record
    name     : unbounded_string;  -- Command name
    id       : ecommand_id;       -- Command enum id
    succ     : ecommand_info_acc; -- Next command with same hash
  end record;

  -- Hashtable of commands
  type ecommand_map is array (natural range <>) of ecommand_info_acc;


  type eresult_obj is record
    value : vt_eparse_node_acc; -- Result of expression evaluation
    is_ref : boolean; -- Indicates if value is a reference or an owned object
  end record;

  type expr_interp;
  type expr_interp_acc is access expr_interp;
  type expr_interp is record
    commands : ecommand_map(0 to 20);  -- Array of hashed command defs

    rand_seed1, rand_seed2 : positive; -- Current state of the PRNG for srand() and rand()

    result : eresult_obj;
  end record;

  -- ## Initialize an expression interpreter
  procedure expr_interp_init( EIO : inout expr_interp_acc );

  -- ## Evaluate a parsed expression
  procedure eval_expr( EIO : inout expr_interp_acc; expr : inout vt_eparse_node_acc );

  -- ## Test if value of last evaluated expression is true
  procedure expr_is_true(EIO : inout expr_interp_acc; is_true : out boolean );

  -- ## Release an expression interpreter
  procedure free( EIO : inout expr_interp_acc );

end package;


library ieee;
use ieee.math_real.all;

library vertcl;
use vertcl.vt_expr_lexer.all;

package body vt_expr_interpreter is
  alias SF is extras.strings_fixed;

-- PRIVATE procedures:
-- ===================

  -- // Validate test conditions with standard error message format
  procedure assert_true(test : boolean; msg : string; severity_lvl : severity_level;
    EIO : inout expr_interp_acc) is
  begin
    if not test then
      assert false report "EXPR INTERP: " & msg severity severity_lvl;
    end if;

  end procedure;


  -- // Define a builtin expression function
  procedure def_command( EIO : inout expr_interp_acc; name : in string; id : in ecommand_id ) is
    variable cmd : ecommand_info_acc;
    constant h : natural := SF.hash(name) mod EIO.commands'length;
  begin
    cmd := new ecommand_info;
    cmd.name := new string'(name);
    cmd.id := id;

    cmd.succ := EIO.commands(h);
    EIO.commands(h) := cmd;
  end procedure;

  -- // Lookup an expression function
  procedure get_command( EIO : inout expr_interp_acc; name : in string; cmd : inout ecommand_info_acc ) is
    variable cur_cmd : ecommand_info_acc;

    constant h : natural := SF.hash(name) mod EIO.commands'length;
  begin
    -- Lookup the command ID
    cur_cmd := EIO.commands(h);
    while cur_cmd /= null loop
      exit when cur_cmd.name.all = name;
      cur_cmd := cur_cmd.succ;
    end loop;

    cmd := cur_cmd;
  end procedure;


  -- // Set the return value
  -- /  is_ref - Return value is a reference when true, otherwise it is an object owned by the interp object
  procedure set_result( EIO : inout expr_interp_acc; variable rval : vt_eparse_node_acc := null;
    is_ref : boolean := true ) is
  begin
    report "@@@@@@@@@@@ set_result";
    if EIO.result.value /= null and not EIO.result.is_ref then
      report "DBG: set_result: freeing old value: " & integer'image(EIO.result.value.id); -- %2008 DEBUG%
      free(EIO.result.value);
    end if;

    -- We defer copying the return value when it is a reference, otherwise we take
    -- ownership of an independent value node tree.
    EIO.result.value := rval;

    if rval /= null then
      EIO.result.is_ref := is_ref;
    else
      EIO.result.is_ref := false;
    end if;

  end procedure;


  -- // Convert a node to a real
  procedure to_real(EIO : inout expr_interp_acc; node : inout vt_eparse_node_acc; value : out real ) is
  begin
    assert_true(node.kind = EN_numeric, "Expecting numeric value for command argument", failure, EIO);
    to_real(node.tok, value);
  end procedure;


  -- // Execute the max() math function
  procedure do_ecmd_max( EIO : inout expr_interp_acc; args : inout vt_eparse_node_acc ) is
    variable cur : vt_eparse_node_acc;
    variable mvi : integer := integer'low;
    variable mvf : real    := real'low;
    variable ra  : real;
    variable is_integer : boolean := true;
  begin
    cur := args;
    while cur /= null loop
      eval_expr(EIO, cur);
      if EIO.result.value.tok.kind /= ETOK_integer then
        mvf := real(mvi);
        is_integer := false;
      end if;

      if is_integer then
        if EIO.result.value.tok.int > mvi then
          mvi := EIO.result.value.tok.int;
        end if;
      else -- Floating point
        to_real(EIO, EIO.result.value, ra);
        if ra > mvf then
          mvf := ra;
        end if;
      end if;
      cur := cur.succ;
    end loop;

    if is_integer then
      EIO.result.value.tok.int := mvi;
    else
      EIO.result.value.tok.float := mvf;
      EIO.result.value.tok.kind := ETOK_float;
    end if;
  end procedure;

  -- // Execute the min() math function
  procedure do_ecmd_min( EIO : inout expr_interp_acc; args : inout vt_eparse_node_acc ) is
    variable cur : vt_eparse_node_acc;
    variable mvi : integer := integer'high;
    variable mvf : real    := real'high;
    variable ra  : real;
    variable is_integer : boolean := true;
  begin
    cur := args;
    while cur /= null loop
      eval_expr(EIO, cur);
      if EIO.result.value.tok.kind /= ETOK_integer then
        mvf := real(mvi);
        is_integer := false;
      end if;

      if is_integer then
        if EIO.result.value.tok.int < mvi then
          mvi := EIO.result.value.tok.int;
        end if;
      else -- Floating point
        to_real(EIO, EIO.result.value, ra);
        if ra < mvf then
          mvf := ra;
        end if;
      end if;
      cur := cur.succ;
    end loop;

    if is_integer then
      EIO.result.value.tok.int := mvi;
    else
      EIO.result.value.tok.float := mvf;
      EIO.result.value.tok.kind := ETOK_float;
    end if;
  end procedure;


  -- // Evaluate builtin functions
  procedure eval_func( EIO : inout expr_interp_acc; func : inout vt_eparse_node_acc ) is
    variable cmd_def : ecommand_info_acc;
    variable cmd_id  : ecommand_id := ECMD_UNKNOWN;
    variable rval, arg1, arg2 : vt_eparse_node_acc;
    variable rx, ry  : real;
  begin

    -- Lookup the command ID
    get_command(EIO, func.tok.data.all, cmd_def);
    if cmd_def /= null then
      cmd_id := cmd_def.id;
    end if;

    if cmd_id = ECMD_rand then -- rand() takes no arguments so we handle it here
      new_vt_eparse_node(rval, EN_numeric);
      uniform(EIO.rand_seed1, EIO.rand_seed2, rval.tok.float);
      rval.tok.kind := ETOK_float;
      set_result(EIO, rval, false);
      return;
    elsif cmd_id = ECMD_max then
      do_ecmd_max(EIO, func.child);
      return;
    elsif cmd_id = ECMD_min then
      do_ecmd_min(EIO, func.child);
      return;
    end if;


    assert_true(func.child /= null, "Missing arguments to function '" &
      func.tok.data.all & "'", failure, EIO);
    eval_expr(EIO, func.child);
    arg1 := EIO.result.value;

    new_vt_eparse_node(rval, EN_numeric);
    rval.tok.kind := ETOK_float;

    case cmd_id is
      when ECMD_abs    =>
        if arg1.tok.kind = ETOK_integer then
          rval.tok.int := abs arg1.tok.int;
          rval.tok.kind := ETOK_integer;
        else
          to_real(EIO, arg1, rx);
          rval.tok.float := abs rx;
        end if;
      when ECMD_acos   => to_real(EIO, arg1, rx); rval.tok.float := arccos(rx);
      when ECMD_asin   => to_real(EIO, arg1, rx); rval.tok.float := arcsin(rx);
      when ECMD_atan   => to_real(EIO, arg1, rx); rval.tok.float := arctan(rx);
      when ECMD_bool   =>
        rval.tok.kind := ETOK_integer;
        if arg1.tok.kind = ETOK_integer then
          if arg1.tok.int /= 0 then
            rval.tok.int := 1;
          else
            rval.tok.int := 0;
          end if;
        else -- Float
          if arg1.tok.float /= 0.0 then
            rval.tok.int := 1;
          else
            rval.tok.int := 0;
          end if;
          -- FIXME: handle boolean strings
        end if;
      when ECMD_ceil   => to_real(EIO, arg1, rx); rval.tok.float := ceil(rx);
      when ECMD_cos    => to_real(EIO, arg1, rx); rval.tok.float := cos(rx);
      when ECMD_cosh   => to_real(EIO, arg1, rx); rval.tok.float := cosh(rx);
      when ECMD_double => to_real(EIO, arg1, rx); rval.tok.float := rx; 
      when ECMD_exp    => to_real(EIO, arg1, rx); rval.tok.float := exp(rx);
      when ECMD_floor  => to_real(EIO, arg1, rx); rval.tok.float := floor(rx);
      when ECMD_int    =>
        assert_true(arg1.kind = EN_numeric, "Expecting numeric argumentfor int()", failure, EIO);
        if arg1.tok.kind = ETOK_float then
          rval.tok.int := integer(arg1.tok.float);
        else
          rval.tok.int := arg1.tok.int;
        end if;
        rval.tok.kind := ETOK_integer;
      when ECMD_log    => to_real(EIO, arg1, rx); rval.tok.float := log(rx);
      when ECMD_log10  => to_real(EIO, arg1, rx); rval.tok.float := log10(rx);
      when ECMD_round  => to_real(EIO, arg1, rx); rval.tok.float := round(rx);
      when ECMD_sin    => to_real(EIO, arg1, rx); rval.tok.float := sin(rx);
      when ECMD_sinh   => to_real(EIO, arg1, rx); rval.tok.float := sinh(rx);
      when ECMD_sqrt   => to_real(EIO, arg1, rx); rval.tok.float := sqrt(rx);
      when ECMD_srand  =>
        assert_true(arg1.tok.kind = ETOK_integer, "'srand' Expecting integer seed", failure, EIO);
        EIO.rand_seed1 := abs arg1.tok.int;
        EIO.rand_seed2 := 12345;
        uniform(EIO.rand_seed1, EIO.rand_seed2, rval.tok.float);
      when ECMD_tan    => to_real(EIO, arg1, rx); rval.tok.float := tan(rx);
      when ECMD_tanh   => to_real(EIO, arg1, rx); rval.tok.float := tanh(rx);

      when others => -- 2 argument commands
        assert_true(func.child.succ /= null, "Missing second argument to function '" &
          func.tok.data.all & "'", failure, EIO);
        to_real(EIO, arg1, rx);

        eval_expr(EIO, func.child.succ);
        arg2 := EIO.result.value;
        to_real(EIO, arg2, ry);

        case cmd_id is
          when ECMD_atan2  => rval.tok.float := arctan(rx, ry);
          when ECMD_fmod   => rval.tok.float := rx mod ry;
          when ECMD_hypot  =>
            -- math_real doesn't provide a hypot() function. We'll just fake it
            -- and suffer the loss of precision.
            rval.tok.float := sqrt(rx*rx + ry*ry);
            
          when ECMD_pow    => rval.tok.float := rx ** ry;
          when others =>
            assert_true(false, "Invalid expr function '" & func.tok.data.all & "'", failure, EIO);
        end case;
    end case;

    set_result(EIO, rval, false);
  end procedure;



-- PUBLIC procedures:
-- ==================

  -- ## Evaluate a parsed expression
  procedure eval_expr( EIO : inout expr_interp_acc; expr : inout vt_eparse_node_acc ) is
    variable lterm, rterm, rval : vt_eparse_node_acc;
    variable lreal, rreal : real;
    variable sign : integer;
  begin
    assert_true(expr /= null, "Invalid operator; null node", failure, EIO);

    if is_unary_operator(expr.tok.kind) and expr.child.succ = null then -- Unary operators
        eval_expr(EIO, expr.child);
        lterm := EIO.result.value;
        assert_true(lterm.kind = EN_numeric, "Expecting numeric operand to unary operator",
          failure, EIO);

        if expr.tok.kind = ETOK_minus then
          sign := -1;
        else
          sign := 1;
        end if;
        
        if lterm.tok.kind = ETOK_float then
          to_real(lterm.tok, lreal);
          lterm.tok.float := real(sign) * lreal;
        else -- Integer
          lterm.tok.int := sign * lterm.tok.int;
        end if;

        -- lterm is already the return value
        --EIO.result.value := lterm;
        --set_result(EIO, lterm);

    elsif is_operator(expr.tok.kind) then -- Binary operators
      eval_expr(EIO, expr.child);
      --lterm := EIO.result.value;
      copy_parse_tree(EIO.result.value, lterm);
      eval_expr(EIO, expr.child.succ);
      --rterm := EIO.result.value;
      copy_parse_tree(EIO.result.value, rterm);

      assert_true(lterm.kind = EN_numeric and rterm.kind = EN_numeric,
        "Expecting numeric operands to binary operator", failure, EIO);

      new_vt_eparse_node(rval, EN_numeric);

      -- FIXME: handle strings in expressions

      if lterm.tok.kind = ETOK_float or rterm.tok.kind = ETOK_float then -- At least one float
        to_real(lterm.tok, lreal);
        to_real(rterm.tok, rreal);

        rval.tok.kind := ETOK_float;

        case expr.tok.kind is
          when ETOK_eq =>
            rval.tok.int := boolean'pos(lreal = rreal);
            rval.tok.kind := ETOK_integer;
          when ETOK_ne =>
            rval.tok.int := boolean'pos(lreal /= rreal);
            rval.tok.kind := ETOK_integer;

          when ETOK_lt =>
            rval.tok.int := boolean'pos(lreal < rreal);
            rval.tok.kind := ETOK_integer;

          when ETOK_gt =>
            rval.tok.int := boolean'pos(lreal > rreal);
            rval.tok.kind := ETOK_integer;
          when ETOK_le =>
            rval.tok.int := boolean'pos(lreal <= rreal);
            rval.tok.kind := ETOK_integer;
          when ETOK_ge =>
            rval.tok.int := boolean'pos(lreal >= rreal);
            rval.tok.kind := ETOK_integer;

          when ETOK_plus =>
            rval.tok.float := lreal + rreal;
          when ETOK_minus =>
            rval.tok.float := lreal - rreal;
          when ETOK_mul =>
            rval.tok.float := lreal * rreal;
          when ETOK_div =>
            rval.tok.float := lreal / rreal;
          when ETOK_pow =>
            rval.tok.float := lreal ** rreal;
          when others =>
            --free(EIO.result.value);
            --EIO.result.value := null;
            free(rval); rval := null;
        end case;

        --report "#### Terms: " & real'image(lreal) & " <op> " & real'image(rreal) & " = " & real'image(result);

        --rval.tok.data := new string'(real'image(result));
--        rval.tok.float := result;

      else -- Both are integers
        rval.tok.kind := ETOK_integer;

        case expr.tok.kind is
          when ETOK_eq =>
            rval.tok.int := boolean'pos(lterm.tok.int = rterm.tok.int);
          when ETOK_ne =>
            rval.tok.int := boolean'pos(lterm.tok.int /= rterm.tok.int);

          when ETOK_lt =>
            rval.tok.int := boolean'pos(lterm.tok.int < rterm.tok.int);

          when ETOK_gt =>
            rval.tok.int := boolean'pos(lterm.tok.int > rterm.tok.int);

          when ETOK_le =>
            rval.tok.int := boolean'pos(lterm.tok.int <= rterm.tok.int);

          when ETOK_ge =>
            rval.tok.int := boolean'pos(lterm.tok.int >= rterm.tok.int);

          when ETOK_plus =>
            rval.tok.int := lterm.tok.int + rterm.tok.int;
          when ETOK_minus =>
            rval.tok.int := lterm.tok.int - rterm.tok.int;
          when ETOK_mul =>
            rval.tok.int := lterm.tok.int * rterm.tok.int;
          when ETOK_div =>
            rval.tok.int := lterm.tok.int / rterm.tok.int;
          when ETOK_pow =>
            rval.tok.int := lterm.tok.int ** rterm.tok.int;
          when others =>
            free(EIO.result.value);
            --EIO.result.value := null;
            free(rval); rval := null;
        end case;
      end if;
      free(lterm); free(rterm);
      set_result(EIO, rval, false);

    else -- Others
      case expr.kind is
        when EN_identifier =>
          eval_func(EIO, expr);

        when EN_numeric =>
          --copy_parse_tree(expr, EIO.result.value);
          copy_parse_tree(expr, rval);
          set_result(EIO, rval, false);

        when others =>
          --EIO.result.value := null;
          set_result(EIO);
      end case;
    end if;

  end procedure;



  -- ## Initialize an expression interpreter
  procedure expr_interp_init( EIO : inout expr_interp_acc ) is
  begin
    EIO := new expr_interp;
    EIO.result.value := null;
    EIO.result.is_ref := false;

    -- Build table of internal commands
    for id in builtin_expr_commands loop
      -- Strip "ECMD_" prefix from id image for use as the Tcl-visible command name
      def_command(EIO, SF.delete(ecommand_id'image(id),1,5), id);
    end loop;

  end procedure;

  -- ## Test if value of last evaluated expression is true
  procedure expr_is_true(EIO : inout expr_interp_acc; is_true : out boolean ) is
  begin
    if EIO.result.value = null then
      is_true := false;
      return;

    else
      case EIO.result.value.tok.kind is
        when ETOK_integer =>
          is_true := EIO.result.value.tok.int /= 0;

        when ETOK_float =>
          is_true := EIO.result.value.tok.float /= 0.0;

        when others =>
          is_true := false;

      end case;
    end if;
  end procedure;

  -- ## Release an expression interpreter
  procedure free( EIO : inout expr_interp_acc ) is
    variable cur_cmd, succ_cmd : ecommand_info_acc;
  begin

    if EIO.result.value /= null and not EIO.result.is_ref then
      report ">>>>>>>>>>>>>>>>>>>>> DBG: free expr interp: return val id: " & integer'image(EIO.result.value.id); -- %2008 DEBUG%

      free(EIO.result.value);
      EIO.result.value := null;
    end if;

    -- Release the command list
    for i in EIO.commands'range loop
      cur_cmd := EIO.commands(i);
      while cur_cmd /= null loop
        succ_cmd := cur_cmd.succ;
        free(cur_cmd.name);
        cur_cmd := succ_cmd;
      end loop;
    end loop;

    deallocate(EIO);
  end procedure;

end package body;
