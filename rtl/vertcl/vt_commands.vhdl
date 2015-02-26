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
--# vt_commands.vhdl - Builtin commands
--# Freely available from http://code.google.com/p/vertcl
--#
--# Copyright © 2015 Kevin Thibedeau
--# (kevin 'period' thibedeau 'at' gmail 'punto' com)
--#
--# This source is released under the terms of the MIT license.
--# See the LICENSE file for more details.
-------------------------------------------------------------------

--  Commands:
--  ---------
--  ??after
--  append      DONE
--  ??array
--  ??binary
--  break       DONE
--  concat      DONE
--  continue    DONE
--  error
--  eval        DONE
--  exit        DONE
--  expr        partial
--  for         DONE
--  foreach     DONE
--  ??format
--  global      DONE
--  if          DONE
--  incr        DONE
--  ??info
--  join        DONE
--  lappend     DONE
--  lassign
--  lindex      DONE
--  linsert     DONE
--  list        DONE
--  llength     partial
--  ??lmap
--  lrange      DONE
--  lrepeat     DONE
--  lreplace    DONE
--  lreverse    DONE
--  ??lsearch
--  lset        DONE
--  ??lsort
--  ??package
--  ??parray
--  proc        DONE
--  puts        DONE
--  rename      DONE
--  return      partial
--  ??scan
--  set         DONE
--  source
--  split       DONE
--  string:     DONE
--    cat       DONE
--    compare   DONE
--    equal     DONE
--    first     DONE
--    index     DONE
--    last      DONE
--    length    DONE
--    map       DONE
--    range     DONE
--    repeat    DONE
--    replace   DONE
--    reverse   DONE
--    tolower   DONE
--    totitle   DONE
--    toupper   DONE
--    trim      DONE
--    trimleft  DONE
--    trimright DONE
--  subst       partial
--  switch
--  ??tailcall
--  unknown     DONE
--  unset       partial
--  uplevel     DONE
--  upvar       DONE
--  while       DONE
--  yield       DONE


library vertcl;
use vertcl.vt_parser.all;
use vertcl.vt_interpreter_core.all;

package vt_commands is
  procedure do_cmd_append( VIO : inout vt_interp_acc; args : inout vt_parse_node_acc);
  procedure do_cmd_break( VIO : inout vt_interp_acc; args : inout vt_parse_node_acc);
  procedure do_cmd_concat( VIO : inout vt_interp_acc; args : inout vt_parse_node_acc);
  procedure do_cmd_continue( VIO : inout vt_interp_acc; args : inout vt_parse_node_acc);
  procedure do_cmd_eval( VIO : inout vt_interp_acc; args : inout vt_parse_node_acc);
  procedure do_cmd_exit( VIO : inout vt_interp_acc; args : inout vt_parse_node_acc);
  procedure do_cmd_expr( VIO : inout vt_interp_acc; args : inout vt_parse_node_acc);
  procedure do_cmd_for( VIO : inout vt_interp_acc; args : inout vt_parse_node_acc );
  procedure do_cmd_foreach( VIO : inout vt_interp_acc; args : inout vt_parse_node_acc );
  procedure do_cmd_global( VIO : inout vt_interp_acc; args : inout vt_parse_node_acc );
  procedure do_cmd_if( VIO : inout vt_interp_acc; args : inout vt_parse_node_acc);
  procedure do_cmd_incr( VIO : inout vt_interp_acc; args : inout vt_parse_node_acc);
  procedure do_cmd_join( VIO : inout vt_interp_acc; args : inout vt_parse_node_acc);
  procedure do_cmd_lappend( VIO : inout vt_interp_acc; args : inout vt_parse_node_acc );
  procedure do_cmd_lindex( VIO : inout vt_interp_acc; args : inout vt_parse_node_acc );
  procedure do_cmd_linsert( VIO : inout vt_interp_acc; args : inout vt_parse_node_acc );
  procedure do_cmd_list( VIO : inout vt_interp_acc; args : inout vt_parse_node_acc );
  procedure do_cmd_llength( VIO : inout vt_interp_acc; args : inout vt_parse_node_acc );
  procedure do_cmd_lrange( VIO : inout vt_interp_acc; args : inout vt_parse_node_acc );
  procedure do_cmd_lrepeat( VIO : inout vt_interp_acc; args : inout vt_parse_node_acc );
  procedure do_cmd_lreplace( VIO : inout vt_interp_acc; args : inout vt_parse_node_acc );
  procedure do_cmd_lreverse( VIO : inout vt_interp_acc; args : inout vt_parse_node_acc );
  procedure do_cmd_lset( VIO : inout vt_interp_acc; args : inout vt_parse_node_acc );
  procedure do_cmd_proc( VIO : inout vt_interp_acc; args : inout vt_parse_node_acc );
  procedure do_cmd_puts( VIO : inout vt_interp_acc; args : inout vt_parse_node_acc );
  procedure do_cmd_rename( VIO : inout vt_interp_acc; args : inout vt_parse_node_acc );
  procedure do_cmd_return( VIO : inout vt_interp_acc; args : inout vt_parse_node_acc );
  procedure do_cmd_set(VIO : inout vt_interp_acc; args : inout vt_parse_node_acc);
  procedure do_cmd_split(VIO : inout vt_interp_acc; args : inout vt_parse_node_acc);
  procedure do_cmd_string(VIO : inout vt_interp_acc; args : inout vt_parse_node_acc);
  procedure do_cmd_subst(VIO : inout vt_interp_acc; args : inout vt_parse_node_acc);
  procedure do_cmd_unknown( VIO : inout vt_interp_acc; cmd : inout vt_parse_node_acc );
  procedure do_cmd_unset( VIO : inout vt_interp_acc; args : inout vt_parse_node_acc );
  procedure do_cmd_uplevel( VIO : inout vt_interp_acc; args : inout vt_parse_node_acc );
  procedure do_cmd_upvar( VIO : inout vt_interp_acc; args : inout vt_parse_node_acc );
  procedure do_cmd_wait( VIO : inout vt_interp_acc; args : inout vt_parse_node_acc );
  procedure do_cmd_while( VIO : inout vt_interp_acc; args : inout vt_parse_node_acc );
  procedure do_cmd_yield( VIO : inout vt_interp_acc; args : inout vt_parse_node_acc );
end package;



library extras;
use extras.strings_unbounded.unbounded_string;
use extras.strings;
use extras.strings_maps_constants.all;

library vertcl;
use vertcl.vt_lexer.all;
use vertcl.vt_parser.all;
use vertcl.vt_interpreter_core.all;

use vertcl.vt_expr_lexer.all;
use vertcl.vt_expr_parser.all;
use vertcl.vt_expr_interpreter.all;


package body vt_commands is

  -- FIXME: Should there be a contains_separator() that ignores leading and trailing WS?
  procedure contains_whitespace(str : inout unbounded_string; has_ws : out boolean) is
  begin
    has_ws := false;
    for i in str.all'range loop
      if str(i) = ' ' or str(i) = HT then
        has_ws := true;
        exit;
      end if;
    end loop;
  end procedure;

  procedure contains_non_whitespace(str : inout unbounded_string; has_nws : out boolean) is
  begin
    has_nws := false;
    for i in str.all'range loop
      if str(i) /= ' ' and str(i) /= HT then
        has_nws := true;
        exit;
      end if;
    end loop;
  end procedure;

  -- FIXME: move to core
--  procedure stringify( node : inout vt_parse_node_acc ) is
--    variable img : unbounded_string;
--  begin
--    if node.tok.kind /= TOK_string then
--      if node.kind = VN_group then
--        to_unbounded_string(node.child, img);
--      else -- Numeric value
--        to_unbounded_string(node, img, false);
--      end if;

--      node.tok.data := img;
--      node.tok.kind := TOK_string;
--      node.kind := VN_word;
--      free(node.child);
--      node.child := null;
--    end if;
--  end procedure;

-- ////////////////////////////

  procedure do_cmd_append( VIO : inout vt_interp_acc; args : inout vt_parse_node_acc) is
    variable var : scope_var_acc;
    variable vobj, cur_arg : vt_parse_node_acc;
  begin
    assert_true(args /= null, "'append' Missing varName argument", failure, VIO);

    get_variable(VIO, args.tok.data.all, var);

    if var = null then -- Unknown variable, create an empty string
      new_vt_parse_node(vobj, VN_word);
      vobj.tok.kind := TOK_string;
      SU.initialize(vobj.tok.data);
      set_variable(VIO, args.tok.data.all, vobj, false); -- Pass ownership to the var
      get_variable(VIO, args.tok.data.all, var);
    end if;

    vobj := var.var.value;

    -- Convert non-strings before appending
    stringify(vobj);

    cur_arg := args.succ;
    while cur_arg /= null loop
      stringify(cur_arg);
      SU.append(vobj.tok.data, cur_arg.tok.data);
      cur_arg := cur_arg.succ;
    end loop;

    set_result(VIO, vobj);
  end procedure;


  procedure do_cmd_break( VIO : inout vt_interp_acc; args : inout vt_parse_node_acc) is
  begin
    -- Unwind the script stack until we find one with active looping or reach the bottom
    while VIO.scope.script.script_state /= MODE_LOOP and VIO.scope.script.prev /= null loop
      pop_script(VIO);
    end loop;

    if VIO.scope.script.script_state = MODE_LOOP then
      VIO.scope.script.script_state := MODE_NORMAL;
    else
      report "Invoked 'break' outside of a loop";
    end if;
  end procedure;


  procedure concat_args(args : inout vt_parse_node_acc; force_group : boolean := false) is
    variable lobj, ltail, cur, prev, node_child : vt_parse_node_acc;
    variable was_true : boolean;
    variable img : unbounded_string;
  begin
    if args.kind /= VN_group and (force_group or args.tok.kind /= TOK_string) then -- Convert first arg to a group
      -- FIXME: properly groupify strings with whitespace
      new_vt_parse_node(lobj, args.kind);
      lobj.tok := args.tok;
      lobj.child := args.child;
      args.tok.kind := TOK_group_begin;
      args.tok.data := null;
      args.kind := VN_group;
      args.child := lobj;
    end if;

    -- FIXME: trim leading and trailing whitespace from strings

    if args.kind = VN_group then -- Concatenate to a group object
      lobj := args;

      get_last(lobj.child, ltail);

      -- Make remaining args siblings following the list tail
      if ltail /= null then
        ltail.succ := args.succ;
      else -- First group was empty
        lobj.child := args.succ;
      end if;
      args.succ := null;
      
      -- Any list elements that are empty or whitespace strings need to be eliminated
      if ltail /= null then
        cur := ltail.succ;
        prev := ltail;
      else -- First arg was an empty list
        cur := lobj.child;
        prev := null;
      end if;

      while cur /= null loop
        if cur.tok.kind = TOK_string then
          contains_non_whitespace(cur.tok.data, was_true);
          if not was_true then -- Empty or all-whitespace; eliminate it
            if prev /= null then
              prev.succ := cur.succ;
              cur.succ := null;
              free(cur);
              cur := prev;
            else
              lobj.child := cur.succ;
              cur.succ := null;
              free(cur);
              cur := lobj.child;
            end if;
          end if;
        end if;
        prev := cur;
        cur := cur.succ;
      end loop;
      
      -- Strings need to be groupified if they have whitespace
      if ltail /= null then
        cur := ltail.succ;
      else
        cur := lobj.child;
      end if;
      
      while cur /= null loop
        if cur.tok.kind = TOK_string then
          contains_whitespace(cur.tok.data, was_true);
          if was_true then
            groupify(cur);
          end if;
        end if;
        cur := cur.succ;
      end loop;

      -- Any args after the first that are groups need to have their top level group removed
      if ltail /= null then
        cur := ltail.succ;
        prev := ltail;
      else -- First arg was an empty list
        cur := lobj.child;
        prev := null;
      end if;
      
      while cur /= null loop
        if cur.kind = VN_group then
          if cur.child /= null then -- Transfer first node to group node
            cur.kind := cur.child.kind;
            cur.tok := cur.child.tok;
            
            -- Find end of group list
            get_last(cur.child, ltail);

            if ltail /= cur.child then -- More than one child
              ltail.succ := cur.succ;
              cur.succ := cur.child.succ;

              cur.child.succ := null;
              node_child := cur.child.child;
              cur.child.child := null;
              cur.child.tok.data := null;
              free(cur.child);
              cur.child := node_child;

              cur := ltail;
            else -- One child
              node_child := cur.child.child;
              cur.child.child := null;
              cur.child.tok.data := null;
              free(cur.child);
              cur.child := node_child;
            end if;

          else -- Empty group; eliminate it
            if prev /= null then
              prev.succ := cur.succ;
              cur.succ := null;
              free(cur);
              cur := prev;
            else
              lobj.child := cur.succ;
              cur.succ := null;
              free(cur);
              cur := lobj.child;
            end if;
          end if;
        end if;
        prev := cur;
        cur := cur.succ;
      end loop;

    elsif args.tok.kind = TOK_string then -- Concatenate to a string
      lobj := args;

      SU.trim(lobj.tok.data, strings.both);

      if lobj.succ /= null then
        cur := lobj.succ;
        while cur /= null loop
          if cur.kind = VN_group then
            to_unbounded_string(cur.child, img);
            if cur.child /= null then
              SU.append(lobj.tok.data, " ");
            end if;
          else
            to_unbounded_string(cur, img, false);
            SU.append(lobj.tok.data, " ");
          end if;

          SU.trim(img, strings.both);
          SU.append(lobj.tok.data, img);
          deallocate(img);
          cur := cur.succ;
        end loop;
        
        free(lobj.succ);
        lobj.succ := null;
      end if;

    end if;

  end procedure;


  procedure do_cmd_concat( VIO : inout vt_interp_acc; args : inout vt_parse_node_acc) is
  begin
    if args = null then -- Return empty string
      set_result(VIO);
    else
      concat_args(args);

      -- Disconnect the modified args list from its parent command so we can use
      -- it as the return value without copying
      VIO.scope.script.cur_cmd.child.succ := null;
      set_result(VIO, args, false);
    end if;
  end procedure;


  procedure do_cmd_continue( VIO : inout vt_interp_acc; args : inout vt_parse_node_acc) is
  begin
    -- Unwind the script stack until we find one just inside a loop
    while VIO.scope.script.prev /= null and VIO.scope.script.prev.script_state /= MODE_LOOP loop
      pop_script(VIO);
    end loop;

    if VIO.scope.script.prev.script_state = MODE_LOOP then
      -- Skip remaining commands in this loop to effect the continue
      VIO.scope.script.cur_cmd := null;
    else
      report "Invoked 'continue' outside of a loop";
    end if;

  end procedure;


-- FIXME: merge with stringify() ?
  procedure stringify_expr(args : inout vt_parse_node_acc; estr : inout unbounded_string;
    follow_siblings : in boolean ) is
    variable cur : vt_parse_node_acc;
    variable tstr : unbounded_string;
  begin
    SU.initialize(estr);
    cur := args;
    while cur /= null loop
      if cur.kind = VN_group then -- Remove enclosing braces at the top level
        to_unbounded_string(cur.child, tstr);
      else
        to_unbounded_string(cur, tstr, false);
      end if;
      SU.append(estr, tstr);

      deallocate(tstr);

      if not follow_siblings then exit; end if;

      if cur.succ /= null then
        SU.append(estr, ' ');
      end if;

      cur := cur.succ;

      --report "######### stringify: >" & estr.all & "<";
    end loop;
  end procedure;

  procedure eval_expr( VIO : inout vt_interp_acc; args : inout vt_parse_node_acc;
    follow_siblings : boolean := false) is
    variable parse_tree : vt_eparse_node_acc;
    variable estr : unbounded_string;
  begin
    assert_true(args /= null, "Empty expression", failure, VIO);

    -- Combine all args into a single string

    stringify_expr(args, estr, follow_siblings);
    report "### EXPR: >" & estr.all & "<";

    -- Perform second round of substitutions
    substitute_in_string(VIO, estr);

    -- Parse and interpret expression
    parse_expression(estr, parse_tree);
    --write_parse_tree("expr_tree.txt", parse_tree); -- FIXME: remove
    deallocate(estr);

		eval_expr(VIO.EI, parse_tree);
    free(parse_tree);
  end procedure;

  
  procedure do_cmd_eval( VIO : inout vt_interp_acc; args : inout vt_parse_node_acc) is
    variable cmd_list : vt_parse_node_acc;
  begin
    assert_true(args /= null, "'eval' Missing arguments", failure, VIO);

    copy_parse_tree(args, cmd_list);
    write_parse_tree("eval_tree1.txt", cmd_list); -- FIXME: remove
    concat_args(cmd_list, true);
    write_parse_tree("eval_tree3.txt", cmd_list);
    group_to_script(VIO, cmd_list);
--    convert_to_commands(cmd_list);
--    assert_true(cmd_list /= null and cmd_list.kind = VN_cmd_list,
--      "Expecting command list in 'eval'", failure, VIO);
    write_parse_tree("eval_tree2.txt", cmd_list);
    push_script(VIO.scope, MODE_NORMAL, cmd_list);
  end procedure;


  procedure do_cmd_exit( VIO : inout vt_interp_acc; args : inout vt_parse_node_acc) is
  begin
    if args /= null then
      expect_arg_count(args, 1, "exit", "?returnCode?", VIO);

      if args.tok.kind = TOK_integer then
        VIO.exit_code := args.tok.value;
      else
        VIO.exit_code := -1;
      end if;
    end if;

    -- Unwind the complete scope and script stacks
    while VIO.scope /= VIO.scope_stack loop
      pop_scope(VIO);
    end loop;
    while VIO.scope.script /= VIO.scope.script_stack loop
      pop_script(VIO);
    end loop;

    VIO.scope.script.cur_cmd := null;
  end procedure;


  procedure do_cmd_expr( VIO : inout vt_interp_acc; args : inout vt_parse_node_acc) is
    variable rval : vt_parse_node_acc;
  begin

    eval_expr(VIO, args, true);

    -- Set result
    new_vt_parse_node(rval, VN_word);

    case VIO.EI.result.value.tok.kind is
      when ETOK_integer =>
        rval.tok.kind := TOK_integer;
        rval.tok.value := VIO.EI.result.value.tok.int;

      when ETOK_float =>
        rval.tok.kind := TOK_float;
        rval.tok.float := VIO.EI.result.value.tok.float;

      when others =>
        rval.tok.kind := TOK_UNKNOWN;
        assert_true(false, "Unknown return value from expr", error, VIO);

    end case;

    set_result(VIO, rval, false);
  end procedure;



  constant FOR_STATE_START : integer := -100100;
  constant FOR_STATE_NEXT  : integer := -100200;
  constant FOR_STATE_BODY  : integer := -100300;

  procedure do_cmd_for( VIO : inout vt_interp_acc; args : inout vt_parse_node_acc ) is
    variable count : natural;
    variable is_true : boolean;
    variable a_start, a_test, a_next, a_body, next_body, cmd_list : vt_parse_node_acc;
  begin

    expect_arg_count(args, 4, "for", "start test next body", VIO);

    -- We need to store state information in the for command code body.
    -- The token value field of the VN_group node will be set to predefined constants
    -- This will determine whether we need to execute the start, next, or body blocks
    -- at the start of each loop iteration.

    a_start := args;
    a_test := a_start.succ;
    a_next := a_test.succ;
    a_body := a_next.succ;

    case a_body.tok.value is
      when FOR_STATE_START =>
        a_body.tok.value := FOR_STATE_BODY;
        next_body := a_body;

      when FOR_STATE_NEXT =>
        a_body.tok.value := FOR_STATE_BODY;
        next_body := a_body;

      when FOR_STATE_BODY =>
        a_body.tok.value := FOR_STATE_NEXT;
        next_body := a_next;

      when others => -- Not initialized
        a_body.tok.value := FOR_STATE_START;
        next_body := a_start;
    end case;

    is_true := true;
    if a_body.tok.value /= FOR_STATE_START then -- Evaluate test expression
      eval_expr(VIO, a_test);
      expr_is_true(VIO.EI, is_true);
    end if;

    if is_true then -- Execute next code body
      assert_true(next_body.kind = VN_group, "Expecting group for code body", failure, VIO);
      copy_parse_tree(next_body, cmd_list);
      group_to_script(VIO, cmd_list);
--      convert_to_commands(cmd_list);
--      assert_true(cmd_list /= null and cmd_list.kind = VN_cmd_list,
--        "Expecting command list in 'for'", failure, VIO);
      push_script(VIO.scope, MODE_LOOP, cmd_list);

    else -- Stop looping
      set_result(VIO);
      VIO.scope.script.script_state := MODE_NORMAL;
      a_body.tok.value := 0;
    end if;

  end procedure;



  constant FOREACH_IX_OFFSET : integer := -100000;

  procedure do_cmd_foreach( VIO : inout vt_interp_acc; args : inout vt_parse_node_acc ) is
    variable lcount : natural;
    variable a_var, a_list, a_body, next_val, cmd_list : vt_parse_node_acc;
    variable i, ix, len, llength : integer;
  begin
    assert_true(args /= null and args.tok.kind = TOK_string, "'foreach' expecting variable name", failure, VIO);

    -- Determine number of lists    
    arg_count(args.succ, lcount);
    assert_true(lcount/2 >= 1 and (lcount/2)*2 = lcount, "'foreach' missing varname list", failure, VIO);
    lcount := lcount / 2;
    
    -- Get the length of the longest list
    i := 0;
    llength := 0;
    a_list := args.succ;
    loop
      assert_true(a_list.kind = VN_group, "'foreach' expecting list", failure, VIO);
      arg_count(a_list.child, len);
      if len > llength then
        llength := len;
      end if;
      i := i + 1;
      exit when i = lcount;
      a_list := a_list.succ.succ;
    end loop;
    
    a_body := a_list.succ;

    -- Determine iteration index    
    if a_body.tok.value < 0 then -- Resuming from previous iteration
      ix := a_body.tok.value - FOREACH_IX_OFFSET + 1;
    else -- First iteration
      ix := 0;
    end if;

    if ix = llength then -- Stop looping
      set_result(VIO);
      VIO.scope.script.script_state := MODE_NORMAL;
      a_body.tok.value := 0;
    else -- Execute next code body
      --report "@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ FOREACH: " & integer'image(ix);

      -- Set all of the list variables
      i := 0;
      a_var := args;
      loop
        a_list := a_var.succ;
        get_element(a_list.child, ix, next_val);
        if next_val /= null then
          set_variable(VIO, a_var.tok.data.all, next_val, true, false);
        else -- Set to empty string
          new_vt_parse_node(next_val, VN_word);
          next_val.tok.kind := TOK_string;
          SU.initialize(next_val.tok.data);
          set_variable(VIO, a_var.tok.data.all, next_val, false);
        end if;
        
        i := i + 1;
        exit when i = lcount;
        a_var := a_list.succ;
      end loop;
      
      assert_true(a_body.kind = VN_group, "'foreach' Expecting group for code body", failure, VIO);
      copy_parse_tree(a_body, cmd_list);
      group_to_script(VIO, cmd_list);
--      convert_to_commands(cmd_list);
--      assert_true(cmd_list /= null and cmd_list.kind = VN_cmd_list,
--        "Expecting command list in 'foreach'", failure, VIO);
      push_script(VIO.scope, MODE_LOOP, cmd_list);
      
      a_body.tok.value := ix + FOREACH_IX_OFFSET;
    end if;
  end procedure;



  procedure do_cmd_global( VIO : inout vt_interp_acc; args : inout vt_parse_node_acc ) is
    variable cur, gobj : vt_parse_node_acc;
    variable var : scope_var_acc;
  begin
    assert_true(args /= null, "'global' Missing arguments", failure, VIO);
    set_result(VIO);
    if VIO.scope = VIO.scope_stack then -- Already in global scope; Do nothing
      return;
    end if;

    -- Link variables to global scope
    cur := args;
    while cur /= null loop
      -- Make sure variable doesn't already exist in this scope
      get_variable(VIO.scope, cur.tok.data.all, var);
      assert_true(var = null,
        "'global' Variable already exists: " & cur.tok.data.all, failure, VIO);

      -- Check if it exists at the global scope      
      get_variable(VIO.scope_stack, cur.tok.data.all, var);
      if var = null then -- Create new global variable
        new_vt_parse_node(gobj, VN_word);
        SU.initialize(gobj.tok.data);
        gobj.tok.kind := TOK_string;
        set_variable(VIO.scope_stack, cur.tok.data.all, gobj, false); -- Pass ownership to the var
        get_variable(VIO.scope_stack, cur.tok.data.all, var);
      end if;

      -- Create a linked variable in the current scope
      link_variable(VIO, cur.tok.data.all, var);

      cur := cur.succ;
    end loop;
  end procedure;


  procedure do_cmd_if( VIO : inout vt_interp_acc; args : inout vt_parse_node_acc) is
    variable enode, cblock, cmd_list : vt_parse_node_acc;
    variable is_true : boolean;
  begin
    enode := args;

    --report "                   $$$$$$$$$$$$$$$$$$$$ STARTING IF";

    loop
      assert_true(enode /= null, "Expecting expression in 'if' command", failure, VIO);
      eval_expr(VIO, enode);
      expr_is_true(VIO.EI, is_true);

      cblock := enode.succ;
      assert_true(cblock /= null, "Missing true code body", failure, VIO);

      -- Optional "then"
      if cblock.tok.kind = TOK_string and cblock.tok.data.all = "then" then
        cblock := cblock.succ;
        assert_true(cblock /= null, "Missing true code body", failure, VIO);
      end if;


      if is_true then -- Evaluate true path
        --report "IS TRUE";

        assert_true(cblock.kind = VN_group, "Expecting group for code body", failure, VIO);
        copy_parse_tree(cblock, cmd_list);
        group_to_script(VIO, cmd_list);
--        convert_to_commands(cmd_list);
--        assert_true(cmd_list /= null and cmd_list.kind = VN_cmd_list,
--          "Expecting command list in 'if'", failure, VIO);
        push_script(VIO.scope, MODE_NORMAL, cmd_list);
        return;
      else -- Evaluate false path
        if cblock.succ = null then -- No false block
          set_result(VIO);
          return;
        end if;

        cblock := cblock.succ; -- Node after true block

        --enode := enode.succ.succ; -- Node after true block
        if cblock.tok.kind = TOK_string and cblock.tok.data.all = "elseif" then
          --report "ELSEIF";
          enode := cblock.succ;
          next;
        else -- This should be the final false block
          --report "IS FALSE";

          -- Optional "else"
          if cblock.tok.kind = TOK_string and cblock.tok.data.all = "else" then
            cblock := cblock.succ;
            assert_true(cblock /= null, "Missing false code body", failure, VIO);
          end if;

          copy_parse_tree(cblock, cmd_list);
          group_to_script(VIO, cmd_list);
          push_script(VIO.scope, MODE_NORMAL, cmd_list);
          return;
        end if;

      end if;
    end loop;
  end procedure;


  procedure do_cmd_incr( VIO : inout vt_interp_acc; args : inout vt_parse_node_acc) is

    variable var : scope_var_acc;
    variable inc : integer := 1;
    variable rval : vt_parse_node_acc;
  begin
    -- FIXME: no failure with : incr y -10 when y is a string
    -- First arg is the variable name to increment
    assert_true(args.tok.kind = TOK_string,
      "'incr' Expecting string as variable name", failure, VIO);

    -- Second arg is an optional increment
    if args.succ /= null then
      assert_true(args.succ.tok.kind = TOK_integer,
        "'incr' Second argument must be an integer", failure, VIO);

      inc := args.succ.tok.value;
    end if;

    get_variable(VIO, args.tok.data.all, var);
    var.var.value.tok.value := var.var.value.tok.value + inc;

    set_result(VIO, var.var.value);

    --report "### incr return: " & integer'image(VIO.result.value.tok.value);
  end procedure;

-- FIXME: Bad result with [join "x {y z}" ,] should be x,y z (or x,{y z})
  procedure do_cmd_join( VIO : inout vt_interp_acc; args : inout vt_parse_node_acc ) is
    variable jstr, rval, elem : unbounded_string;
    variable cur, list : vt_parse_node_acc;
  begin
    assert_true(args /= null, "'join' Missing list", failure, VIO);
    list := args;
    if list.succ /= null then
      to_unbounded_string(list.succ, jstr, false);
    else
      SU.copy(" ", jstr);
    end if;

    if list.kind /= VN_group then -- Convert to a group
      groupify(list);
    end if;

    cur := list.child;
    while cur /= null loop
      if cur.kind = VN_group then
        to_unbounded_string(cur.child, elem, true, jstr.all);
      else
        to_unbounded_string(cur, elem, false);
      end if;
      SU.append(rval, elem);
      if cur.succ /= null then
        SU.append(rval, jstr);
      end if;

      deallocate(elem);
      cur := cur.succ;
    end loop;

    deallocate(jstr);

    -- Construct node for joined string
    new_vt_parse_node(cur, VN_word);
    cur.tok.data := rval;
    cur.tok.kind := TOK_string;
    set_result(VIO, cur, false);
  end procedure;


  procedure do_cmd_lappend( VIO : inout vt_interp_acc; args : inout vt_parse_node_acc ) is
    variable lvar : scope_var_acc;
    variable lobj, cur, cur_arg, append : vt_parse_node_acc;

    variable has_ws : boolean;
    variable astr : unbounded_string;
  begin
    assert_true(args /= null, "'lappend' Missing varName argument", failure, VIO);

    get_variable(VIO, args.tok.data.all, lvar);

    if lvar = null then -- Unknown variable, create an empty group
      new_vt_parse_node(lobj, VN_group);
      set_variable(VIO, args.tok.data.all, lobj, false); -- Pass ownership to the var
      get_variable(VIO, args.tok.data.all, lvar);
    end if;

    lobj := lvar.var.value;

    -- If an arg is a quoted string with spaces it needs to be converted to a group
    -- i.e.  lappend a "foo   bar " "baz" => "old data {foo bar} baz" or {old data {foo bar} baz}
    cur_arg := args.succ;
    while cur_arg /= null loop
      if cur_arg.tok.kind = TOK_string then
        contains_whitespace(cur_arg.tok.data, has_ws);
        if has_ws then
          groupify(cur_arg);
        end if;
      end if;
      cur_arg := cur_arg.succ;
    end loop;

    if lobj.tok.kind = TOK_string then -- Append to the string
      -- Convert args into a string
      to_unbounded_string(args.succ, astr);
      SU.append(lobj.tok.data, " " & astr.all);
      deallocate(astr);

    else -- Append to a group

      -- Convert scalar to a group
      if lobj.kind /= VN_group then
        new_vt_parse_node(cur, lobj.kind);
        cur.tok := lobj.tok;
        cur.succ := lobj.succ;
        cur.child := lobj.child;

        lobj.kind := VN_group;
        lobj.succ := null;
        lobj.child := cur;
      end if;

      -- Append args
      copy_parse_tree(args.succ, append); -- Copy all remaining arguments

      if lobj.child /= null then
        get_last(lobj.child, cur);
        cur.succ := append;
      else -- No children
        lobj.child := append;
      end if;

    end if;

    set_result(VIO, lobj);
  end procedure;



  alias maps is extras.strings_maps;
  constant PLUS_MINUS_SET : maps.character_set := ( '+'|'-' => true, others => false);

  -- // Convert an index expression in the form of (n1|"end"([+|-]n2)*) to an integer
  function parse_index( expr : string; length : integer) return integer is
    variable val1, val2, op_pos, rval : integer;
  begin
  
    report ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> PARSE IX: " & expr;

    op_pos := SF.index(expr, PLUS_MINUS_SET);
  
    if SF.index(expr, "end") = 1 then
      val1 := length-1;
    else
      if op_pos > 0 then -- Get first number
        val1 := integer'value(expr(expr'low to op_pos-1));
      else -- Treat as a single number
        val1 := integer'value(expr);
      end if;
    end if;
    
    if op_pos > 0 then -- There is an operator
      report ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> GOT INDEX OP";
      val2 := integer'value(expr(op_pos+1 to expr'high));
      if expr(op_pos) = '+' then -- Plus
        rval := val1 + val2;
      else -- Minus
        rval := val1 - val2;
      end if;
    else -- No operator
      rval := val1;
    end if;

    return rval;

  end function;
  
  procedure parse_index( node : inout vt_parse_node_acc; length : in integer; index : out integer) is
  begin
    if node.tok.kind = TOK_string then
      index := parse_index(node.tok.data.all, length);
    elsif node.tok.kind = TOK_integer then
      index := node.tok.value;
    else
      report "Invalid index value" severity failure;  
    end if;
  end procedure;
  

  procedure do_cmd_lindex( VIO : inout vt_interp_acc; args : inout vt_parse_node_acc ) is
    variable start_ix, end_ix, cur, selected_list, elem : vt_parse_node_acc;
    variable have_indices : boolean;
    variable count, ix : integer;
  begin
    assert_true(args /= null and args.kind = VN_group, "'lindex' Missing list", failure, VIO);

    have_indices := false;
    if args.succ /= null then -- Have index args
      if args.succ.kind = VN_group then
        start_ix := args.succ.child;
        get_last(args.succ.child, end_ix);
      else
        start_ix := args.succ;
        get_last(args.succ, end_ix);
      end if;

      if start_ix /= null then
        have_indices := true;
      end if;
    end if;

    if not have_indices then -- Return entire list
      copy_parse_tree(args, elem, false); -- FIXME: make a no-copy operation
      set_result(VIO, elem, false);

    else -- Index into the list

      -- Look up nested groups until the last one we're going to retrieve an element from
      selected_list := args;
      cur := start_ix;
      loop
        assert_true(selected_list.kind = VN_group, "'lindex' Expecting a nested list object", failure, VIO);

        -- Compute next index      
        arg_count(selected_list.child, count);
--        if cur.tok.kind = TOK_string then
--          ix := parse_index(cur.tok.data.all, count);
--        else
--          ix := cur.tok.value;
--        end if;
        parse_index(cur, count, ix);
       
        assert_true(ix >= 0 and ix < count, "'lindex' index out of range: " & integer'image(ix), failure, VIO);
       
        -- Get nested list
        get_element(selected_list.child, ix, selected_list);
        
        exit when cur = end_ix;
        
        cur := cur.succ;
      end loop;

      -- Retrieve indexed value
      copy_parse_tree(selected_list, elem, false);
      set_result(VIO, elem, false);
    end if;
  end procedure;


  procedure do_cmd_linsert( VIO : inout vt_interp_acc; args : inout vt_parse_node_acc ) is
    variable index_expr, cur, new_list, insert_point, succ : vt_parse_node_acc;
    variable llength, index, i : integer;
  begin
    assert_true(args /= null, "'linsert' Missing list", failure, VIO);
    write_parse_tree("linsert_tree.txt", args);
    assert_true(args.kind = VN_group, "'linsert' Expecting list argument", failure, VIO);
    assert_true(args.succ /= null, "'linsert' Missing index", failure, VIO);
    index_expr := args.succ;

    -- Get the list length
    cur := args.child;
    llength := 0;
    while cur /= null loop
      llength := llength + 1;
      cur := cur.succ;
    end loop;

    -- Adjust index
--    if index_expr.tok.kind = TOK_string then
--      index := parse_index(index_expr.tok.data.all, llength+1);
--    else
--      index := index_expr.tok.value;
--    end if;
    parse_index(index_expr, llength+1, index);
    
    -- Check bounds
    if index > llength then
      index := llength;
    elsif index < 0 then
      index := 0;   
    end if;
    
    -- Copy the list
    copy_parse_tree(args, new_list, false);
    
    if index_expr.succ /= null then -- There are elements to insert
      
      if index > 0 then
        -- Find the node to insert after
        get_element(new_list.child, index-1, insert_point);
        succ := insert_point.succ;
        insert_point.succ := index_expr.succ;
        
      else -- Start of list
        succ := new_list.child;
        new_list.child := index_expr.succ;
      end if;
      
      -- Find end of new elements
      get_last(index_expr.succ, cur);
      cur.succ := succ;
      
      -- Disconnect list elements from arg list    
      index_expr.succ := null;

    end if;
        
    set_result(VIO, new_list, false);

  end procedure;
  
  
  procedure do_cmd_list( VIO : inout vt_interp_acc; args : inout vt_parse_node_acc ) is
    variable lobj, cur : vt_parse_node_acc;
    variable has_ws : boolean;
  begin
    if args = null then
      set_result(VIO);
      return;
    end if;

    -- Convert args into a group
    new_vt_parse_node(lobj, VN_group);
    copy_parse_tree(args, lobj.child);

    -- Groupify strings containing whitespace
    -- NOTE: This doesn't preserve whitespace as in Tcl. We could alternately mark
    -- group-strings so that they render with {}'s during string conversion of their parent.
    cur := lobj.child;
    while cur /= null loop
      if cur.tok.kind = TOK_string then
        contains_whitespace(cur.tok.data, has_ws);
        if has_ws then
          groupify(cur);
        end if;
      end if;
      cur := cur.succ;
    end loop;
    set_result(VIO, lobj, false); -- FIXME: make copy in set_result()?
  end procedure;



  procedure do_cmd_llength( VIO : inout vt_interp_acc; args : inout vt_parse_node_acc ) is
    variable count : natural;
    variable ch : character;
    variable in_word : boolean := false;
  begin
    expect_arg_count(args, 1, "llength", "list", VIO);

    if args.kind = VN_group then
      arg_count(args.child, count);

    elsif args.tok.kind = TOK_string and args.tok.data.all'length > 0 then -- Count spaces
      -- FIXME: This should ignore spaces in {} groups and ""
      for i in args.tok.data.all'range loop
        ch := args.tok.data(i);
        if ch = ' ' or ch = HT then
          in_word := false;
        else
          if not in_word then
            count := count + 1;
          end if;
          in_word := true;
        end if;
      end loop;
    end if;

    set_result(VIO, count);
  end procedure;
  
  procedure do_cmd_lrange( VIO : inout vt_interp_acc; args : inout vt_parse_node_acc ) is
    variable llength, six, eix: integer;
    variable start_node, cur : vt_parse_node_acc;
  begin
    expect_arg_count(args, 3, "lrange", "list first last", VIO);
    assert_true(args.kind = VN_group, "'lrange' expecting a list", failure, VIO);

    -- Get the range indices
    arg_count(args.child, llength);
    parse_index( args.succ, llength, six);
    parse_index( args.succ.succ, llength, eix);
    
    if six < 0 then
      six := 0;
    end if;
    if eix >= llength then
      eix := llength-1;
    end if;
    
    report "############################ LRANGE: " & integer'image(six) & "  " & integer'image(eix) & "  " & integer'image(llength);
    
    if six > eix then -- Return empty string
      set_result(VIO);
    else -- Return range
      -- Remove all siblings after the end node
      get_element(args.child, eix, cur);
      free(cur.succ);
      cur.succ := null;
      
      if six = 0 then -- Keep start of group
        start_node := args.child;
        args.child := null;
      else -- Disconnect preceding nodes
        get_element(args.child, six-1, cur);
        start_node := cur.succ;
        cur.succ := null;
      end if;
    end if;
    
    new_vt_parse_node(cur, VN_group);
    cur.child := start_node;

    set_result(VIO, cur, false);    
  end procedure;


  procedure do_cmd_lrepeat( VIO : inout vt_interp_acc; args : inout vt_parse_node_acc ) is
    variable count : integer;
    variable rval, last, elements, next_elements : vt_parse_node_acc;
  begin
    assert_true(args /= null and args.tok.kind = TOK_integer, "'lrepeat' Missing count", failure, VIO);
    
    count := args.tok.value;
    
    assert_true(count > 0, "'lrepeat' Invalid count", failure, VIO);
    
    new_vt_parse_node(rval, VN_group);

    if args.succ /= null then
      rval.child := args.succ;
      elements := rval.child;
      
      for i in 2 to count loop
        get_last(elements, last);
        copy_parse_tree(elements, next_elements);
        elements := next_elements;
        last.succ := elements;
      end loop;
    
      args.succ := null; -- Disconnect args reused for first repetition
    end if;

    set_result(VIO, rval, false);    
  end procedure;


  procedure do_cmd_lreplace( VIO : inout vt_interp_acc; args : inout vt_parse_node_acc ) is
    variable llength, six, eix : integer;
    variable last, elements, cur, succ : vt_parse_node_acc;
  begin
    assert_true(args /= null and args.kind = VN_group, "'lreplace' Missing list argument", failure, VIO);
    assert_true(args.succ /= null and args.succ.succ /= null, "'lrepleace' Missing first and last indices", failure, VIO);    
    
    arg_count(args.child, llength);

    if llength > 0 then
      parse_index( args.succ, llength, six);
      parse_index( args.succ.succ, llength, eix);

      assert_true(six < llength, "'lreplace' list doesn't contain element " & integer'image(six), failure, VIO);
      
      if eix >= six and eix >= 0 then -- Delete elements being replaced
        if eix >= llength then
          eix := llength - 1;
        end if;
        
        if six > 0 then -- Deleting after first node
          get_element(args.child, six-1, cur);
          get_element(args.child, eix, last);
          succ := cur.succ;
          cur.succ := last.succ;
          last.succ := null;
          free(succ);
          
        else -- Deleting from first node
          get_element(args.child, eix, last);
          succ := args.child;
          args.child := last.succ;
          last.succ := null;
          free(succ);
        end if;
      end if;
      
      six := six - 1;
        
    else -- Insert into empty list
      six := -1;
    end if;
    
    -- Disconnect inserted elements
    last := args.succ.succ;    
    elements := last.succ;
    last.succ := null;

    if elements /= null then
      get_last(elements, last);
      
      if six < 0 then -- Insert before start of list
        last.succ := args.child;
        args.child := elements;
      else -- Insert after node before first
        get_element(args.child, six, cur);
        last.succ := cur.succ;
        cur.succ := elements;
      end if;
    end if;
    
    -- Release first and last indices
    free(args.succ);
    args.succ := null;
    
    -- Disconnect the modified args list from its parent command so we can use
    -- it as the return value without copying
    VIO.scope.script.cur_cmd.child.succ := null;
    set_result(VIO, args, false);

  end procedure;


  procedure do_cmd_lreverse( VIO : inout vt_interp_acc; args : inout vt_parse_node_acc ) is
    variable rlist, cur, succ : vt_parse_node_acc;
  begin
    expect_arg_count(args, 1, "lreverse", "list", VIO);
    assert_true(args.kind = VN_group, "'lreverse' Expecting list", failure, VIO);
    
    rlist := args.child;
    if rlist /= null then
      cur := rlist.succ;
      rlist.succ := null;
      while cur /= null loop
        succ := cur.succ;
        cur.succ := rlist;
        rlist := cur;
        cur := succ;
      end loop;
      
      args.child := rlist;
    end if;
    
    -- Disconnect the modified args list from its parent command so we can use
    -- it as the return value without copying
    VIO.scope.script.cur_cmd.child.succ := null;
    set_result(VIO, args, false);
  end procedure;


  procedure do_cmd_lset( VIO : inout vt_interp_acc; args : inout vt_parse_node_acc ) is
    variable lvar : scope_var_acc;
    variable lobj, cur, value, start_ix, end_ix, selected_list : vt_parse_node_acc;
    variable count : natural;
    variable ix : integer;
  begin
    assert_true(args /= null, "'lset' Missing varName argument", failure, VIO);

    get_variable(VIO, args.tok.data.all, lvar);
    
    assert_true(lvar /= null, "'lset' can't read '" & args.tok.data.all & "': no such variable", failure, VIO);

    lobj := lvar.var.value;

    -- Look for index args
    arg_count(args.succ, count);
    assert_true(count > 0, "wrong # args: should be 'lset listVar ?index? ?index...? value'", failure, VIO);

    if count = 1 or (count = 2 and args.succ.kind = VN_group and args.succ.child = null) then -- No index
      -- Replace variable with new value
      set_variable(VIO, args.tok.data.all, value); -- FIXME: eliminate copy
      return;
    elsif count = 2 then -- 1 index or a grouped index
      if args.succ.kind = VN_group then
        start_ix := args.succ.child;
        get_last(args.succ.child, end_ix);
      else -- 1 index
        start_ix := args.succ;
        end_ix := args.succ;
      end if;
    else -- Multiple indices
      -- Find first and last index
      start_ix := args.succ;
      get_element(args.succ, count-2, end_ix);
    end if;

    -- Disconnect the value node
    get_element(args.succ, count-2, cur); -- Next to last node
    value := cur.succ;
    cur.succ := null;
    
    -- Look up nested groups until the last one we're going to set
    selected_list := lobj;
    cur := start_ix;
    loop
      assert_true(selected_list.kind = VN_group, "'lset' Expecting a nested list object", failure, VIO);

      -- Compute next index      
      arg_count(selected_list.child, count);
--      if cur.tok.kind = TOK_string then
--        ix := parse_index(cur.tok.data.all, count);
--      else
--        ix := cur.tok.value;
--      end if;
      parse_index(cur, count, ix);
      
      exit when cur = end_ix;
      
      assert_true(ix >= 0 and ix < count, "'lset' index out of range: " & integer'image(ix), failure, VIO);
      
      -- Get nested list
      get_element(selected_list.child, ix, selected_list);
      
      cur := cur.succ;
    end loop;
    
    
    assert_true(ix >= 0 and ix <= count, "'lset' index out of range: " & integer'image(ix), failure, VIO);
    
    if ix = count then -- Append new value
      get_last(selected_list.child, cur);
      cur.succ := value;
    else -- Replace index
      get_element(selected_list.child, ix, cur);
      free(cur.tok);
      free(cur.child);
      cur.tok := value.tok;
      cur.tok.kind := value.tok.kind;
      cur.kind := value.kind;
      cur.child := value.child;
      value.tok.data := null;
      value.child := null;
      free(value);
    end if;
    
    set_result(VIO, lobj, true);
    
  end procedure;


  procedure do_cmd_proc( VIO : inout vt_interp_acc; args : inout vt_parse_node_acc ) is
    variable arg_defs, cbody, cur, child : vt_parse_node_acc;
    variable count : natural;
  begin
    expect_arg_count(args, 3, "proc", "name args body", VIO);

    -- Get the arg definitions
    -- We will accept a group or a single identifier
    if args.succ.kind = VN_group then
      copy_parse_tree(args.succ.child, arg_defs);
      -- Reorganize the group elements so that default values are children
      cur := arg_defs;
      while cur /= null loop
        if cur.kind = VN_group then
          child := cur.child;
          assert_true(child.tok.kind = TOK_string,
            "proc argument must be a string", failure, VIO);
          cur.tok := child.tok; -- Move the arg name token to top level node
          cur.child := child.succ; -- Connect any default value
          child.tok.data := null;
          child.succ := null;
          free(child);
        end if;
        cur.kind := VN_word; --VN_argument;
        cur := cur.succ;
      end loop;
    else -- Single argument
      new_vt_parse_node(arg_defs, VN_word); --VN_argument);
      copy_token(args.succ.tok, arg_defs.tok);
    end if;

    --write_parse_tree("proc_args.txt", arg_defs);

    copy_parse_tree(args.succ.succ, cbody);
    group_to_script(VIO, cbody);

--    convert_to_commands(cbody);
    --write_parse_tree("proc_body.txt", cbody);

    def_command(VIO, args.tok.data.all, CMD_proc_def, arg_defs, cbody);
    set_result(VIO);
  end procedure;


  procedure do_cmd_puts( VIO : inout vt_interp_acc; args : inout vt_parse_node_acc ) is
    variable msg : unbounded_string;
  begin
    --report "@@@@@ puts args: " & vt_token_kind'image(args.tok.kind) & " >" & args.tok.data.all & "<";
    --report "DBG: " & integer'image(args.id);
    to_unbounded_string(args, msg);
    report msg.all severity note;
    deallocate(msg);

    set_result(VIO);
  end procedure;


  procedure do_cmd_rename( VIO : inout vt_interp_acc; args : inout vt_parse_node_acc ) is
    variable cmd_def : command_info_acc;
  begin
    expect_arg_count(args, 2, "rename", "oldName newName", VIO);
    
    stringify(args);
    stringify(args.succ);
    
    -- Lookup existing command
    get_command(VIO, args.tok.data.all, cmd_def);
    
    assert_true(cmd_def /= null, "can't rename """ & args.tok.data.all & """: command doesn't exist", failure, VIO);
    
    if args.succ.tok.data.all = "" then
      del_command(VIO, args.tok.data.all);
    else
      rename_command(VIO, args.tok.data.all, args.succ.tok.data.all);
    end if;
    
    set_result(VIO);
  end procedure;
  

  procedure do_cmd_return( VIO : inout vt_interp_acc; args : inout vt_parse_node_acc ) is
    variable cur : vt_parse_node_acc;
  begin
    -- FIXME: handle additional command parameters

    --report "DBG: return cmd node id: " & integer'image(args.id);
    cur := args;
    if cur /= null then
      set_result(VIO, cur);
      report "DBG: return cmd: set return value";
    else -- Return empty string
      set_result(VIO);
    end if;

    -- We want to exit the current scope if it is not the global scope
    -- This will be done in prepare_next_cmd() after we set the cur_cmd to null
    -- at the top-level script in this scope.
    while VIO.scope.script /= VIO.scope.script_stack loop
      pop_script(VIO);
    end loop;
    VIO.scope.script.cur_cmd := null;
    VIO.scope.script.cur_arg := null;

  end procedure;


  procedure do_cmd_set( VIO : inout vt_interp_acc; args : inout vt_parse_node_acc) is

    variable var : scope_var_acc;
  begin
    -- First arg is the variable name to set
    assert_true(args /= null and args.tok.kind = TOK_string,
      "Expecting identifier as variable name", failure, VIO);

    if args.succ /= null then -- Got optional value: We need to set the variable
      assert_true(args.succ.succ = null, "'set' Wrong # args", failure, VIO);
      set_variable(VIO, args.tok.data.all, args.succ);
      --set_result(VIO, args.succ);
      get_variable(VIO, args.tok.data.all, var);
      set_result(VIO, var.var.value);
    else -- Return value of variable if it exists
      get_variable(VIO, args.tok.data.all, var);
      if var /= null then
        set_result(VIO, var.var.value);
      else
        set_result(VIO);
        assert_true(false, "'set' Unknown variable '" & args.tok.data.all & "'",
          error, VIO);
      end if;
    end if;

  end procedure;


  use extras.strings_maps.all;  
  
  procedure do_cmd_split(VIO : inout vt_interp_acc; args : inout vt_parse_node_acc) is
    variable split_chars : unbounded_string;
    variable split_set : character_set;
    variable lobj, tail, element : vt_parse_node_acc;
    variable ch : character;
    variable start : natural;
  begin
    assert_true(args /= null, "'split' Missing string", failure, VIO);
    
    stringify(args); -- Make sure it is a string
    
    -- Determine which chars to split on
    if args.succ /= null then
      stringify(args.succ);
      split_chars := args.succ.tok.data;
      args.succ.tok.data := null;
      args.succ.tok.kind := TOK_UNKNOWN;
    else
      split_chars := SU.to_unbounded_string(" " & HT & LF);
    end if;

    
    new_vt_parse_node(lobj, VN_group);
    
    if split_chars'length = 0 then -- Empty string; each char is an element
      for i in args.tok.data'range loop
        new_vt_parse_node(element, VN_word);
        element.tok.data := SU.to_unbounded_string(args.tok.data(i to i));
        element.tok.kind := TOK_string;
        if tail = null then
          lobj.child := element;
        else
          tail.succ := element;
        end if;
        tail := element;
      end loop;

    else -- Splitting on chars
      split_set := to_set(split_chars.all);
      start := args.tok.data'low;

      for i in args.tok.data'range loop
        ch := args.tok.data(i);
        if split_set(ch) then -- This is a split char
          new_vt_parse_node(element, VN_word);
          element.tok.data := SU.to_unbounded_string(args.tok.data(start to i-1));
          element.tok.kind := TOK_string;
          if tail = null then
            lobj.child := element;
          else
            tail.succ := element;
          end if;
          tail := element;
          start := i + 1;
        end if;
      end loop;
      
      if start <= args.tok.data'high or split_set(args.tok.data(args.tok.data'high)) then -- Append final element
        new_vt_parse_node(element, VN_word);
        element.tok.data := SU.to_unbounded_string(args.tok.data(start to args.tok.data'high));
        element.tok.kind := TOK_string;
        if tail = null then
          lobj.child := element;
        else
          tail.succ := element;
        end if;
      end if;
    end if;
    
    deallocate(split_chars);
    set_result(VIO, lobj, false);
  end procedure;
  

  procedure do_cmd_string(VIO : inout vt_interp_acc; args : inout vt_parse_node_acc) is
    variable id : ensemble_id;
    variable return_string : boolean := false;
    variable a_string, cur, str2 : vt_parse_node_acc;
    variable lset, rset : character_set;
    variable ix, ix2 : integer;
    variable tstr : unbounded_string;
    variable parsing_opts, nocase_opt : boolean;
    variable length_opt : integer;
  begin
    assert_true(args /= null and args.tok.kind = TOK_string, "'string' expecting subcommand", failure, VIO);
    
    -- Lookup subcommand
    get_ensemble(VIO, args.tok.data.all, id);

    a_string := args.succ;
        
    case id is
      when ENS_cat =>
        stringify(a_string);
        if a_string /= null then
          cur := a_string;
          while cur /= null loop
            stringify(cur);
            cur := cur.succ;
          end loop;
          
          -- Concat args
          cur := a_string.succ;
          while cur /= null loop
            SU.append(a_string.tok.data, cur.tok.data);
            cur := cur.succ;
          end loop;
          
          return_string := true;
        
        else -- Return empty string
          set_result(VIO);
          return;
        end if;
      when others =>
        null;
    end case;
    

    assert_true(a_string /= null, "'string' missing string arg", failure, VIO);
    if id /= ENS_map then -- First arg should be a string
      stringify(a_string);
    end if;
    
    case id is
      when ENS_compare | ENS_equal =>
        parsing_opts := true;
        nocase_opt := false;
        length_opt := -1;
        -- Look for options
        cur := args.succ;

        while cur /= null loop
          exit when cur.tok.kind /= TOK_string or cur.tok.data(1) /= '-';
          
          if cur.tok.data.all = "-nocase" then
            nocase_opt := true;
          elsif cur.tok.data.all = "-length" then -- Get length parmaeter
            assert_true(cur.succ /= null and cur.succ.tok.kind = TOK_integer, "'string' expecting integer length parameter", failure, VIO);
            length_opt := cur.succ.tok.value;
            cur := cur.succ;
          end if;
        
          cur := cur.succ;
        end loop;
        
        report "@@@@@@@@@@@@@@@@@@@@ COMPARE OPTS: " & cur.tok.data.all;
        
        -- cur points to string1
        assert_true(cur.succ /= null, "'string' missing second string to compare against", failure, VIO);
        str2 := cur.succ; -- str2 points to string2
        stringify(str2);
        
        if nocase_opt then -- Convert to lower case for case insensitive compare
          SU.translate(cur.tok.data, LOWER_CASE_MAP);
          SU.translate(str2.tok.data, LOWER_CASE_MAP);
        end if;
        
       
        -- Get length of shortest string
        ix := cur.tok.data'length;
        if str2.tok.data'length < ix then
          ix := str2.tok.data'length;
        end if;
        
        ix2 := ix;
        if length_opt >= 0 and length_opt < ix then
          ix2 := length_opt;
        end if;
        
        -- Do char by char comparison over matching lengths
        ix := 0;
        for i in 1 to ix2 loop
          ix := character'pos(cur.tok.data(i)) - character'pos(str2.tok.data(i));
          exit when ix /= 0;
          -- Else chars were equal so keep looping
        end loop;
        
        if ix = 0 then -- Compared sub-strings matched
          ix := cur.tok.data'length;
          ix2 := str2.tok.data'length;
        
          if length_opt >= 0 then
            if ix > length_opt then
              ix := length_opt;
            end if;
            if ix2 > length_opt then
              ix2 := length_opt;
            end if;
          end if;

          if ix = ix2 then
            ix := 0;
          elsif ix > ix2 then
            ix := 1;
          else
            ix := -1;
          end if;
        end if;
        
        
        case id is
          when ENS_compare =>
            if ix > 0 then
              ix := 1;
            elsif ix < 0 then
              ix := -1;
            end if;
            
          when ENS_equal =>
            if ix /= 0 then
              ix := 0;
            else
              ix := 1;
            end if;
          
          when others =>
            null;
        end case;
        
        set_result(VIO, ix);
      
      
      when ENS_first | ENS_last =>
        str2 := a_string.succ;
        assert_true(str2 /= null, "'string' missing haystackString", failure, VIO);
        stringify(str2);
        
        if id = ENS_first then
          ix := 0;
        else
          ix := str2.tok.data'length - 1;
        end if;
        
        if str2.succ /= null then -- Get index
          parse_index(str2.succ, str2.tok.data'length, ix);
        end if;
        
        if id = ENS_first then
          ix := SF.index(str2.tok.data.all(str2.tok.data'left + ix to str2.tok.data'right),
            a_string.tok.data.all) - 1;
        else
          ix := SF.index(str2.tok.data.all(str2.tok.data'left to str2.tok.data'left + ix),
            a_string.tok.data.all, strings.backward) - 1;
        end if;
        
        set_result(VIO, ix);
        
      
      when ENS_index =>
        ix := 0;
        assert_true(a_string.succ /= null, "'string' missing charIndex", failure, VIO);
        parse_index(a_string.succ, a_string.tok.data'length, ix);
        if ix < 0 or ix >= a_string.tok.data'length then
          set_result(VIO);
        else
          set_result(VIO, a_string.tok.data(a_string.tok.data'left + ix));
        end if;
        

      when ENS_length =>
        set_result(VIO, a_string.tok.data'length);
      
      when ENS_map =>
        assert_true(a_string.succ /= null, "'string' missing string parameter to map", failure, VIO);
        a_string := a_string.succ;
        stringify(a_string);
        
        assert_true(args.succ.kind = VN_group, "'string' expecting group for map command", failure, VIO);
        arg_count(args.succ.child, ix2);
        assert_true(ix2 / 2 * 2 = ix2, "'string' char map list unbalanced", failure, VIO);
        
        -- Stringify map elements
        cur := args.succ.child;
        while cur /= null loop
          stringify(cur);
          cur := cur.succ;
        end loop;
        
        ix := 1; -- Current string pos
        while ix <= a_string.tok.data'length loop
          -- Check each mapping for a match to the current position in the string
          cur := args.succ.child;
          while cur /= null loop
            if cur.tok.data'length <= a_string.tok.data'length - ix + 1 then -- Look for a match
              if cur.tok.data.all = a_string.tok.data(ix to ix + cur.tok.data'length - 1) then
                SU.replace_slice(a_string.tok.data, ix, ix + cur.tok.data'length - 1, cur.succ.tok.data.all);
                ix := ix + cur.succ.tok.data'length - 1;
                exit;
              end if;
            end if;
            cur := cur.succ.succ;
          end loop;
          ix := ix + 1;
        end loop;
        
        -- Disconnect string arg
        free(a_string.succ);
        a_string.succ := null;
        args.succ.succ := null;
        set_result(VIO, a_string, false);

        
      
      when ENS_range =>
        assert_true(a_string.succ /= null and a_string.succ.succ /= null, "'string' expecting range indices", failure, VIO);
        parse_index(a_string.succ, a_string.tok.data'length, ix);
        parse_index(a_string.succ.succ, a_string.tok.data'length, ix2);
        if ix < 0 then
          ix := 0;
        end if;
        if ix2 >= a_string.tok.data'length then
          ix2 := a_string.tok.data'length-1;
        end if;
        
        if ix <= ix2 then
          tstr := new string(1 to ix2-ix+1);
          tstr(tstr'range) := a_string.tok.data(ix+1 to ix2+1);
          --SU.slice(a_string.tok.data, ix, ix2, tstr);
          deallocate(a_string.tok.data);
          a_string.tok.data := tstr;
          return_string := true;
        else
          set_result(VIO);
        end if;
      
      
      when ENS_repeat =>
        assert_true(a_string.succ /= null and a_string.succ.tok.kind = TOK_integer, "'string' invalud repeat count", failure, VIO);
        
        ix := a_string.succ.tok.value;
        
        if ix > 0 then -- FIXME: implement with "*"
          tstr := a_string.tok.data;
          a_string.tok.data := null;
          for i in 1 to a_string.succ.tok.value loop
            SU.append(a_string.tok.data, tstr);
          end loop;
          deallocate(tstr);
          return_string := true;
        else
          set_result(VIO);
        end if;
      
      
      when ENS_replace =>
        assert_true(a_string.succ /= null and a_string.succ.succ /= null, "'string' expecting replace indices", failure, VIO);
        parse_index(a_string.succ, a_string.tok.data'length, ix);
        parse_index(a_string.succ.succ, a_string.tok.data'length, ix2);
        if ix < 0 then
          ix := 0;
        end if;
        if ix2 >= a_string.tok.data'length then
          ix2 := a_string.tok.data'length-1;
        end if;
        
        if ix <= ix2 and ix < a_string.tok.data'length and ix2 >= 0 then -- Replace string
          str2 := a_string.succ.succ.succ;
          if str2 /= null then -- Use replacement string
            stringify(str2);
            SU.replace_slice(a_string.tok.data, ix+1, ix2+1, str2.tok.data.all);
          else -- Replace with nothing
            SU.replace_slice(a_string.tok.data, ix+1, ix2+1, "");
          end if;
        end if;

        return_string := true;
      
      
      when ENS_reverse =>
        ix := a_string.tok.data'length;
        tstr := new string(1 to ix);
        for i in a_string.tok.data'range loop
          tstr(ix-i+1) := a_string.tok.data(i);
        end loop;
        deallocate(a_string.tok.data);
        a_string.tok.data := tstr;
        return_string := true;
        
      when ENS_tolower | ENS_totitle | ENS_toupper =>
        ix := 0;
        ix2 := a_string.tok.data'length-1;
        
        -- Check for optional indices
        if a_string.succ /= null then
          parse_index(a_string.succ, a_string.tok.data'length, ix);
          ix2 := ix;
          if a_string.succ.succ /= null then
            parse_index(a_string.succ.succ, a_string.tok.data'length, ix2);
          end if;
        end if;
        
        if ix < 0 then
          ix := 0;
        end if;
        if ix2 >= a_string.tok.data'length then
          ix2 := a_string.tok.data'length-1;
        end if;
        
        ix := ix + 1;
        ix2 := ix2 + 1;
        
        case id is
          when ENS_tolower =>
            SU.replace_slice(a_string.tok.data, ix, ix2, SF.translate(a_string.tok.data(ix to ix2), LOWER_CASE_MAP));
          when ENS_toupper =>
            SU.replace_slice(a_string.tok.data, ix, ix2, SF.translate(a_string.tok.data(ix to ix2), UPPER_CASE_MAP));
          when ENS_totitle =>
            if ix2 > ix then
              -- Capitalize first char
              SU.replace_slice(a_string.tok.data, ix, ix, SF.translate(a_string.tok.data(ix to ix), UPPER_CASE_MAP));
              -- Lower case everything else
              SU.replace_slice(a_string.tok.data, ix+1, ix2, SF.translate(a_string.tok.data(ix+1 to ix2), LOWER_CASE_MAP));
            else -- Just capitalize one char
              SU.replace_slice(a_string.tok.data, ix, ix2, SF.translate(a_string.tok.data(ix to ix2), UPPER_CASE_MAP));            
            end if;          
          
          when others =>
            null;
        end case;
        return_string := true;
      

      when ENS_trim =>
        -- Get optional char list
        if a_string.succ /= null then
          lset := to_set(a_string.succ.tok.data.all);
          SU.trim(a_string.tok.data, lset, lset);
        else -- Trim whitespace
          SU.trim(a_string.tok.data, strings.both);
        end if;
        return_string := true;        
        
      when ENS_trimleft =>
        -- Get optional char list
        if a_string.succ /= null then
          lset := to_set(a_string.succ.tok.data.all);
          rset := (others => false);
          SU.trim(a_string.tok.data, lset, rset);
        else -- Trim whitespace
          SU.trim(a_string.tok.data, strings.left);
        end if;
        return_string := true;

      when ENS_trimright =>
        -- Get optional char list
        if a_string.succ /= null then
          lset := (others => false);
          rset := to_set(a_string.succ.tok.data.all);
          SU.trim(a_string.tok.data, lset, rset);
        else -- Trim whitespace
          SU.trim(a_string.tok.data, strings.right);
        end if;
        return_string := true;

      when ENS_UNKNOWN =>
        report "UNKNOWN subcommand: " & args.tok.data.all severity failure;
      when others =>
        null;
    end case;
    
    if return_string then
      -- Disconnect string arg
      free(a_string.succ);
      a_string.succ := null;
      args.succ := null;
      set_result(VIO, a_string, false);
    end if;
  end procedure;
  
  
  procedure do_cmd_subst(VIO : inout vt_interp_acc; args : inout vt_parse_node_acc) is
    variable cur, prev : vt_parse_node_acc;
    variable nobackslashes, nocommands, novariables : boolean;
  begin
    -- Look for options
    cur := args;
    while cur /= null loop
      exit when cur.tok.kind /= TOK_string or cur.tok.data(1) /= '-';
      
      if cur.tok.data.all = "-nobackslashes" then
        nobackslashes := true;
      elsif cur.tok.data.all = "-nocommands" then
        nocommands := true;
      elsif cur.tok.data.all = "-novariables" then
        novariables := true;
      else
        assert_true(false, "'subst' bad switch """ & cur.tok.data.all
          & """: must be -nobackslashes, -nocommands, or -novariables", failure, VIO);
      end if;
      
      prev := cur;
      cur := cur.succ;
    end loop;
    
    assert_true(cur.succ = null, "'subst' Too many arguments", failure, VIO);
    
    if cur.kind = VN_group or cur.tok.kind = TOK_string then -- We may have substitutions to make
      stringify(cur);
      write_parse_tree("subst_tree.txt", cur);
      
      -- Substitute variables and backslashes
      if (not novariables) or (not nobackslashes) then
        report "SUBST("& cur.tok.data.all &") nb=" & boolean'image(nobackslashes) & "  nv=" & boolean'image(novariables) severity error;
        substitute(VIO, cur, nobackslashes, novariables);
      end if;
      
      -- Substitute commands
      if not nocommands then
        -- FIXME: Implement cmd subst
      end if;
    end if;

    report "SUBST RESULT: >" & cur.tok.data.all &"<";

    if prev /= null then
      prev.succ := null;
    else
      -- Disconnect the modified args list from its parent command so we can use
      -- it as the return value without copying
      VIO.scope.script.cur_cmd.child.succ := null;
    end if;
    set_result(VIO, cur, false);
  end procedure;
  
  
  procedure do_cmd_unknown( VIO : inout vt_interp_acc; cmd : inout vt_parse_node_acc ) is
  begin
    assert_true(false, "invalid command name """ & cmd.tok.data.all & """", error, VIO);
  end procedure;
  

  procedure do_cmd_unset( VIO : inout vt_interp_acc; args : inout vt_parse_node_acc ) is
    variable cur : vt_parse_node_acc;
    variable nocomplain : boolean := false;
    variable var : scope_var_acc;
  begin
    -- Look for options
    cur := args;
    while cur /= null loop
      exit when cur.tok.kind /= TOK_string or cur.tok.data(1) /= '-';
      
      if cur.tok.data.all = "-nocomplain" then
        nocomplain := true;
      elsif cur.tok.data.all = "--" then -- End of options
        exit;
      end if;
    
      cur := cur.succ;
    end loop;
    
    -- FIXME: handle arrays
    
    while cur /= null loop
      get_variable(VIO, cur.tok.data.all, var);
      assert_true(var /= null or nocomplain, "can't unset "& cur.tok.data.all &": no such variable", failure, VIO);
      if var /= null then
        del_variable(VIO, cur.tok.data.all);
      end if;
      cur := cur.succ;
    end loop;

    set_result(VIO);
  end procedure;


  procedure do_cmd_uplevel( VIO : inout vt_interp_acc; args : inout vt_parse_node_acc )is
    variable cur, cmd_list : vt_parse_node_acc;
    variable level : integer := 1;
    variable i : integer;
    variable outer_scope : scope_obj_acc;
    variable ul_scope : ul_scope_obj_acc;
  begin
    cur := args;

    if cur.tok.kind = TOK_integer then
      level := cur.tok.value;
      cur := cur.succ;
    elsif cur.tok.kind = TOK_string and cur.tok.data(1) = '#' then
      level := integer'value(cur.tok.data(2 to cur.tok.data'high)) * (-1);
      cur := cur.succ;
    end if;
    
    -- Look up the referenced scope
    if level >= 0 then -- Level is relative to current scope
      i := level;
      outer_scope := VIO.scope;
      while i > 0 and outer_scope /= null loop
        outer_scope := outer_scope.prev;
        i := i - 1;
      end loop;
    else -- Level is relative to global scope
      i := level;
      outer_scope := VIO.scope_stack;
      while i < 0 and outer_scope /= null loop
        outer_scope := outer_scope.succ;
        i := i + 1;
      end loop;
    end if;

    assert_true(outer_scope /= null, "'uplevel' Invalid scope level", failure, VIO);
    
    -- Build the new script
--    write_parse_tree("uplevel_script.txt", cur);
    concat_args(cur, true);
    write_parse_tree("uplevel_script.txt", cur);
    copy_parse_tree(cur, cmd_list);
    group_to_script(VIO, cmd_list);

--    convert_to_commands(cmd_list);
--    write_parse_tree("uplevel_script.txt", cmd_list);
--    assert_true(cmd_list /= null and cmd_list.kind = VN_cmd_list,
--      "Expecting command list in 'uplevel'", failure, VIO);

    
    -- If selected scope is the current scope then just push an ordinary script
    if outer_scope = VIO.scope then
      push_script(VIO.scope, MODE_NORMAL, cmd_list);

    else
      
      -- Disconnect higher level scopes
      ul_scope := new ul_scope_obj;
      ul_scope.scope_stack := outer_scope.succ;
      outer_scope.succ := null;
      ul_scope.succ := VIO.uplevel_scopes;
      VIO.uplevel_scopes := ul_scope;
      
      VIO.scope := outer_scope;
      
      -- Add new script to outer scope
      push_script(VIO.scope, MODE_NORMAL, cmd_list);
      VIO.scope.script.uplevel_script := true;
    end if;

  end procedure;


  procedure do_cmd_upvar( VIO : inout vt_interp_acc; args : inout vt_parse_node_acc ) is
    variable cur, myvar, vobj : vt_parse_node_acc;
    variable level : integer := 1;
    variable i : integer;
    variable outer_scope : scope_obj_acc;
    variable var : scope_var_acc;
  begin
    --report ">>>>>>>>> Running upvar";
    assert_true(args /= null, "'upvar' Missing arguments", failure, VIO);
    -- Retrieve the optional level parameter
    cur := args;

    if cur.tok.kind = TOK_integer then
      level := cur.tok.value;
      cur := cur.succ;
    elsif cur.tok.kind = TOK_string and cur.tok.data(1) = '#' then
      level := integer'value(cur.tok.data(2 to cur.tok.data'high)) * (-1);
      cur := cur.succ;
    end if;

    -- Look up the referenced scope
    if level >= 0 then -- Level is relative to current scope
      i := level;
      outer_scope := VIO.scope;
      while i > 0 and outer_scope /= null loop
        outer_scope := outer_scope.prev;
        i := i - 1;
      end loop;
    else -- Level is relative to global scope
      i := level;
      outer_scope := VIO.scope_stack;
      while i < 0 and outer_scope /= null loop
        outer_scope := outer_scope.succ;
        i := i + 1;
      end loop;
    end if;

    assert_true(outer_scope /= null, "'upvar' Invalid scope level", failure, VIO);

    -- Link variables to outer scope
    while cur /= null loop
      assert_true(cur.succ /= null, "'upvar' Missing second variable name", failure, VIO);
      myvar := cur.succ;

      --report ">>>>>>>>>> upvar: " & cur.tok.data.all & " -> " & myvar.tok.data.all;

      -- "cur" is the othervar we're linking to from the outer scope
      -- Retrieve variable reference from outer scope
      get_variable(outer_scope, cur.tok.data.all, var);
      if var = null then -- Create new variable in outer scope
        new_vt_parse_node(vobj, VN_word);
        SU.initialize(vobj.tok.data);
        vobj.tok.kind := TOK_string;
        set_variable(outer_scope, cur.tok.data.all, vobj, false); -- Pass ownership to the var
        get_variable(outer_scope, cur.tok.data.all, var);
      end if;

      -- Create a linked variable in the current scope
      link_variable(VIO, myvar.tok.data.all, var);

      cur := myvar.succ;
    end loop;

    set_result(VIO);
  end procedure;


  procedure do_cmd_wait( VIO : inout vt_interp_acc; args : inout vt_parse_node_acc ) is
    variable tstr : unbounded_string;
    variable delay : time := 0 sec;
  begin
    assert_true(args.tok.kind = TOK_integer or args.tok.kind = TOK_float,
      "'wait' Expecting numeric value", failure, VIO);
    assert_true(args.succ /= null, "'wait' Missing units", failure, VIO);

    if args.tok.kind = TOK_integer then
      SU.copy(integer'image(args.tok.value) & " " & args.succ.tok.data.all, tstr);
      delay := time'value(tstr.all);
    elsif args.tok.kind = TOK_float then
      SU.copy(real'image(args.tok.float) & " " & args.succ.tok.data.all, tstr);
      delay := time'value(tstr.all);
      --report "@@@ delay: " & tstr.all & "  " & time'image(delay);
    end if;

    wait for delay;
    deallocate(tstr);
    set_result(VIO);
  end procedure;


  procedure do_cmd_while( VIO : inout vt_interp_acc; args : inout vt_parse_node_acc ) is
    variable count : natural;
    variable is_true : boolean;
    variable cmd_list : vt_parse_node_acc;
  begin

    expect_arg_count(args, 2, "while", "test body", VIO);

    eval_expr(VIO, args);
    expr_is_true(VIO.EI, is_true);
    if is_true then -- Continue looping
      if args.succ.kind /= VN_group then
        groupify(args.succ);
      end if;
      assert_true(args.succ.kind = VN_group, "Expecting group for code body", failure, VIO);
      copy_parse_tree(args.succ, cmd_list);
      group_to_script(VIO, cmd_list);

--      convert_to_commands(cmd_list);
--      assert_true(cmd_list /= null and cmd_list.kind = VN_cmd_list,
--        "Expecting command list in 'while'", failure, VIO);
      push_script(VIO.scope, MODE_LOOP, cmd_list);

    else -- Stop looping
      set_result(VIO);
      VIO.scope.script.script_state := MODE_NORMAL;
    end if;
  end procedure;


  procedure do_cmd_yield( VIO : inout vt_interp_acc; args : inout vt_parse_node_acc ) is
    variable cur : vt_parse_node_acc;
  begin

    cur := args;
    if cur /= null then
      set_result(VIO, cur);
      report "DBG: yield cmd: set return value";
    else -- Yield empty string
      set_result(VIO);
    end if;

  end procedure;

end package body;

