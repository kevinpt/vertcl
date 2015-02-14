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
--# Copyright � 2015 Kevin Thibedeau
--# (kevin 'period' thibedeau 'at' gmail 'punto' com)
--#
--# This source is released under the terms of the MIT license.
--# See the LICENSE file for more details.
-------------------------------------------------------------------

--  Commands:
--  ---------
--  ??after
--  append   DONE
--  ??array
--  binary
--  break    DONE
--  concat   DONE
--  continue DONE
--  error
--  eval
--  exit     DONE
--  expr     partial
--  for      DONE
--  foreach  partial
--  ??format
--  global   DONE
--  if       DONE
--  incr     DONE
--  ??info
--  join     DONE
--  lappend  DONE
--  lindex   DONE
--  linsert  DONE
--  list     DONE
--  llength  partial
--  lrange   DONE
--  lrepeat
--  lreplace
--  lreverse
--  lsearch
--  lset     DONE
--  ??lsort
--  ??package
--  ??parray
--  proc     DONE
--  puts     DONE
--  rename
--  return   partial
--  ??scan
--  set      DONE
--  source
--  split
--  string:
--    cat
--    compare
--    equal
--    first
--    index
--    last
--    length
--    map
--    range
--    repeat
--    replace
--    reverse
--    tolower
--    totitle
--    toupper
--    trim
--    trimleft
--    trimright
--  subst
--  switch
--  ??tailcall
--  unknown
--  unset
--  ??uplevel
--  upvar    DONE
--  while    DONE
--  yield


library vertcl;
use vertcl.vt_parser.all;
use vertcl.vt_interpreter_core.all;

package vt_commands is
  procedure do_cmd_append( VIO : inout vt_interp_acc; args : inout vt_parse_node_acc);
  procedure do_cmd_break( VIO : inout vt_interp_acc; args : inout vt_parse_node_acc);
  procedure do_cmd_concat( VIO : inout vt_interp_acc; args : inout vt_parse_node_acc);
  procedure do_cmd_continue( VIO : inout vt_interp_acc; args : inout vt_parse_node_acc);
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
  procedure do_cmd_lset( VIO : inout vt_interp_acc; args : inout vt_parse_node_acc );
  procedure do_cmd_proc( VIO : inout vt_interp_acc; args : inout vt_parse_node_acc );
  procedure do_cmd_puts( VIO : inout vt_interp_acc; args : inout vt_parse_node_acc );
  procedure do_cmd_return( VIO : inout vt_interp_acc; args : inout vt_parse_node_acc );
  procedure do_cmd_set(VIO : inout vt_interp_acc; args : inout vt_parse_node_acc);
  procedure do_cmd_upvar( VIO : inout vt_interp_acc; args : inout vt_parse_node_acc );
  procedure do_cmd_wait( VIO : inout vt_interp_acc; args : inout vt_parse_node_acc );
  procedure do_cmd_while( VIO : inout vt_interp_acc; args : inout vt_parse_node_acc );
  procedure do_cmd_yield( VIO : inout vt_interp_acc; args : inout vt_parse_node_acc );
end package;



library extras;
use extras.strings_unbounded.unbounded_string;
use extras.strings;

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
  procedure stringify( node : inout vt_parse_node_acc ) is
    variable img : unbounded_string;
  begin
    if node.kind = VN_group then
      to_unbounded_string(node.child, img);
    else -- Numeric value
      to_unbounded_string(node, img, false);
    end if;

    node.tok.data := img;
    node.tok.kind := TOK_string;
    node.kind := VN_word;
    free(node.child);
    node.child := null;
  end procedure;

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
    if vobj.tok.kind /= TOK_string then
      stringify(vobj);
    end if;

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
      pop_script(VIO.scope);
    end loop;

    if VIO.scope.script.script_state = MODE_LOOP then
      VIO.scope.script.script_state := MODE_NORMAL;
    else
      report "Invoked 'break' outside of a loop";
    end if;
  end procedure;


  procedure do_cmd_concat( VIO : inout vt_interp_acc; args : inout vt_parse_node_acc) is
    variable lobj, ltail, cur, prev, node_child : vt_parse_node_acc;
    variable was_true : boolean;
    variable img : unbounded_string;
  begin
    if args = null then -- Return empty string
      set_result(VIO);
      return;
    end if;

    -- FIXME: trim leading and trailing whitespace from strings

    if args.kind /= VN_group and args.tok.kind /= TOK_string then -- Convert first arg to a group
      new_vt_parse_node(lobj, args.kind);
      lobj.tok := args.tok;
      lobj.child := args.child;
      args.tok.kind := TOK_group_begin;
      args.tok.data := null;
      args.kind := VN_group;
      args.child := lobj;
    end if;

    if args.kind = VN_group then -- Concatenate to a group object
      lobj := args;

      ltail := lobj.child; -- Find the end of the first group list
      while ltail /= null loop
        if ltail.succ = null then
          exit;
        end if;
        ltail := ltail.succ;
      end loop;

      -- Make remaining args siblings following the list tail
      if ltail /= null then
        ltail.succ := args.succ;
      else -- First group was empty
        lobj.child := args.succ;
      end if;
      args.succ := null;

      -- Any list elements that are empty or whitespace strings need to be eliminated
      cur := lobj.child;
      prev := null;
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
      cur := lobj.child;
      while cur /= null loop
        if cur.tok.kind = TOK_string then
          contains_whitespace(cur.tok.data, was_true);
          if was_true then
            groupify(cur);
          end if;
        end if;
        cur := cur.succ;
      end loop;

      -- Any args that are groups need to have their top level group removed
      cur := lobj.child;
      prev := null;
      while cur /= null loop
        if cur.kind = VN_group then
          if cur.child /= null then -- Transfer first node to group node
            cur.kind := cur.child.kind;
            cur.tok := cur.child.tok;
            
            -- Find end of group list
            ltail := cur.child;
            while ltail /= null loop
              if ltail.succ = null then exit; end if;
              ltail := ltail.succ;
            end loop;

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

    else -- This shouldn't happen
      assert_true(false, "Unsupported argument to 'concat'", failure, VIO);
    end if;

    -- Disconnect the modified args list from its parent command so we can use
    -- it as the return value without copying
    VIO.scope.script.cur_cmd.child := null;
    set_result(VIO, lobj, false);
  end procedure;


  procedure do_cmd_continue( VIO : inout vt_interp_acc; args : inout vt_parse_node_acc) is
  begin
    -- Unwind the script stack until we find one just inside a loop
    while VIO.scope.script.prev /= null and VIO.scope.script.prev.script_state /= MODE_LOOP loop
      pop_script(VIO.scope);
    end loop;

    if VIO.scope.script.prev.script_state = MODE_LOOP then
      -- Skip remaining commands in this loop to effect the continue
      VIO.scope.script.cur_cmd := null;
    else
      report "Invoked 'continue' outside of a loop";
    end if;

  end procedure;


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
      pop_script(VIO.scope);
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
      convert_to_commands(cmd_list);
      assert_true(cmd_list /= null and cmd_list.kind = VN_cmd_list,
        "Expecting command list in 'for'", failure, VIO);
      push_script(VIO.scope, MODE_LOOP, cmd_list);

    else -- Stop looping
      set_result(VIO);
      VIO.scope.script.script_state := MODE_NORMAL;
      a_body.tok.value := 0;
    end if;

  end procedure;



  constant FOREACH_IX_OFFSET : integer := -100000;

  procedure do_cmd_foreach( VIO : inout vt_interp_acc; args : inout vt_parse_node_acc ) is
    variable a_list, a_body, next_val, cmd_list : vt_parse_node_acc;
    variable ix, llength : integer;
  begin
    assert_true(args /= null and args.tok.kind = TOK_string, "'foreach' expecting variable name", failure, VIO);
    
    -- FIXME: Handle multiple lists
    
    a_list := args.succ; -- FIXME: validate args exist
    a_body := a_list.succ;
    
    assert_true(a_list.kind = VN_group, "'foreach' expecting list", failure, VIO);
    arg_count(a_list.child, llength);
    
    if a_body.tok.value < 0 then -- resuming from previous iteration
      ix := a_body.tok.value - FOREACH_IX_OFFSET + 1;
    else
      ix := 0;
    end if;

    if ix = llength then -- Stop looping
      set_result(VIO);
      VIO.scope.script.script_state := MODE_NORMAL;
      a_body.tok.value := 0;
    else -- Execute next code body
      report "@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ FOREACH: " & integer'image(ix);

      get_sibling(a_list.child, ix, next_val);
      set_variable(VIO, args.tok.data.all, next_val, true, false);
    
    
      assert_true(a_body.kind = VN_group, "'foreach' Expecting group for code body", failure, VIO);
      copy_parse_tree(a_body, cmd_list);
      convert_to_commands(cmd_list);
      assert_true(cmd_list /= null and cmd_list.kind = VN_cmd_list,
        "Expecting command list in 'foreach'", failure, VIO);
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
        convert_to_commands(cmd_list);
        assert_true(cmd_list /= null and cmd_list.kind = VN_cmd_list,
          "Expecting command list in 'if'", failure, VIO);
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


          assert_true(cblock.kind = VN_group, "Expecting group for code body", failure, VIO);
          copy_parse_tree(cblock, cmd_list);
          convert_to_commands(cmd_list);
          assert_true(cmd_list /= null and cmd_list.kind = VN_cmd_list,
            "Expecting command list in 'if'", failure, VIO);
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
        cur := lobj.child;
        while cur /= null loop
          if cur.succ = null then -- At end of list
            cur.succ := append;
            exit;
          end if;
          cur := cur.succ;
        end loop;
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
        if cur.tok.kind = TOK_string then
          ix := parse_index(cur.tok.data.all, count);
        else
          ix := cur.tok.value;
        end if;
       
        assert_true(ix >= 0 and ix < count, "'lindex' index out of range: " & integer'image(ix), failure, VIO);
       
        -- Get nested list
        get_sibling(selected_list.child, ix, selected_list);
        
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
    if index_expr.tok.kind = TOK_string then
      index := parse_index(index_expr.tok.data.all, llength+1);
    else
      index := index_expr.tok.value;
    end if;
    
    -- Check bounds
    if index > llength then
      index := llength;
    elsif index < 0 then
      index := 0;   
    end if;
    
    -- Copy the list
    copy_parse_tree(args, new_list, false);
    
    if index_expr.succ /= null then -- There are elements to insert
      
      -- Find the node to insert after
      cur := new_list.child;
      i := 1;
      while cur /= null loop
        exit when i = index;
        i := i + 1;
        cur := cur.succ;
      end loop;
      insert_point := cur;

      if index > 0 then
        succ := insert_point.succ;
        insert_point.succ := index_expr.succ;
        
      else -- Start of list
        succ := new_list.child;
        new_list.child := index_expr.succ;
      end if;
      
      -- Find end of new elements
      cur := index_expr.succ;
      while cur /= null loop
        if cur.succ = null then
          cur.succ := succ;
          exit;      
        end if;
        cur := cur.succ;
      end loop;
      
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
      get_sibling(args.child, eix, cur);
      free(cur.succ);
      cur.succ := null;
      
      if six = 0 then -- Keep start of group
        start_node := args.child;
        args.child := null;
      else -- Disconnect preceding nodes
        get_sibling(args.child, six-1, cur);
        start_node := cur.succ;
        cur.succ := null;
      end if;
    end if;
    
    new_vt_parse_node(cur, VN_group);
    cur.child := start_node;

    set_result(VIO, cur, false);    
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
      get_sibling(args.succ, count-2, end_ix);
    end if;

    -- Disconnect the value node
    get_sibling(args.succ, count-2, cur); -- Next to last node
    value := cur.succ;
    cur.succ := null;
    
    -- Look up nested groups until the last one we're going to set
    selected_list := lobj;
    cur := start_ix;
    loop
      assert_true(selected_list.kind = VN_group, "'lset' Expecting a nested list object", failure, VIO);

      -- Compute next index      
      arg_count(selected_list.child, count);
      if cur.tok.kind = TOK_string then
        ix := parse_index(cur.tok.data.all, count);
      else
        ix := cur.tok.value;
      end if;
      
      exit when cur = end_ix;
      
      assert_true(ix >= 0 and ix < count, "'lset' index out of range: " & integer'image(ix), failure, VIO);
      
      -- Get nested list
      get_sibling(selected_list.child, ix, selected_list);
      
      cur := cur.succ;
    end loop;
    
    
    assert_true(ix >= 0 and ix <= count, "'lset' index out of range: " & integer'image(ix), failure, VIO);
    
    if ix = count then -- Append new value
      get_last(selected_list.child, cur);
      cur.succ := value;
    else -- Replace index
      get_sibling(selected_list.child, ix, cur);
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
    convert_to_commands(cbody);
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
      pop_script(VIO.scope);
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
    if level > 0 then -- Level is relative to current scope
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
      assert_true(args.succ.kind = VN_group, "Expecting group for code body", failure, VIO);
      copy_parse_tree(args.succ, cmd_list);
      convert_to_commands(cmd_list);
      assert_true(cmd_list /= null and cmd_list.kind = VN_cmd_list,
        "Expecting command list in 'while'", failure, VIO);
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

