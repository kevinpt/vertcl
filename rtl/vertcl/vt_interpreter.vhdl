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
--# vt_interpreter.vhdl - Top level interpreter for VerTcl
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
use vertcl.vt_parser.all;
use vertcl.vt_interpreter_core.all;

package vt_interpreter is

  procedure vt_interp_init( VIO : inout vt_interp_acc );
  procedure interpret( VIO : inout vt_interp_acc );

  -- FIXME: Expand exported API calls
  alias get_variable is vertcl.vt_interpreter_core.get_variable[vt_interp_acc, string, scope_var_acc];
  alias get_variable is vertcl.vt_interpreter_core.get_variable[scope_obj_acc, string, scope_var_acc];


  type vertcl_interpreter is protected
    procedure init;
    procedure load_file(fname : in string);
    procedure run;
    procedure terminate;

    impure function get(name : string) return integer;
    impure function active return boolean;
  end protected;
end package;



library ieee;
use ieee.math_real.all;

library extras;
use extras.strings_unbounded.unbounded_string;

library vertcl;
use vertcl.vt_lexer.all;
use vertcl.vt_parser.all;
use vertcl.vt_interpreter_core.all;
use vertcl.vt_commands.all;
use vertcl.vt_expr_interpreter.all;


package body vt_interpreter is

  type vertcl_interpreter is protected body
    variable VIO : vt_interp_acc;

    procedure init is
    begin
      if VIO /= null then
        free(VIO);
      end if;
      VIO := new vt_interp;
      vt_interp_init(VIO);
    end procedure;

    procedure load_file(fname : in string) is
      variable parse_tree : vt_parse_node_acc;
    begin
      parse_vertcl_file(fname, parse_tree);
      push_script(VIO.scope, MODE_NORMAL, parse_tree);

      write_parse_tree("parse_tree.txt", parse_tree); -- FIXME: remove this
    end procedure;

    procedure run is
    begin
      interpret(VIO);
    end procedure;

    procedure terminate is
    begin
      free(VIO);
    end procedure;

    impure function get(name : string) return integer is
      variable rval : integer;
      variable var : scope_var_acc;
    begin
      --get_variable(VIO, name, rval); -- FIXME: have an integer version of get_variable
      get_variable(VIO, name, var);
      if var /= null then
        rval := var.var.value.tok.value;
      end if;

      return rval;
    end function;

    impure function active return boolean is
    begin
      return VIO.active;
    end function;

  end protected body;


-- PRIVATE procedures:
-- ===================


  procedure substitute_plain_variable( VIO : inout vt_interp_acc; node : inout vt_parse_node_acc;
    was_plain_var : out boolean) is

    variable ch : character;
    variable var_name : unbounded_string;
    variable var : scope_var_acc;
    variable var_value, cur_node, next_node : vt_parse_node_acc;
  begin

    if node.tok.data.all'length < 2 or 
      (node.tok.data.all'length > 1 and node.tok.data(node.tok.data'left) /= '$') then -- This is not a variable
      return;
    end if;
    
    if node.child /= null then
      return;
    end if;


    SU.slice(node.tok.data, 2, node.tok.data.all'high, var_name);

    --report "#### sub var: " & var_name.all;

    get_variable(VIO, var_name.all, var);

    if var /= null then
      -- Convert node into variable parse tree
      copy_parse_tree(var.var.value, var_value);

      --report  "DBG: subst copied tree: " & integer'image(var_value.id);

      -- Copy first value node into identifier node
      free(node.tok);
      node.tok := var_value.tok;
      node.kind := var_value.kind;

      -- Not possible for identifiers to have children. Safe to overwrite.
      node.child := var_value.child;
      node.last_child := var_value.last_child;

      -- FIXME: This "if" can be removed? (pull assignment to cur_node.succ out of loop)
      if var_value.succ /= null then -- Reconnect the node siblings
        next_node := node.succ;
        node.succ := var_value.succ;

        -- Look for end of siblings
        cur_node := var_value.succ; -- FIXME: start from node like below in subst_cmds()?
        while cur_node /= null loop
          if cur_node.succ = null then -- Found end
            cur_node.succ := next_node;
            exit;
          end if;
          cur_node := cur_node.succ;
        end loop;
      end if;

      -- Discard the first node from var_value now that it is redundant
      var_value.tok.data := null;
      var_value.succ := null;
      var_value.child := null;
      free(var_value);

    else
      assert_true(false, "Unknown variable '" & var_name.all & "'", warning, VIO);
    end if;

    deallocate(var_name);
    was_plain_var := true;

  end procedure;




  procedure substitute( VIO  : inout vt_interp_acc; parse_tree : inout vt_parse_node_acc) is
    variable cur_node : vt_parse_node_acc;
    variable was_plain_var : boolean;
  begin

    cur_node := parse_tree;

    while cur_node /= null loop
      if cur_node.tok.kind = TOK_string then
        -- Substitute variables and escape chars in strings
        -- If a string consists solely of a variable name then we want to
        -- substitute with its parsed object form. Otherwise everything becomes a string.
        substitute_plain_variable(VIO, cur_node, was_plain_var);
        
        if not was_plain_var then
          substitute_in_string(VIO, cur_node.tok.data);
        end if;

      end if;
      cur_node := cur_node.succ;
    end loop;

  end procedure;


  procedure exec_proc(VIO : inout vt_interp_acc; args : inout vt_parse_node_acc;
    cmd_def : inout command_info_acc) is
    variable arg, arg_def, next_arg, cbody : vt_parse_node_acc;
  begin
    report "### Executing proc: " & cmd_def.name.all;

    -- Set a new scope
    push_scope(VIO);

    -- Convert arguments into local variables
    arg := args;
    arg_def := cmd_def.arg_defs;
    while arg /= null and arg_def /= null loop
      if arg_def.tok.data.all = "args" then -- Append remaining arguments as a group

        -- Copy current arg into new next_arg
        new_vt_parse_node(next_arg, arg.kind);
        next_arg.tok := arg.tok;
        next_arg.succ := arg.succ;
        next_arg.child := arg.child;

        -- Convert arg into a group node with the remaining arguments as children
        arg.tok.data := null;
        arg.tok.kind := TOK_UNKNOWN;
        arg.child := next_arg;
        arg.succ := null;
        arg.kind := VN_group;
      end if;

      set_variable(VIO, arg_def.tok.data.all, arg, true, false); -- Copy just this arg into variable

      arg := arg.succ;
      arg_def := arg_def.succ;
    end loop;

    while arg_def /= null loop -- Additional args remaining that need a default value
      assert_true(arg_def.child /= null,
        "Missing argument with no default value: '" & arg_def.tok.data.all & "'", failure, VIO);

      set_variable(VIO, arg_def.tok.data.all, arg_def.child);
      arg_def := arg_def.succ;
    end loop;

    -- Create a new script with the proc code block
    copy_parse_tree(cmd_def.cbody, cbody);
    push_script(VIO.scope, MODE_NORMAL, cbody);

  end procedure;



--  procedure get_command( VIO : inout vt_interp_acc; name : in string; cmd : inout command_info_acc ) is
--    variable cur_cmd : command_info_acc;

--    constant h : natural := SF.hash(name) mod VIO.commands'length;
--  begin
--    -- Lookup the command ID
--    cur_cmd := VIO.commands(h);
--    while cur_cmd /= null loop
--      exit when cur_cmd.name.all = name;
--      cur_cmd := cur_cmd.succ;
--    end loop;

--    cmd := cur_cmd;
--  end procedure;
  
  
--  procedure handle_unknown( VIO : inout vt_interp_acc; cmd : inout vt_parse_node_acc ) is
--  begin
--    assert_true(false, "Unknown command: " & cmd.child.tok.data.all, error, VIO);
--  end procedure;


  procedure exec_command( VIO : inout vt_interp_acc; cmd : inout vt_parse_node_acc ) is

    variable args : vt_parse_node_acc;
    variable cmd_def : command_info_acc;
    variable cmd_id : command_id := CMD_UNDETERMINED;
  begin

    -- Perform variable substitutions on command elements
    substitute(VIO, cmd.child);

    assert_true(cmd.child.tok.kind = TOK_string,
      "Invalid command name. Must be a string: " & vt_token_kind'image(cmd.child.tok.kind),
      failure, VIO);


    get_command(VIO, cmd.child.tok.data.all, cmd_def);
    if cmd_def /= null then
      cmd_id := cmd_def.id;
    end if;

    args := cmd.child.succ;

    case cmd_id is
      when CMD_append =>        do_cmd_append(VIO, args);
      when CMD_break =>         do_cmd_break(VIO, args);
      when CMD_concat =>        do_cmd_concat(VIO, args);
      when CMD_continue =>      do_cmd_continue(VIO, args);
      when CMD_exit =>          do_cmd_exit(VIO, args);
      when CMD_expr =>          do_cmd_expr(VIO, args);
      when CMD_for =>           do_cmd_for(VIO, args);
      when CMD_foreach =>       do_cmd_foreach(VIO, args);
      when CMD_global =>        do_cmd_global(VIO, args);
      when CMD_if =>            do_cmd_if(VIO, args);
      when CMD_incr =>          do_cmd_incr(VIO, args);
      when CMD_join =>          do_cmd_join(VIO, args);
      when CMD_lappend =>       do_cmd_lappend(VIO, args);
      when CMD_lindex =>        do_cmd_lindex(VIO, args);
      when CMD_linsert =>       do_cmd_linsert(VIO, args);
      when CMD_list =>          do_cmd_list(VIO, args);
      when CMD_llength =>       do_cmd_llength(VIO, args);
      when CMD_lset =>          do_cmd_lset(VIO, args);
      when CMD_lrange =>        do_cmd_lrange(VIO, args);
      when CMD_proc =>          do_cmd_proc(VIO, args);
      when CMD_puts =>          do_cmd_puts(VIO, args);
      when CMD_rename =>        do_cmd_rename(VIO, args);
      when CMD_return =>        do_cmd_return(VIO, args);
      when CMD_set =>           do_cmd_set(VIO, args);
      when CMD_string =>        do_cmd_string(VIO, args);
      when CMD_unknown =>       do_cmd_unknown(VIO, args);
      when CMD_unset =>         do_cmd_unset(VIO, args);
      when CMD_uplevel =>       do_cmd_uplevel(VIO, args);
      when CMD_upvar =>         do_cmd_upvar(VIO, args);
      when CMD_wait =>          do_cmd_wait(VIO, args);
      when CMD_while =>         do_cmd_while(VIO, args);
      when CMD_yield =>         do_cmd_yield(VIO, args);
      when CMD_proc_def =>      exec_proc(VIO, args, cmd_def);
      when others =>
        -- Lookup unknown command definition
        get_command(VIO, "unknown", cmd_def);
        if cmd_def.id = CMD_proc_def then
          exec_proc(VIO, cmd.child, cmd_def);
        else
          do_cmd_unknown(VIO, cmd.child);
        end if;
        
    end case;

    report ">>>>>>>>> DBG: did command: " & command_id'image(cmd_id);
  end procedure;



  procedure prepare_next_cmd( VIO : inout vt_interp_acc ) is
    variable cur_cmd, cur_arg, cur_seg, new_script : vt_parse_node_acc;
    variable group_node, splat_terms, spl_succ, spl_cur : vt_parse_node_acc;
    variable seg_str : unbounded_string;

    procedure complete_script is
    begin

      pop_script(VIO);
      cur_cmd := VIO.scope.script.cur_cmd;
      cur_arg := VIO.scope.script.cur_arg;

      case VIO.scope.script.script_state is
        when MODE_NORMAL => -- Advance to next command
          cur_cmd := cur_cmd.succ;
          cur_arg := null;

        when MODE_LOOP => -- Stay on current command until loop terminates
          null;

        when MODE_SUBST => -- Complete substitution
          VIO.scope.script.script_state := MODE_NORMAL;

          if VIO.result.value /= null then
            if VIO.result.is_ref then -- We need to copy the return value from its reference
              report ">>>>>>>>>>>>>>>> COPYING RETURN REFERENCE <<<<<<<<<<<<<<<<<<<<<";
              copy_parse_tree(VIO.result.value, new_script);
              VIO.result.value := new_script;
              VIO.result.is_ref := false;
            end if;

            if cur_arg.kind /= VN_string_seg and cur_arg.kind /= VN_splat then
              splice_parse_tree(VIO.result.value, cur_arg);
            else -- Substituting in segmented string or splat
              report ">>>>>>>>>>>>>>>>>>>>>> SPLICE SPLAT OR SEG STR <<<<<<<<<<<<<<<<<<<<<<<";
              splice_parse_tree(VIO.result.value, VIO.scope.script.cur_seg);
            end if;
            VIO.result.value := null;

          else -- Replace with empty string
            if cur_arg.kind /= VN_string_seg then
              cur_arg.kind := VN_word;
              cur_arg.tok.kind := TOK_string;
              deallocate(cur_arg.tok.data); -- FIXME: is this needed?
              SU.initialize(cur_arg.tok.data);

              free(cur_arg.child);
              cur_arg.child := null;
            else
              cur_seg := VIO.scope.script.cur_seg;
              cur_seg.kind := VN_word;
              cur_seg.tok.kind := TOK_string;
              deallocate(cur_seg.tok.data); -- FIXME: is this needed?
              SU.initialize(cur_seg.tok.data);

              free(cur_seg.child);
              cur_seg.child := null;

            end if;

          end if;
      end case;

    end procedure;

  begin
    cur_cmd := VIO.scope.script.cur_cmd;
    cur_arg := VIO.scope.script.cur_arg;

    if cur_arg = null then -- All arguments were expanded and we need a new command
      --report "                   $$$$$$$$$$$$$$$$$$$$$$$ NEED NEW COMMAND";
      -- Determine the next command in the current script
      if cur_cmd = null then -- Script is done
        --report "                 $$$$$$$$$$$$$$$$$$$$$$$$ SCRIPT IS DONE";
        -- FIXME: put this test into outer if statement
      elsif cur_cmd.kind = VN_cmd_list then -- Start of script
        --report "                          $$$$$$$$$$$$$$ CHOSE CHILD:" & integer'image(cur_cmd.tok.value);
        cur_cmd := cur_cmd.child;
      else
        --report "                          $$$$$$$$$$$$$$ CHOSE SUCC: " & cur_cmd.tok.data.all;
        cur_cmd := cur_cmd.succ;
        --if cur_cmd /= null then
        --report "                          $$$$$$$$$$$$$$ SUCC: " & cur_cmd.tok.data.all;
        --end if;
      end if;

      VIO.scope.script.cur_cmd := cur_cmd;
      if cur_cmd /= null then
        cur_arg := cur_cmd.child;
        VIO.scope.script.cur_arg := cur_arg;
      end if;
    end if;


    if cur_cmd = null then -- Script is done
      -- If this is not the top level script for the current scope we
      -- need complete it and possibly substitute the return value into the
      -- calling script.
      while VIO.scope.script /= VIO.scope.script_stack loop
        report "                  >>>>>>>>> UNWIND SCRIPT 1";
        complete_script;

        if cur_cmd /= null then -- FIXME exit when
          report "               >>>>>>>> UNWIND 1 FOUND CMD ";
          exit;
        end if;
      end loop;

--      VIO.scope.script.cur_cmd := cur_cmd;
--      VIO.scope.script.cur_arg := cur_arg;

    end if;


    if cur_cmd = null then -- Scope is done

      -- If this is not the top level script for the current scope we
      -- need to replace the active argument of the previous script with
      -- the return value of the last command.
--      if VIO.scope.script.prev /= null then
--        complete_subst;
--        -- FIXME: handle null cur_cmd
--      else -- Top-level script ended
        report "                           >>>>>>>>>> TOP LEVEL ENDED " & boolean'image(VIO.scope = VIO.scope_stack);
        -- If this is not the top-level scope then we are in a proc that ended
        unwind: while VIO.scope /= VIO.scope_stack loop
          pop_scope(VIO); -- Return to calling scope
          cur_cmd := VIO.scope.script.cur_cmd.succ;

--          if cur_cmd /= null then -- This scope has more commands to process
--            exit unwind;
--          else -- The script ended
--            while VIO.scope.script /= VIO.scope.script_stack loop
--              report "                  >>>>>>>>> UNWIND SCRIPT 2";
--              complete_script;

--              if cur_cmd /= null then
--                report "               >>>>>>>> UNWIND 2 FOUND CMD " & cur_cmd.tok.data.all;
--                exit unwind;
--              end if;
--            end loop;

--          end if;
          exit unwind when cur_cmd /= null; -- This scope has more commands to process
          -- The script ended
          while VIO.scope.script /= VIO.scope.script_stack loop
            report "                  >>>>>>>>> UNWIND SCRIPT 2";
            complete_script;

            if cur_cmd /= null then -- FIXME: exit when
              report "               >>>>>>>> UNWIND 2 FOUND CMD " & cur_cmd.tok.data.all;
              exit unwind;
            end if;
          end loop;

        end loop;

        if cur_cmd = null then
          -- We unwound the full call stack and have no more commands to process
          VIO.scope.script.cur_cmd := null;
          return;
        end if;

        --cur_arg := cur_cmd.child;
--      end if;

    end if;

    VIO.scope.script.cur_cmd := cur_cmd;
    VIO.scope.script.cur_arg := cur_arg;


    --report ">>>>>>>>>>>>>>>>>>>>> GOT COMMAND: " & cur_cmd.tok.data.all & " line: " & integer'image(cur_cmd.tok.value);
    report ">>>>>>>>>>>>>>>>>>>>> GOT COMMAND: " & " line: " & integer'image(cur_cmd.child.tok.value);

    -- Check if any arguments require command substitution
    -- If so then push a new script so that they evaluate first
    --cur_arg := cur_cmd.child;
    while cur_arg /= null loop
      case cur_arg.kind is
        when VN_cmd_list =>
          VIO.scope.script.cur_arg := cur_arg; -- Save our current position in this script

          -- Establish a new script
          copy_parse_tree(cur_arg, new_script);
          push_script(VIO.scope, MODE_SUBST, new_script);
          
          prepare_next_cmd(VIO);
          return;
        
        when VN_string_seg =>  -- Apply all command substitutions to the string
          cur_seg := cur_arg.child;
          while cur_seg /= null loop
            if cur_seg.kind = VN_cmd_list then
              VIO.scope.script.cur_arg := cur_arg; -- Save our current position in this script
              VIO.scope.script.cur_seg := cur_seg;

              -- Establish a new script
              copy_parse_tree(cur_seg, new_script);
              push_script(VIO.scope, MODE_SUBST, new_script);
              
              prepare_next_cmd(VIO);
              return;
            end if;
            cur_seg := cur_seg.succ;
          end loop;

          -- If we reach this point then the command substitutions in the string are complete

          -- Concatenate segments into final result string
          cur_seg := cur_arg.child;
          while cur_seg /= null loop
            case cur_seg.tok.kind is
              when TOK_string =>
                SU.append(cur_arg.tok.data, cur_seg.tok.data);

              when others =>
                if cur_seg.kind = VN_group then
                  to_unbounded_string(cur_seg.child, seg_str);
                else
                  to_unbounded_string(cur_seg, seg_str, false);
                end if;
                SU.append(cur_arg.tok.data, seg_str);
                deallocate(seg_str);
            end case;

            cur_seg := cur_seg.succ;
          end loop;

          cur_arg.tok.kind := TOK_string;
          cur_arg.kind := VN_word;
          free(cur_arg.child);
          cur_arg.child := null;


        when VN_splat =>
          report "************************* SPLAT ***********************************";
          if cur_arg.child.kind = VN_cmd_list then -- Splatting a command substitution
            report ">>>>>>>>>>>>>>>>>>>>>>>> SPLAT CMD SUBST <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<";
            VIO.scope.script.cur_arg := cur_arg; -- Save our current position in this script
            VIO.scope.script.cur_seg := cur_arg.child;

            -- Establish a new script
            copy_parse_tree(cur_arg.child, new_script);
            push_script(VIO.scope, MODE_SUBST, new_script);

            prepare_next_cmd(VIO);
            return;
          end if;
          
          -- If we reach this point command substitutions in the splat are complete
          
          if cur_arg.child.kind /= VN_group then -- We need to convert to a group
            -- Substitute variables
            substitute(VIO, cur_arg.child);
            if cur_arg.child.kind /= VN_group then -- Didn't become group after var substitution
              groupify(cur_arg.child);
              -- FIXME: In Tcl embedded $varname elements are not expanded further by wrapping them in {}
              -- set x {a b $y}; puts [list {*}$x]  --> a b {$y}
            end if;
          end if;
          
          assert_true(cur_arg.child.kind = VN_group, "Expecting group in splat expansion", failure, VIO);
          
          -- Insert group elements into argument list
          -- FIXME: use splicer
          group_node := cur_arg.child;
          splat_terms := group_node.child;
          
          cur_arg.tok := splat_terms.tok;     -- Copy first term into current arg
          cur_arg.kind := splat_terms.kind;
          cur_arg.child := splat_terms.child;
          splat_terms.tok.data := null;
          splat_terms.child := null;
          
          spl_succ := cur_arg.succ; -- Save next argument
          
          cur_arg.succ := splat_terms.succ;

          -- Find last element in splatted group
          spl_cur := cur_arg;
          while spl_cur /= null loop
            if spl_cur.succ = null then
              spl_cur.succ := spl_succ; -- Reconnect with following arguments
              exit;
            end if;
            spl_cur := spl_cur.succ;
          end loop;
          
          -- Release unneeded group node and its first splat term
          splat_terms.succ := null;
          free(group_node);

          
        when others =>
          null;
      end case;

      cur_arg := cur_arg.succ; -- Next arg
    end loop;

    if cur_arg = null then -- No more command expansions
      VIO.scope.script.cur_arg := cur_arg;
    end if;

  end procedure;



-- PUBLIC procedures:
-- ==================

  procedure vt_interp_init( VIO : inout vt_interp_acc ) is
    --variable cur_cmd : command_info_acc;
  begin
    VIO.active := true;

    -- Create top-level scope
    VIO.scope_stack := new scope_obj;
    VIO.scope := VIO.scope_stack;
    
    VIO.recursion_limit := 1000;

    VIO.result.value := null;
    VIO.result.is_ref := false;
    VIO.exit_code := 0;

    -- Build table of internal commands
    for id in builtin_commands loop
      -- Strip "CMD_" prefix from id image for use as the Tcl-visible command name
      def_command(VIO, SF.delete(command_id'image(id),1,4), id);
    end loop;

    -- Build table of command ensemble subcommands
    for id in ensemble_commands loop
      -- Strip "ENS_" prefix from id image for use as the Tcl-visible subcommand name
      def_ensemble(VIO, SF.delete(ensemble_id'image(id),1,4), id);
    end loop;

    expr_interp_init(VIO.EI);
  end procedure;


  procedure interpret( VIO : inout vt_interp_acc ) is
    variable cmd : vt_parse_node_acc;
  begin

    report "                            ################## START INTERP";

    loop
      prepare_next_cmd(VIO);
      cmd := VIO.scope.script.cur_cmd;

      if cmd /= null and cmd.kind = VN_command then
        exec_command(VIO, cmd);

        if cmd.child.tok.data.all = "yield" then -- Suspend interpreter
          return;
        end if;

      else -- Not a valid command or no commands left
        VIO.active := false;
        return;
      end if;

    end loop;
  end procedure;


end package body;
