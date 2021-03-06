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
--# Copyright � 2015 Kevin Thibedeau
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

  procedure initialize( VIO : inout vt_interp_acc );
  procedure load_tcl_script(VIO : inout vt_interp_acc; fname : in string);
  procedure interpret( VIO : inout vt_interp_acc );

  -- Alias some required types and procedures from vt_interpreter_core.
  -- This allows us to only require the use of the vt_interpreter package to use vertcl.
  
  alias vt_interp_acc is vertcl.vt_interpreter_core.vt_interp_acc;

  -- FIXME: Expand exported API calls
  alias get_variable is vertcl.vt_interpreter_core.get_variable[vt_interp_acc, string, scope_var_acc];
  alias get_variable is vertcl.vt_interpreter_core.get_variable[scope_obj_acc, string, scope_var_acc];
  alias free is vertcl.vt_interpreter_core.free[vt_interp_acc];
  
  -- Additional specialized implementations of get_variable() and set_variable()
  procedure get_variable( scope : inout scope_obj_acc; name : in string; val : out integer );  -- Get an integer
  procedure get_variable( VIO : inout vt_interp_acc; name : in string; val : out integer );


--  The standalone procedures for Vertcl can only be used within a single process.
--  If you need to access an interpreter from multiple processes it must be implemented as
--  a shared variable. The best approach is to use VHDL-93 style shared variables as they
--  impose no limiatations on the use of access types as parameters and the existing procedures
--  can be used. If VHDL-93 can't be used then a VHDL-2002 shared variable can be created using
--  the following potected type. There are significant limitations on the types that can be
--  returned from the get() method though.
  type vertcl_interpreter is protected
    procedure initialize;
    procedure load_tcl_script(fname : in string);
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

    procedure initialize is
    begin
      if VIO /= null then
        free(VIO);
      end if;
      VIO := new vt_interp;
      initialize(VIO);
    end procedure;

    procedure load_tcl_script(fname : in string) is
    begin
      load_tcl_script(VIO, fname);
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
    begin
      get_variable(VIO, name, rval);
      return rval;
    end function;

    impure function active return boolean is
    begin
      return VIO.active;
    end function;

  end protected body;



  procedure get_variable( scope : inout scope_obj_acc; name : in string; val : out integer ) is
    variable var : scope_var_acc;
  begin
    get_variable(scope, name, var);
    if var /= null then
      val := var.var.value.tok.value;
    end if;
  end procedure;

  procedure get_variable( VIO : inout vt_interp_acc; name : in string; val : out integer ) is
  begin
    get_variable(VIO.scope, name, val);
  end procedure;

-- PRIVATE procedures:
-- ===================


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

    assert_true(cmd.child.tok.kind = TOK_string,
      "Invalid command name. Must be a string: " & vt_token_kind'image(cmd.child.tok.kind),
      failure, VIO);

    get_command(VIO, cmd.child.tok.data.all, cmd_def);
    if cmd_def /= null then
      cmd_id := cmd_def.id;
    end if;

    args := cmd.child.succ;

    case cmd_id is
      when CMD_append   =>   do_cmd_append(VIO, args);
      when CMD_break    =>   do_cmd_break(VIO, args);
      when CMD_concat   =>   do_cmd_concat(VIO, args);
      when CMD_continue =>   do_cmd_continue(VIO, args);
      when CMD_eval     =>   do_cmd_eval(VIO, args);
      when CMD_exit     =>   do_cmd_exit(VIO, args);
      when CMD_expr     =>   do_cmd_expr(VIO, args);
      when CMD_for      =>   do_cmd_for(VIO, args);
      when CMD_foreach  =>   do_cmd_foreach(VIO, args);
      when CMD_global   =>   do_cmd_global(VIO, args);
      when CMD_if       =>   do_cmd_if(VIO, args);
      when CMD_incr     =>   do_cmd_incr(VIO, args);
      when CMD_join     =>   do_cmd_join(VIO, args);
      when CMD_lappend  =>   do_cmd_lappend(VIO, args);
      when CMD_lassign  =>   do_cmd_lassign(VIO, args);
      when CMD_lindex   =>   do_cmd_lindex(VIO, args);
      when CMD_linsert  =>   do_cmd_linsert(VIO, args);
      when CMD_list     =>   do_cmd_list(VIO, args);
      when CMD_llength  =>   do_cmd_llength(VIO, args);
      when CMD_lset     =>   do_cmd_lset(VIO, args);
      when CMD_lrange   =>   do_cmd_lrange(VIO, args);
      when CMD_lrepeat  =>   do_cmd_lrepeat(VIO, args);
      when CMD_lreplace =>   do_cmd_lreplace(VIO, args);
      when CMD_lreverse =>   do_cmd_lreverse(VIO, args);
      when CMD_proc     =>   do_cmd_proc(VIO, args);
      when CMD_puts     =>   do_cmd_puts(VIO, args);
      when CMD_rename   =>   do_cmd_rename(VIO, args);
      when CMD_return   =>   do_cmd_return(VIO, args);
      when CMD_set      =>   do_cmd_set(VIO, args);
      when CMD_source   =>   do_cmd_source(VIO, args);
      when CMD_split    =>   do_cmd_split(VIO, args);
      when CMD_string   =>   do_cmd_string(VIO, args);
      when CMD_subst    =>   do_cmd_subst(VIO, args);
      when CMD_unknown  =>   do_cmd_unknown(VIO, args);
      when CMD_unset    =>   do_cmd_unset(VIO, args);
      when CMD_uplevel  =>   do_cmd_uplevel(VIO, args);
      when CMD_upvar    =>   do_cmd_upvar(VIO, args);
      when CMD_wait     =>   do_cmd_wait(VIO, args);
      when CMD_while    =>   do_cmd_while(VIO, args);
      when CMD_yield    =>   do_cmd_yield(VIO, args);
      when CMD_proc_def =>   exec_proc(VIO, args, cmd_def);
      when others       =>
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

    if cur_arg = null and cur_cmd /= null then -- All arguments were expanded and we need a new command
      -- Determine the next command in the current script
      if cur_cmd.kind = VN_cmd_list then -- Start of script
        cur_cmd := cur_cmd.child;
      else
        cur_cmd := cur_cmd.succ;
      end if;

      VIO.scope.script.cur_cmd := cur_cmd;
      if cur_cmd /= null then
        cur_arg := cur_cmd.child;
        VIO.scope.script.cur_arg := cur_arg;
      end if;
    end if;


    if cur_cmd = null then -- Script is done
      -- If this is not the top level script for the current scope we
      -- need to complete it and possibly substitute the return value into the
      -- calling script.
      while VIO.scope.script /= VIO.scope.script_stack loop
        complete_script;
        exit when cur_cmd /= null;
      end loop;

    end if;


    if cur_cmd = null then -- Scope is done

      -- If this is not the top level script for the current scope we
      -- need to replace the active argument of the previous script with
      -- the return value of the last command.
--      report "                           >>>>>>>>>> TOP LEVEL ENDED " & boolean'image(VIO.scope = VIO.scope_stack);
      -- If this is not the top-level scope then we are in a proc that ended
      unwind: while VIO.scope /= VIO.scope_stack loop
        pop_scope(VIO); -- Return to calling scope
        cur_cmd := VIO.scope.script.cur_cmd.succ;

        exit unwind when cur_cmd /= null; -- This scope has more commands to process
        -- The script ended
        while VIO.scope.script /= VIO.scope.script_stack loop
          complete_script;
          exit unwind when cur_cmd /= null;
        end loop;

      end loop;

      if cur_cmd = null then
        -- We unwound the full call stack and have no more commands to process
        VIO.scope.script.cur_cmd := null;
        return;
      end if;

      cur_arg := cur_cmd.child;
    end if;

    VIO.scope.script.cur_cmd := cur_cmd;
    VIO.scope.script.cur_arg := cur_arg;


    if VIO.scope.script.permit_subst then -- FIXME: review looping commands to see if they need to clear this flag
      substitute(VIO, cur_arg); -- Perform variable and backslash substitutions before any command substs
      -- Prevent additional substitutions until this command is done
      VIO.scope.script.permit_subst := false;
    end if;


--    report ">>>>>>>>>>>>>>>>>>>>> GOT COMMAND: " & " line: " & integer'image(cur_cmd.child.tok.value);

    -- Check if any arguments require command substitution
    -- If so then push a new script so that they evaluate first
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
--          report "************************* SPLAT ***********************************";
          if cur_arg.child.kind = VN_cmd_list then -- Splatting a command substitution
--            report ">>>>>>>>>>>>>>>>>>>>>>>> SPLAT CMD SUBST <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<";
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
--            report "SUBSTITUTE IN PREP NEXT CMD SPLAT";
            substitute(VIO, cur_arg.child); -- FIXME: test with permit_subst?
            if cur_arg.child.kind /= VN_group then -- Didn't become group after var substitution
              groupify(cur_arg.child);
              -- FIXME: In Tcl embedded $varname elements are not expanded further by wrapping them in {}
              -- set x {a b $y}; puts [list {*}$x]  --> a b {$y}
            end if;
          end if;
          
          assert_true(cur_arg.child.kind = VN_group, "Expecting group in splat expansion", failure, VIO);
          
          -- Insert group elements into argument list
          -- FIXME: use splicer
          report "$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ SPLAT SPLICE ";
          splat_terms := cur_arg.child.child;
          cur_arg.child.child := null;
          splice_parse_tree(splat_terms, cur_arg);
          ---------------------------
--          group_node := cur_arg.child;
--          splat_terms := group_node.child;
--          
--          cur_arg.tok := splat_terms.tok;     -- Copy first term into current arg
--          cur_arg.kind := splat_terms.kind;
--          cur_arg.child := splat_terms.child;
--          splat_terms.tok.data := null;
--          splat_terms.child := null;
--          
--          spl_succ := cur_arg.succ; -- Save next argument
--          
--          cur_arg.succ := splat_terms.succ;

--          -- Find last element in splatted group
--          spl_cur := cur_arg;
--          while spl_cur /= null loop
--            if spl_cur.succ = null then
--              spl_cur.succ := spl_succ; -- Reconnect with following arguments
--              exit;
--            end if;
--            spl_cur := spl_cur.succ;
--          end loop;
--          
--          -- Release unneeded group node and its first splat term
--          splat_terms.succ := null;
--          free(group_node);

          
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

  procedure initialize( VIO : inout vt_interp_acc ) is
  begin
    if VIO = null then
      VIO := new vt_interp;
    end if;
    
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


  procedure load_tcl_script(VIO : inout vt_interp_acc; fname : in string) is
    variable parse_tree : vt_parse_node_acc;
  begin
    if VIO = null then
      initialize(VIO);
    end if;
  
    parse_vertcl_file(fname, parse_tree);
    push_script(VIO.scope, MODE_NORMAL, parse_tree);
  end procedure;


  procedure interpret( VIO : inout vt_interp_acc ) is
    variable cmd : vt_parse_node_acc;
  begin

    report "                            ################## START INTERP";

    loop
      prepare_next_cmd(VIO);
      cmd := VIO.scope.script.cur_cmd;
      VIO.scope.script.permit_subst := true;

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
