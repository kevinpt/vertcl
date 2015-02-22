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
--# vt_interpreter_core.vhdl - Interpreter core operations
--# Freely available from http://code.google.com/p/vertcl
--#
--# Copyright © 2015 Kevin Thibedeau
--# (kevin 'period' thibedeau 'at' gmail 'punto' com)
--#
--# This source is released under the terms of the MIT license.
--# See the LICENSE file for more details.
-------------------------------------------------------------------

-- This package is separated out because of issues when attempting to
-- isolate the do_cmd_* procedures into their own package in vt_commands.vhdl.
-- Modelsim falsely claims circular dependencies among packages that refer to
-- each other only in their bodies.

library extras;
use extras.strings_unbounded.all;

library vertcl;
use vertcl.vt_parser.all;
use vertcl.vt_expr_interpreter.all;

package vt_interpreter_core is

  type variable_rec;
  type variable_rec_acc is access variable_rec;
  type variable_rec is record
    value     : vt_parse_node_acc;
    ref_count : natural;
  end record;

  type scope_var;
  type scope_var_acc is access scope_var;
  type scope_var is record
    name  : unbounded_string;
    var  : variable_rec_acc;
    succ : scope_var_acc;
  end record;

  type variable_map is array (natural range <>) of scope_var_acc;


  type command_id is (
    CMD_UNDETERMINED,
    CMD_proc_def,

    CMD_append,
    CMD_break,
    CMD_concat,
    CMD_continue,
    CMD_exit,
    CMD_expr,
    CMD_for,
    CMD_foreach,
    CMD_global,
    CMd_if,
    CMD_incr,
    CMD_join,
    CMD_lappend,
    CMD_lindex,
    CMD_linsert,
    CMD_list,
    CMD_llength,
    CMD_lrange,
    CMD_lrepeat,
    CMD_lreplace,
    CMD_lreverse,
    CMD_lset,
    CMD_proc,
    CMD_puts,
    CMD_rename,
    CMD_return,
    CMD_set,
    CMD_string,
    CMD_unknown,
    CMD_unset,
    CMD_uplevel,
    CMD_upvar,
    CMD_wait,
    CMD_while,
    CMD_yield
  );

  subtype builtin_commands is command_id range CMD_append to CMD_yield;

  type command_info;
  type command_info_acc is access command_info;
  type command_info is record
    name     : unbounded_string;
    id       : command_id;
    arg_defs : vt_parse_node_acc;
    cbody    : vt_parse_node_acc;
    succ     : command_info_acc;
  end record;

  type command_map is array (natural range <>) of command_info_acc;

  type ensemble_id is (
    ENS_UNKNOWN,
    ENS_cat,
    ENS_compare,
    ENS_equal,
    ENS_first,
    ENS_index,
    ENS_last,
    ENS_length,
    ENS_map,
    ENS_range,
    ENS_repeat,
    ENS_replace,
    ENS_reverse,
    ENS_tolower,
    ENS_totitle,
    ENS_toupper,
    ENS_trim,
    ENS_trimleft,
    ENS_trimright
  );
  
  subtype ensemble_commands is ensemble_id range ensemble_id'val(1) to ensemble_id'high;

  type ensemble_info;
  type ensemble_info_acc is access ensemble_info;
  type ensemble_info is record
    name : unbounded_string;
    id   : ensemble_id;
    succ : ensemble_info_acc;
  end record;

  type ensemble_map is array (natural range <>) of ensemble_info_acc;
  

  type script_mode is (MODE_NORMAL, MODE_LOOP, MODE_SUBST);

  type script_obj;
  type script_obj_acc is access script_obj;
  type script_obj is record
    parse_tree : vt_parse_node_acc;
    cur_cmd : vt_parse_node_acc;
    cur_arg : vt_parse_node_acc;
    cur_seg : vt_parse_node_acc;

    script_state : script_mode;
    uplevel_script : boolean; -- Flag to identify script created with an uplevel command

    prev : script_obj_acc; -- Parent script
    succ : script_obj_acc; -- Child script
  end record;


  type scope_obj;
  type scope_obj_acc is access scope_obj;
  type scope_obj is record
    vars : variable_map(0 to 15); -- Array of hashed variable references

    script_stack : script_obj_acc; -- Bottom of script stack
    script : script_obj_acc; -- Current script in this scope
    
    depth : natural;

    prev : scope_obj_acc; -- Parent scope
    succ : scope_obj_acc; -- Child scope
  end record;


  type ul_scope_obj;
  type ul_scope_obj_acc is access ul_scope_obj;
  type ul_scope_obj is record
    scope_stack : scope_obj_acc;
    succ : ul_scope_obj_acc;
  end record;


  type result_obj is record
    value : vt_parse_node_acc;
    is_ref : boolean; -- Indicates if return_value is a reference or an owned object
  end record;

  type vt_interp is record
    active : boolean;
    scope_stack : scope_obj_acc; -- Bottom of scope stack
    scope : scope_obj_acc;       -- Current scope; Top of stack
    
    uplevel_scopes : ul_scope_obj_acc;

    commands  : command_map(0 to 255); -- Array of hashed command defs
    ensembles : ensemble_map(0 to 64); -- Array of hashed ensemble sub-commands

    EI : expr_interp_acc;        -- Interpreter object for evaluating expressions
    
    recursion_limit : natural;

    result : result_obj;

    exit_code : integer;
  end record;

  type vt_interp_acc is access vt_interp;


  procedure assert_true(test : boolean; msg : string; severity_lvl : severity_level;
    VIO : inout vt_interp_acc);

  procedure substitute_in_string( VIO  : inout vt_interp_acc; str : inout unbounded_string);

  procedure arg_count( args : inout vt_parse_node_acc; count : out natural );

  procedure expect_arg_count(args : inout vt_parse_node_acc; count : natural;
    cname : string; arg_help : string; VIO : inout vt_interp_acc);

  -- ## Get the last node in a chain of siblings
  procedure get_last(node : inout vt_parse_node_acc; last : inout vt_parse_node_acc);

  -- ## Get the element at "offset" position after "node". Returns null node if offset is >= length
  procedure get_element(node : inout vt_parse_node_acc; offset : in natural; element : inout vt_parse_node_acc);

  procedure convert_to_commands(cbody : inout vt_parse_node_acc);

  -- ## Convert a node into a string object
  procedure stringify( node : inout vt_parse_node_acc );

  procedure groupify(node : inout vt_parse_node_acc);

  procedure def_command( VIO : inout vt_interp_acc; name : in string; id : in command_id;
    variable arg_defs : vt_parse_node_acc := null; variable cbody : vt_parse_node_acc := null );
    
  procedure get_command( VIO : inout vt_interp_acc; name : in string; cmd : inout command_info_acc );
  procedure rename_command( VIO : inout vt_interp_acc; source : in string; dest : in string );
  procedure del_command( VIO :  inout vt_interp_acc; name : in string );
      
  procedure def_ensemble( VIO : inout vt_interp_acc; name : in string; id : in ensemble_id );

  procedure get_ensemble( VIO : inout vt_interp_acc; name : in string; id : out ensemble_id );
  
  procedure get_variable( VIO : inout vt_interp_acc;
    name : in string; var : out scope_var_acc );

  procedure get_variable( scope : inout scope_obj_acc; name : in string;
    var : out scope_var_acc );


  procedure set_variable( scope : inout scope_obj_acc; variable name : string;
    variable value : vt_parse_node_acc; make_copy : boolean := true; copy_siblings : boolean := true );

  procedure set_variable( VIO : inout vt_interp_acc; variable name : string;
    variable value : vt_parse_node_acc; make_copy : boolean := true; copy_siblings : boolean := true );

  procedure set_variable( VIO : inout vt_interp_acc; variable name : string;
    variable value : integer);

  procedure set_variable( VIO : inout vt_interp_acc; variable name : string;
    variable value : real);

  procedure del_variable( VIO : inout vt_interp_acc; variable name : string );

  procedure link_variable( VIO : inout vt_interp_acc; name : in string; other_var : inout scope_var_acc );

  procedure set_result( VIO : inout vt_interp_acc; variable rval : vt_parse_node_acc := null;
    is_ref : boolean := true );
  procedure set_result( VIO : inout vt_interp_acc; value : integer );
  procedure set_result( VIO : inout vt_interp_acc; value : character );

  procedure push_scope( variable VIO : inout vt_interp_acc );
  procedure pop_scope( variable VIO : inout vt_interp_acc );

  procedure push_script( scope : inout scope_obj_acc; script_state : in script_mode;
    variable parse_tree : vt_parse_node_acc := null );
  procedure pop_script( VIO : inout vt_interp_acc );
--  procedure pop_script( scope : inout scope_obj_acc );

  procedure free( var : inout scope_var_acc );
  procedure free( cmd : inout command_info_acc );
  procedure free( script : inout script_obj_acc );
  procedure free( scope : inout scope_obj_acc );
  procedure free( VIO : inout vt_interp_acc );

end package;


library extras;
use extras.strings_unbounded.unbounded_string;
use extras.characters_handling;

library vertcl;
use vertcl.vt_lexer.all;
use vertcl.vt_parser.all;

package body vt_interpreter_core is

  alias char is extras.characters_handling;

  procedure assert_true(test : boolean; msg : string; severity_lvl : severity_level;
    VIO : inout vt_interp_acc) is
    variable cur_cmd : vt_parse_node_acc;
    variable lnum : integer := -1;
  begin
    if not test then
      -- Get the the current command
      if VIO.scope.script.cur_cmd /= null then
        cur_cmd := VIO.scope.script.cur_cmd;
        if cur_cmd.tok.kind /= TOK_integer then
          lnum := cur_cmd.tok.value;
        end if;
      end if;
      assert false report "INTERP: " & msg & LF &
        "(At line: " & integer'image(lnum) & ")" severity severity_lvl;
    end if;

  end procedure;


  alias maps is extras.strings_maps; 

  constant VARIABLE_CHAR_SET : maps.character_set := (
    '0'                         to  '9'                        => true,
    'A'                         to  'Z'                        => true,
    'a'                         to  'z'                        => true,
    '_'                         to  '_'                        => true,
    others  =>  false
  );


  procedure substitute_in_string( VIO : inout vt_interp_acc; str : inout unbounded_string) is
    variable subst, var_value_str: unbounded_string;
    variable i, head : natural;
    variable ch : character;
    variable quoted : boolean;
    variable var : scope_var_acc;

    type scan_mode is (NORMAL_TEXT, VAR_TEXT, ESCAPE_TEXT);
    variable state : scan_mode;
    variable hex_val : natural;
  begin
    SU.initialize(subst);

    i := str.all'low;
    head := i;
    while i <= str.all'high loop
      ch := str(i);

      case state is
        when NORMAL_TEXT =>
          if ch = '$' then
            while i < str.all'high and str(i+1) = '$' loop -- Skip over repeated '$'
              i := i + 1;
              ch := str(i);
            end loop;
          end if;

          if ch = '$' and i < str.all'high then -- Start of a variable

            if i > head then -- Normal text waiting to be appended
              SU.append(subst, str(head to i-1));
            end if;

            if i < str.all'high and str(i+1) = '{' then -- Quoted with {}
              i := i + 1;
              quoted := true;
            end if;
            head := i+1;
            state := VAR_TEXT;

          elsif ch = '\' then
            if i > head then -- Normal text waiting to be appended
              SU.append(subst, str(head to i-1));
            end if;

            state := ESCAPE_TEXT;
          end if;

        when VAR_TEXT =>
          if (quoted and ch /= '}') or (not quoted and maps.is_in(ch, VARIABLE_CHAR_SET)) then
            -- Valid char for a variable
            -- FIXME: add support for arrays
          else -- Variable has ended
            --report "@@@@@@@@ var ended: " & str(head to i-1);
            get_variable(VIO, str(head to i-1), var);
            if var /= null then
              to_unbounded_string(var.var.value, var_value_str);
              SU.append(subst, var_value_str);
              deallocate(var_value_str);
            else
              assert_true(false, "Unknown variable '" & str(head to i-1) & "'", warning, VIO);
            end if;

            head := i;
            if not quoted then -- Back up
              i := i - 1;
            else -- Skip }
              head := head + 1;
            end if;

            quoted := false;
            state := NORMAL_TEXT;
          end if;

        when ESCAPE_TEXT =>
          case ch is
            when 'a' => SU.append(subst, BEL);
            when 'b' => SU.append(subst, BS);
            when 'f' => SU.append(subst, FF);
            when 'n' => SU.append(subst, LF);
            when 'r' => SU.append(subst, CR);
            when 't' => SU.append(subst, HT);
            when 'v' => SU.append(subst, VT);
            when 'x' =>
              assert_true(i+2 <= str.all'high, "Invalid hex escape at end of string", failure, VIO);
              assert_true(char.is_hexadecimal_digit(str(i+1)) and
                char.is_hexadecimal_digit(str(i+2)), "Invalid hex character", failure, VIO);
              hex_val := to_hex_number(str(i+1));
              hex_val := hex_val*16 + to_hex_number(str(i+2));

              SU.append(subst, character'val(hex_val));

              i := i + 2;
            when others =>
              SU.append(subst, ch);
          end case;

          head := i + 1;
          state := NORMAL_TEXT;

      end case;

      i := i + 1;
    end loop;

    -- Finish up last segment
    case state is
      when NORMAL_TEXT =>
        SU.append(subst, str(head to str'high));

      when VAR_TEXT =>
        assert_true(quoted = false, "Expecting close brace for variable", failure, VIO);

        get_variable(VIO, str(head to str'high), var);

        if var /= null then
          to_unbounded_string(var.var.value, var_value_str);
          SU.append(subst, var_value_str);
          deallocate(var_value_str);
        else
          assert_true(false, "Unknown variable '" & str(head to str'high) & "'", warning, VIO);
        end if;

      when ESCAPE_TEXT => -- This shouldn't happen
        assert_true(false, "Invalid escape", failure, VIO);
--        case ch is
--          when 'a' => SU.append(subst, BEL);
--          when 'b' => SU.append(subst, BS);
--          when 'f' => SU.append(subst, FF);
--          when 'n' => SU.append(subst, LF);
--          when 'r' => SU.append(subst, CR);
--          when 't' => SU.append(subst, HT);
--          when 'v' => SU.append(subst, VT);
--          when 'x' => assert_true(false, "Invalid hex escape at end of string", failure, VIO);
--          when others =>
--            SU.append(subst, ch);
--        end case;

    end case;

    deallocate(str);
    str := subst;
  end procedure;
  

  procedure def_command( VIO : inout vt_interp_acc; name : in string; id : in command_id;
    variable arg_defs : vt_parse_node_acc := null; variable cbody : vt_parse_node_acc := null ) is

    variable cmd : command_info_acc;
    constant h : natural := SF.hash(name) mod VIO.commands'length;
  begin
    -- FIXME: handle redefinition of existing command
    cmd := new command_info;
    cmd.name := new string'(name);
    cmd.id := id;
    cmd.arg_defs := arg_defs;
    cmd.cbody := cbody;

    cmd.succ := VIO.commands(h);
    VIO.commands(h) := cmd;
  end procedure;


  procedure get_command( VIO : inout vt_interp_acc; name : in string; cmd : inout command_info_acc ) is
    variable cur : command_info_acc;
    constant h : natural := SF.hash(name) mod VIO.commands'length;
  begin
    -- Lookup the command ID
    cur := VIO.commands(h);
    while cur /= null loop
      exit when cur.name.all = name;
      cur := cur.succ;
    end loop;

    cmd := cur;
  end procedure;
  
  
  procedure rename_command( VIO : inout vt_interp_acc; source : in string; dest : in string ) is
    variable cmd : command_info_acc;
    constant h : natural := SF.hash(dest) mod VIO.commands'length;
  begin
    get_command(VIO, source, cmd);
    if cmd /= null then
      def_command(VIO, dest, cmd.id, cmd.arg_defs, cmd.cbody);
      cmd.arg_defs := null;
      cmd.cbody := null;
      del_command(VIO, cmd.name.all);
    end if;
  end procedure;

  
  procedure del_command( VIO :  inout vt_interp_acc; name : in string ) is
    variable cur, succ : command_info_acc;
    constant h : natural := SF.hash(name) mod VIO.commands'length;
  begin
    -- Lookup the command ID
    cur := VIO.commands(h);
    if cur = null then -- Nothing to delete
      return;
    end if;
    
    if cur.name.all = name then -- At top of list
      VIO.commands(h) := cur.succ;
      cur.succ := null;
      free(cur);
    elsif cur.succ /= null then
      while cur.succ /= null loop
        if cur.succ.name.all = name then
          succ := cur.succ;
          cur.succ := succ.succ;
          succ.succ := null;
          free(succ);
          exit;
        end if;
        cur := cur.succ;
      end loop;
    end if;

  end procedure;
  

  procedure def_ensemble( VIO : inout vt_interp_acc; name : in string; id : in ensemble_id ) is

    variable e : ensemble_info_acc;
    constant h : natural := SF.hash(name) mod VIO.ensembles'length;
  begin
    e := new ensemble_info;
    e.name := new string'(name);
    e.id := id;

    e.succ := VIO.ensembles(h);
    VIO.ensembles(h) := e;
  end procedure;


  procedure get_ensemble( VIO : inout vt_interp_acc; name : in string; id : out ensemble_id ) is
    variable cur_ens : ensemble_info_acc;

    constant h : natural := SF.hash(name) mod VIO.ensembles'length;
  begin
    -- Lookup the ensemble ID
    cur_ens := VIO.ensembles(h);
    while cur_ens /= null loop
      exit when cur_ens.name.all = name;
      cur_ens := cur_ens.succ;
    end loop;

    if cur_ens /= null then
      id := cur_ens.id;
    else
      id := ENS_UNKNOWN;
    end if;
  end procedure;


  procedure arg_count( args : inout vt_parse_node_acc; count : out natural ) is
    variable cur : vt_parse_node_acc;
    variable cnt : natural;
  begin
    cur := args;
    while cur /= null loop
      cnt := cnt + 1;
      cur := cur.succ;
    end loop;

    count := cnt;
  end procedure;

  procedure expect_arg_count(args : inout vt_parse_node_acc; count : natural;
    cname : string; arg_help : string; VIO : inout vt_interp_acc) is
    variable counted : natural;
  begin
    arg_count(args, counted);
    assert_true(counted = count, "Invalid arguments to '" & cname & "' command." & LF &
      "Expecting: " & arg_help, failure, VIO);
  end procedure;


  -- // Get the last node in a chain of siblings
  procedure get_last(node : inout vt_parse_node_acc; last : inout vt_parse_node_acc) is
    variable cur : vt_parse_node_acc;
  begin
    cur := node;
    while cur /= null loop
      if cur.succ = null then
        last := cur;
        exit;
      end if;
      cur := cur.succ;
    end loop;
  end procedure;

  -- // Get the element at "offset" position after "node". Returns null node if offset is >= length
  procedure get_element(node : inout vt_parse_node_acc; offset : in natural; element : inout vt_parse_node_acc) is
    variable cur : vt_parse_node_acc;
    variable i : integer;
  begin
    cur := node;
    i := 0;
    while cur /= null loop
      if i = offset then
        element := cur;
        return;
      end if;
      cur := cur.succ;
      i := i + 1;
    end loop;
    
    element := null;
  end procedure;



  procedure convert_to_commands(cbody : inout vt_parse_node_acc) is
    variable cur, cmd, last_arg : vt_parse_node_acc;
    variable in_cmd : boolean := false;
  begin
    cbody.kind := VN_cmd_list;
    cur := cbody.child;
    top: while cur /= null loop
      if not in_cmd then
        while cur.kind = VN_EOL loop -- Consume any leading EOLs before the next command
          last_arg := cur;
          if cur = cbody.child then -- No commands built yet
            cbody.child := cur.succ;
            cur := cbody.child;
          else -- At least one command processed
            cmd.succ := cur.succ;
            cur := cur.succ;
          end if;
          last_arg.succ := null;
          free(last_arg);

          exit top when cur = null;
        end loop;
        
        cur.kind := VN_command;
        cmd := cur;
        --last_arg := null;
        in_cmd := true;
        
        -- Transfer command start to new child
        new_vt_parse_node(cur, VN_word);
        cur.child := cmd.child;
        cur.tok := cmd.tok;
        cmd.child := cur;
        cmd.tok.data := null;
        cmd.tok.kind := TOK_UNKNOWN;
        last_arg := cur;
        cur := cmd;
        
        
      elsif cur.kind = VN_EOL then -- Command ended
        cmd.succ := cur.succ;
        cur.succ := null;
        free(cur);
        cur := cmd;
        in_cmd := false;
        
      else -- Add node as argument to command
        last_arg.succ := cur;
        last_arg := cur;
        cmd.succ := cur.succ;
        cur := cmd;
        last_arg.succ := null;
      end if;
      cur := cur.succ;
    end loop;
  end procedure;


--////////////////////////////

  -- ## Convert a node into a string object
  procedure stringify( node : inout vt_parse_node_acc ) is
    variable img : unbounded_string;
  begin
    if node.tok.kind /= TOK_string then
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
    end if;
  end procedure;


  -- ## Convert a node into a group object
  -- #  This will reparse a node's text representation
  procedure groupify(node : inout vt_parse_node_acc) is
    variable script : unbounded_string;
    variable parse_tree : vt_parse_node_acc;
  begin
    to_unbounded_string(node, script, false);
    report "$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ GROUPIFY: >" & script.all & "<";
    parse_vertcl_string(script, parse_tree);
    --SU.free(script);
    
    --assert_true(parse_tree /= null, "Unable to groupify tree", failure);
    assert parse_tree /= null report "Unable to groupify tree" severity failure;

    free(node.tok);
    node.tok.kind := TOK_group_begin;
    node.kind := VN_group;
    node.child := parse_tree;
    
    write_parse_tree("group_tree.txt", node);
    
  end procedure;

  -- ## Convert a node into a group object
  -- #  This will split strings containing whitespace into separate group elements
  procedure xxgroupify(node : inout vt_parse_node_acc) is
    variable first_child, last_child, new_word : vt_parse_node_acc;
    variable first_break, word_start : natural;
    variable at_break : boolean := true;
    variable ch : character;
  begin

    --FIXME: Properly handle embedded {}'s

    -- Transfer the node contents to the new child node
    new_vt_parse_node(first_child, node.kind);
    first_child.tok := node.tok;
    node.tok.data := null;
    node.tok.kind := TOK_group_begin;
    node.kind := VN_group;

    -- Make the data a child of the new group node
    first_child.child := node.child;
    node.child := first_child;

    -- If the child node is a string with spaces then we will split it into separate nodes
    if first_child.tok.kind = TOK_string then
      last_child := first_child;
      for i in first_child.tok.data.all'range loop
        ch := first_child.tok.data(i);
        if ch = ' ' or ch = HT then
          if not at_break then -- Found a word delimited by spaces
            new_vt_parse_node(new_word, VN_word);
            SU.copy(first_child.tok.data(word_start to i-1), new_word.tok.data);
            new_word.tok.kind := TOK_string;
            last_child.succ := new_word;
            last_child := new_word;

            if first_break = 0 then
              first_break := i;
            end if;
            at_break := true;
          end if;

        else -- Other char
          if at_break then
            word_start := i;
            at_break := false;
          end if;
        end if;
        
      end loop;

      if not at_break then -- Append last word
        new_vt_parse_node(new_word, VN_word);
        SU.copy(first_child.tok.data(word_start to first_child.tok.data'high), new_word.tok.data);
        new_word.tok.kind := TOK_string;
        last_child.succ := new_word;
        last_child := new_word;
      end if;

      if first_child.succ /= null then -- The string was split up
        -- Remove first_child now that its data is split into its remaining siblings
        node.child := first_child.succ;
        first_child.succ := null;
        free(first_child);
      end if;
    end if;
    
  end procedure;


  procedure get_variable( scope : inout scope_obj_acc; name : in string;
    var : out scope_var_acc ) is

    variable cur_var : scope_var_acc;
    constant h : natural := SF.hash(name) mod scope.vars'length;
  begin
    cur_var := scope.vars(h);
    while cur_var /= null loop
      if cur_var.name.all = name then
        var := cur_var;
        return;
      end if;
      cur_var := cur_var.succ;
    end loop;

    var := null;
  end procedure;

  procedure get_variable( VIO : inout vt_interp_acc; name : in string;
    var : out scope_var_acc ) is
  begin
    get_variable(VIO.scope, name, var);
  end procedure;


  procedure get_variable( VIO : inout vt_interp_acc; name : in string;
    value : out integer ) is
    variable var : scope_var_acc;
    variable vnode : vt_parse_node_acc;
  begin
    get_variable(VIO.scope, name, var);
    if var /= null then
      vnode := var.var.value;
      if vnode.tok.kind = TOK_integer then
        value := vnode.tok.value;
      elsif vnode.tok.kind = TOK_float then
        value := integer(vnode.tok.float);
      end if;
    end if;
  end procedure;

  procedure get_variable( VIO : inout vt_interp_acc; name : in string;
    value : out real ) is
    variable var : scope_var_acc;
    variable vnode : vt_parse_node_acc;
  begin
    get_variable(VIO.scope, name, var);
    if var /= null then
      vnode := var.var.value;
      if vnode.tok.kind = TOK_integer then
        value := real(vnode.tok.value);
      elsif vnode.tok.kind = TOK_float then
        value := vnode.tok.float;
      end if;
    end if;
  end procedure;


  procedure set_variable( scope : inout scope_obj_acc; variable name : string;
    variable value : vt_parse_node_acc; make_copy : boolean := true; copy_siblings : boolean := true ) is

    variable var : scope_var_acc;
    variable tstr : unbounded_string;
    constant h : natural := SF.hash(name) mod scope.vars'length;
  begin

    get_variable(scope, name, var);

    if var /= null then -- Variable exists
      -- Replace the variable value
      free(var.var.value);
    else -- New variable

      -- Add a new variable to the list
      var := new scope_var;
      var.var := new variable_rec;
      var.var.ref_count := 1;

      SU.copy(name, var.name);

      var.succ := scope.vars(h);
      scope.vars(h) := var;

    end if;

    if make_copy then
      copy_parse_tree(value, var.var.value, copy_siblings);
    else
      var.var.value := value;
    end if;

    to_unbounded_string(var.var.value.tok, tstr);
    report "### set var: " & name & " = " & tstr.all & "  " & vt_node_kind'image(var.var.value.kind);
    deallocate(tstr);
  end procedure;

  -- ## Set variable in current scope
  procedure set_variable( VIO : inout vt_interp_acc; variable name : string;
    variable value : vt_parse_node_acc; make_copy : boolean := true; copy_siblings : boolean := true ) is
  begin
    set_variable(VIO.scope, name, value, make_copy, copy_siblings);
  end procedure;

  -- ## Set integer variable in current scope
  procedure set_variable( VIO : inout vt_interp_acc; variable name : string;
    variable value : integer) is
    variable node : vt_parse_node_acc;
  begin
    new_vt_parse_node(node, VN_word);
    node.tok.kind := TOK_integer;
    node.tok.value := value;
    set_variable(VIO.scope, name, node, false);
  end procedure;

  -- ## Set real variable in current scope
  procedure set_variable( VIO : inout vt_interp_acc; variable name : string;
    variable value : real) is
    variable node : vt_parse_node_acc;
  begin
    new_vt_parse_node(node, VN_word);
    node.tok.kind := TOK_float;
    node.tok.float := value;
    set_variable(VIO.scope, name, node, false);
  end procedure;
  

  procedure del_variable( VIO : inout vt_interp_acc; variable name : string ) is
    variable var, cur_var : scope_var_acc;
    constant h : natural := SF.hash(name) mod VIO.scope.vars'length;
  begin

    get_variable(VIO.scope, name, var);

    if var /= null then -- Variable exists
      -- Find any preceding var defs in the same hash bin
      cur_var := VIO.scope.vars(h);
      if cur_var = var then -- Var def is at start of list
        VIO.scope.vars(h) := cur_var.succ;
      else
        while cur_var /= null loop
          exit when cur_var.succ = var;
          cur_var := cur_var.succ;
        end loop;
        
        cur_var.succ := var.succ;
      end if;

      var.succ := null;
      free(var);
    end if;
    
  end procedure;


  procedure link_variable( VIO : inout vt_interp_acc; name : in string; other_var : inout scope_var_acc ) is
    variable var : scope_var_acc;
    constant h : natural := SF.hash(name) mod VIO.scope.vars'length;
  begin
    assert_true(other_var /= null, "Can't link to invalid variable", failure, VIO);

    var := new scope_var;
    var.var := other_var.var;
    var.var.ref_count := var.var.ref_count + 1;
    SU.copy(name, var.name);
    var.succ := VIO.scope.vars(h);
    VIO.scope.vars(h) := var;
  end procedure;

  procedure push_scope( variable VIO : inout vt_interp_acc ) is
    variable scope : scope_obj_acc;
  begin
    scope := new scope_obj;
    scope.prev := VIO.scope;
    scope.succ := null;
    
    scope.depth := VIO.scope.depth + 1; -- Track recursive depth
    
    assert_true(scope.depth < VIO.recursion_limit, "Recursion limit reached: " & integer'image(VIO.recursion_limit),
      failure, VIO);

    VIO.scope.succ := scope;
    VIO.scope := scope;
  end procedure;

  procedure pop_scope( variable VIO : inout vt_interp_acc ) is
    variable scope : scope_obj_acc;
  begin
    if VIO.scope.prev /= null then -- Not the top-level scope
      scope := VIO.scope;
      VIO.scope := VIO.scope.prev;
      VIO.scope.succ := null;
      free(scope);
    end if;
  end procedure;


  procedure push_script( scope : inout scope_obj_acc; script_state : in script_mode;
    variable parse_tree : vt_parse_node_acc := null ) is
    variable script : script_obj_acc;
  begin
    report "%%%%%%%%% PUSH new script";
    script := new script_obj;
    script.prev := scope.script;
    script.succ := null;
    
    script.uplevel_script := false;

    if scope.script_stack /= null then
      scope.script.script_state := script_state; -- Set mode on higher level script
      scope.script.succ := script;
    else -- Stack is empty
      scope.script_stack := script;
    end if;
    scope.script := script;
    
    script.parse_tree := parse_tree;
    script.cur_cmd := parse_tree;
  end procedure;

  procedure pop_script( VIO : inout vt_interp_acc ) is
    variable script : script_obj_acc;
    variable old_scope, cur : scope_obj_acc;
    variable ul_scope : ul_scope_obj_acc;
  begin
    report "%%%%%%%%% POP script";
    script := VIO.scope.script;
    if script /= null and script.prev /= null then -- Not the top-level script
      VIO.scope.script := VIO.scope.script.prev;
      VIO.scope.script.succ := null;
    else -- Top of script stack
      VIO.scope.script_stack := null;
      VIO.scope.script := null;
    end if;
    
    -- Check if this was an uplevel script
    if script.uplevel_script then -- Restore calling scope
      report "%%%%%%%%%%%%%% FINISHED UPLEVEL SCRIPT";
      assert_true(VIO.uplevel_scopes /= null, "Missing uplevel scope", failure, VIO);
      ul_scope := VIO.uplevel_scopes;
      old_scope := ul_scope.scope_stack;
      VIO.uplevel_scopes := ul_scope.succ;
      deallocate(ul_scope);
      
      -- Find top of old scope stack
      cur := old_scope;
      while cur /= null loop
        exit when cur.succ = null;
        cur := cur.succ;
      end loop;

      -- Reconnect old scope stack
      VIO.scope.succ := old_scope;
      old_scope.prev := VIO.scope;
      VIO.scope := cur;
    end if;
    
    free(script);
  end procedure;

  procedure set_result( VIO : inout vt_interp_acc; variable rval : vt_parse_node_acc := null;
    is_ref : boolean := true ) is
--    variable rv_copy : vt_parse_node_acc;
  begin
    report "@@@@@@@@@@@ set_result";
    if VIO.result.value /= null and not VIO.result.is_ref then
      report "DBG: set_result: freeing old value: " & integer'image(VIO.result.value.id); -- %2008 DEBUG%
      free(VIO.result.value);
    end if;

    -- We defer copying the return value when it is a reference, otherwise we take
    -- ownership of an independent value node tree.
    VIO.result.value := rval;
    VIO.result.is_ref := is_ref;

-------------------
--    if make_copy and rval /= null then
--      copy_parse_tree(rval, rv_copy);
--    else
--      rv_copy := rval;
--    end if;

--    if VIO.result.value /= null then
--      report "DBG: set_result: freeing old value: " & integer'image(VIO.result.value.id);
--      free(VIO.result.value);
--    end if;

--    VIO.result.value := rv_copy;
--    if rv_copy /= null then
--      report "DBG: set_result: parse tree: " & integer'image(VIO.result.value.id);
--    end if;
  end procedure;


  procedure set_result( VIO : inout vt_interp_acc; value : integer ) is
    variable rval : vt_parse_node_acc;
  begin
    new_vt_parse_node(rval, VN_word);
    rval.tok.value := value;
    rval.tok.kind := TOK_integer;
    set_result(VIO, rval, false);
  end procedure;

  procedure set_result( VIO : inout vt_interp_acc; value : character ) is
    variable rval : vt_parse_node_acc;
  begin
    new_vt_parse_node(rval, VN_word);
    rval.tok.data := new string(1 to 1);
    rval.tok.data(1) := value;
    rval.tok.kind := TOK_string;
    set_result(VIO, rval, false);
  end procedure;


  procedure free( var : inout variable_rec_acc ) is
  begin
    if var /= null then
      var.ref_count := var.ref_count - 1;
      if var.ref_count = 0 then
        free(var.value);
        deallocate(var);
      end if;
    end if;
  end procedure;
  
  procedure free( var : inout scope_var_acc ) is
    variable cur_var, succ_var : scope_var_acc;
  begin
    cur_var := var;
    while cur_var /= null loop
      succ_var := cur_var.succ;
      free(cur_var.name);
      free(cur_var.var);
      deallocate(cur_var);
      cur_var := succ_var;
    end loop;
  end procedure;

  procedure free( cmd : inout command_info_acc ) is
    variable cur, succ : command_info_acc;
  begin
    cur := cmd;
    while cur /= null loop
      succ := cur.succ;
      free(cur.name);
      free(cur.arg_defs);
      free(cur.cbody);
      deallocate(cur);
      cur := succ;
    end loop;
  end procedure;
  

  procedure free( script : inout script_obj_acc ) is
  begin
    if script /= null then
      --assert script.parse_tree /= null report "DOUBLE FREE ERROR" severity error;
      -- Release the parse tree
      free(script.parse_tree);
      deallocate(script);
    end if;
  end procedure;


  procedure free( scope : inout scope_obj_acc ) is
    variable cur_script, succ_script : script_obj_acc;
  begin
    -- Release the variables
    for i in scope.vars'range loop
      free(scope.vars(i));
    end loop;

    -- Release the script stack
    cur_script := scope.script_stack;
    while cur_script /= null loop
      succ_script := cur_script.succ;
      free(cur_script);
      cur_script := succ_script;
    end loop;

    deallocate(scope);
  end procedure;

  procedure free( VIO : inout vt_interp_acc ) is
    variable cur_scope, succ_scope : scope_obj_acc;
    variable cur_ens, succ_ens : ensemble_info_acc;
    variable cur_ul, succ_ul : ul_scope_obj_acc;
  begin
    -- Release all scopes
    cur_scope := VIO.scope_stack;
    while cur_scope /= null loop
      succ_scope := cur_scope.succ;
      free(cur_scope);
      cur_scope := succ_scope;
    end loop;
    VIO.scope_stack := null;
    VIO.scope := null;
    
    -- Release any uplevel scopes that weren't restored
    cur_ul := VIO.uplevel_scopes;
    while cur_ul /= null loop
      succ_ul := cur_ul.succ;
      free(cur_ul.scope_stack);
      deallocate(cur_ul);
      cur_ul := succ_ul;
    end loop;
    VIO.uplevel_scopes := null;


    if VIO.result.value /= null and not VIO.result.is_ref then
      free(VIO.result.value);
      VIO.result.value := null;
    end if;

    -- Release the command list
    for i in VIO.commands'range loop
      free(VIO.commands(i));
    end loop;
    
    -- Release the ensemble list
    for i in VIO.ensembles'range loop
      cur_ens := VIO.ensembles(i);
      while cur_ens /= null loop
        succ_ens := cur_ens.succ;
        free(cur_ens.name);
        deallocate(cur_ens);
        cur_ens := succ_ens;
      end loop;
    end loop;
    
    free(VIO.EI);

    deallocate(VIO);
    VIO := null;
  end procedure;


end package body;
