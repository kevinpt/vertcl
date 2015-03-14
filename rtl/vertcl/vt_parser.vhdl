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
--# vt_parser.vhdl - VerTcl parser
--# Freely available from http://code.google.com/p/vertcl
--#
--# Copyright © 2015 Kevin Thibedeau
--# (kevin 'period' thibedeau 'at' gmail 'punto' com)
--#
--# This source is released under the terms of the MIT license.
--# See the LICENSE file for more details.
-------------------------------------------------------------------
library extras;
use extras.strings_unbounded.unbounded_string;
--use extras.text_buffering.all;
--use extras.strings_fixed;

library vertcl;
use vertcl.vt_lexer.all;

library vertcl_2008;            -- %2008 DEBUG%
use vertcl_2008.alloc_tracker;  -- %2008 DEBUG%

package vt_parser is

  alias SU is extras.strings_unbounded;
  alias SF is extras.strings_fixed;

  type vt_node_kind is (
    -- Misc nodes
    VN_UNKNOWN, VN_EOL, VN_cmd_list, VN_command, VN_group, VN_word, VN_splat, VN_string_seg
  );

  type vt_parse_node;
  type vt_parse_node_acc is access vt_parse_node;
  type vt_parse_node is record
    kind       : vt_node_kind;      -- Identifey type of node
    tok        : vt_token;          -- Token for this node
    id         : natural;           -- Allocation sequence number to aid in debugging memory leaks -- %2008 DEBUG%

    child      : vt_parse_node_acc; -- Children tree
    -- NOTE: The last_child is only kept valid in the parser to simplify
    -- the attach_child() procedure. Inside the interpreter, parse trees are
    -- manipulated without maintaining the last_child pointer.
    last_child : vt_parse_node_acc; -- Last node in child
    succ       : vt_parse_node_acc; -- Next sibling
  end record;
  
  procedure is_null(variable o : in vt_parse_node_acc; result : out boolean); -- %2008 DEBUG%
  procedure null_obj(variable o : out vt_parse_node_acc);                     -- %2008 DEBUG%
  procedure obj_id(variable o : in vt_parse_node_acc);                        -- %2008 DEBUG%


  -- ## Parse a file of Tcl commands. The result is a parse tree that can be passed
  -- #  to an interpreter object for execution.
  procedure parse_vertcl_file( fname : in string; parse_tree : inout vt_parse_node_acc );

  -- ## Parse a Tcl script string. The result is a parse tree that can be passed
  -- #  to an interpreter object for execution.
  procedure parse_vertcl_string( script : inout unbounded_string; parse_tree : inout vt_parse_node_acc );

  -- ## Write a parse tree to a text file
  procedure write_parse_tree( fname : in string; variable parse_tree : in vt_parse_node_acc );

  -- ## Perform a deep copy to duplicate a parse tree
  procedure copy_parse_tree( variable source : in vt_parse_node_acc; dest : out vt_parse_node_acc;
    include_siblings : in boolean := true );

  -- ## Replace a node with another parse tree. Ownership of the spliced tree is required
  -- #  as its first node will be deallocated after the splice.
  procedure splice_parse_tree( source : inout vt_parse_node_acc;
    dest : inout vt_parse_node_acc );

  -- ## Create a single line summary of a parse tree in a string
  procedure to_unbounded_string( variable source : in vt_parse_node_acc;
    variable dest : out unbounded_string; follow_siblings : boolean := true; sep : string := " " );

  -- ## Construct a new parse tree node with the provided node kind
  procedure new_vt_parse_node( variable node : inout vt_parse_node_acc;
    kind : in vt_node_kind := VN_UNKNOWN );

  -- ## Release memory allocated to a parse tree structure
  procedure free( parse_tree : inout vt_parse_node_acc );


  package pn_alloc_tracker is new vertcl_2008.alloc_tracker   -- %2008 DEBUG%
    generic map ( obj_t => vt_parser.vt_parse_node_acc,       -- %2008 DEBUG%
      is_null => vt_parser.is_null,                           -- %2008 DEBUG%
      null_obj => vt_parser.null_obj,                         -- %2008 DEBUG%
      obj_id => vt_parser.obj_id);                            -- %2008 DEBUG%
  use pn_alloc_tracker.all;                                   -- %2008 DEBUG%
  shared variable track_nodes : pn_alloc_tracker.tracker;     -- %2008 DEBUG%

end package;

-- FIXME: Handle null command substitutions []


use std.textio.all;

library extras;
use extras.text_buffering.all;

package body vt_parser is

  procedure is_null(variable o : in vt_parse_node_acc; result : out boolean) is -- %2008 DEBUG%
  begin                                                                         -- %2008 DEBUG%
    result := o = null;                                                         -- %2008 DEBUG%
  end procedure;                                                                -- %2008 DEBUG%

  procedure null_obj(variable o : out vt_parse_node_acc) is                     -- %2008 DEBUG%
  begin                                                                         -- %2008 DEBUG%
    o := null;                                                                  -- %2008 DEBUG%
  end procedure;                                                                -- %2008 DEBUG%

  procedure obj_id(variable o : in vt_parse_node_acc) is                        -- %2008 DEBUG%
  begin                                                                         -- %2008 DEBUG%
    report "PARSE NODE: " & integer'image(o.id) & "  " & vt_node_kind'image(o.kind) & -- %2008 DEBUG%
      "  " & vt_token_kind'image(o.tok.kind) &                                  -- %2008 DEBUG%
      "  " & integer'image(o.tok.value);                                        -- %2008 DEBUG%
  end procedure;                                                                -- %2008 DEBUG%

-- PRIVATE procedures:
-- ===================

  -- // Validate test conditions with standard error message format
  procedure assert_true(test : boolean; msg : string; severity_lvl : severity_level;
    variable VLO : in vt_lex_acc) is
  begin
    if not test then
      assert false report "PARSER: " & msg & LF
        & "(At line " & integer'image(VLO.line_num) & ")"
        severity severity_lvl;
    end if;
  end procedure;

  -- // Validate test conditions with standard error message format for nodes
  procedure assert_true(test : boolean; msg : string; severity_lvl : severity_level;
    variable node : in vt_parse_node_acc) is
  begin
    if not test then
      assert false report "PARSER: " & msg severity severity_lvl;
    end if;
  end procedure;


  -- // Write a parse tree to a text file handle. Called by write_parse_tree().
  procedure write_parse_nodes( file fh : text; variable parse_tree : in vt_parse_node_acc;
    depth : in natural ) is

    variable cur : vt_parse_node_acc;
    variable tline : unbounded_string;

    constant leading_space : string(1 to depth*2) := (others => ' ');
  begin

    cur := parse_tree;

    while cur /= null loop
      SU.append(tline, leading_space);
      SU.append(tline, vt_node_kind'image(cur.kind));
      SU.append(tline, "  ");
      if cur.tok.kind /= TOK_UNKNOWN then
        if cur.tok.data /= null then
          SU.append(tline, """" & cur.tok.data.all & """");
        elsif cur.tok.kind = TOK_integer then
          SU.append(tline, integer'image(cur.tok.value));
        elsif cur.tok.kind = TOK_float then
          SU.append(tline, real'image(cur.tok.float));
        elsif cur.tok.kind = TOK_splat then
          SU.append(tline, "{*}");
        end if;

        SU.append(tline, "  ");
        SU.append(tline, vt_token_kind'image(cur.tok.kind));
      end if;

      writeline(fh, tline);

      if cur.child /= null then
        write_parse_nodes(fh, cur.child, depth+1);
      end if;

      cur := cur.succ;
    end loop;

  end procedure;


  -- // Connect a new child to a parent node in a parse tree
  procedure attach_child( variable child : in vt_parse_node_acc; parent : inout vt_parse_node_acc ) is
  begin
    if parent.child = null then
      parent.child := child;
      parent.last_child := child;
    else
      parent.last_child.succ := child;
      parent.last_child := child;
    end if;
  end procedure;



-- Grammar productions
-- -------------------

  procedure parse_command(VLO : inout vt_lex_acc; cmd: inout vt_parse_node_acc);
  procedure parse_string_seg(VLO : inout vt_lex_acc; str_node: inout vt_parse_node_acc);
  procedure parse_splat(VLO : inout vt_lex_acc; splat_node: inout vt_parse_node_acc);
  procedure parse_subst(VLO : inout vt_lex_acc; cmd_list: inout vt_parse_node_acc);
  procedure parse_group(VLO : inout vt_lex_acc; group_node: inout vt_parse_node_acc; expect_end_brace : boolean := true);


  -- // Parse a list of Tcl commands separated by newlines or ";"
  procedure parse_command_list( VLO : inout vt_lex_acc; parse_tree: inout vt_parse_node_acc) is

    variable tok  : vt_token;
    variable cmd, cmd_start  : vt_parse_node_acc;
    variable estr : unbounded_string;
  begin

    next_token(VLO, tok);
    while( tok.kind /= TOK_EOB ) loop
--      consume_token(VLO);

      case tok.kind is
--        when TOK_string => -- Start of a command (FIXME: handle commands that start as substs)
--          consume_token(VLO);
--          new_vt_parse_node(cmd, VN_command);
----          cmd.tok := tok;
--          new_vt_parse_node(cmd_start, VN_word); -- FIXME: simplify this. Merge with subst_begin?
--          cmd_start.tok := tok;
--          attach_child(cmd_start, cmd);
--          
--          parse_command(VLO, cmd);
--          attach_child(cmd, parse_tree);

        when TOK_string | TOK_subst_begin =>
          -- Don't consume token so that it can be picked up inside parse_command()
          new_vt_parse_node(cmd, VN_command);
          parse_command(VLO, cmd);
          attach_child(cmd, parse_tree);

        when TOK_splat => -- Splat group
          consume_token(VLO);
          new_vt_parse_node(cmd, VN_splat);
          cmd.tok := tok;
          parse_splat(VLO, cmd);
          attach_child(cmd, parse_tree);

        when TOK_EOL =>
          consume_token(VLO);
          -- Ignore EOL outside of groups
          null;

        when others =>
          consume_token(VLO);
          to_unbounded_string(tok, estr);

          assert_true(false, "Illegal token: " & estr.all, failure, VLO);
          deallocate(estr);
          free(tok);

      end case;

      next_token(VLO, tok);
    end loop;

    consume_token(VLO); -- Mark last EOB token as used
    free(tok);

  end procedure;


  -- // Parse a Tcl command argument list
  procedure parse_command(VLO : inout vt_lex_acc; cmd: inout vt_parse_node_acc) is

    variable tok : vt_token;
    variable arg, seg : vt_parse_node_acc;
  begin
    -- Get all arguments to the command
    next_token(VLO, tok);

    while( tok.kind /= TOK_EOB and tok.kind /= TOK_EOL ) loop
      consume_token(VLO);
      new_vt_parse_node(arg);

      if tok.kind = TOK_group_begin then
        arg.kind := VN_group;
        arg.tok.value := VLO.line_num;
        free(tok);
        parse_group(VLO, arg);

      elsif tok.kind = TOK_subst_begin then
        arg.kind := VN_cmd_list;
        arg.tok.value := VLO.line_num;
        free(tok);
        parse_subst(VLO, arg);

      elsif tok.kind = TOK_splat then
        arg.kind := VN_splat;
        arg.tok := tok;
        arg.tok.value := VLO.line_num;
        parse_splat(VLO, arg);

      elsif tok.kind = TOK_string_seg then
        arg.kind := VN_string_seg;
        arg.tok.value := VLO.line_num;

        if tok.data.all'length > 0 then -- Start with a segment
          new_vt_parse_node(seg, VN_word);
          seg.tok := tok;
          seg.tok.kind := TOK_string;
          attach_child(seg, arg);
        else -- Empty string
          free(tok);
        end if;

        parse_string_seg(VLO, arg);

      else
        arg.kind := VN_word;
        arg.tok := tok;
      end if;

      attach_child(arg, cmd); -- Add argument to command

      next_token(VLO, tok);
    end loop;

    consume_token(VLO);
    free(tok);

  end procedure;


  -- // Parse a segmented string (one containing embedded command substs)
  procedure parse_string_seg(VLO : inout vt_lex_acc; str_node: inout vt_parse_node_acc) is
    variable tok : vt_token;
    variable seg : vt_parse_node_acc;
  begin

    -- Get the elements of the segmented string
    next_token(VLO, tok);
    while( tok.kind /= TOK_string_seg_end ) loop
      consume_token(VLO);
      assert_true(tok.kind /= TOK_EOB,
        "Unexpected end of buffer while parsing segmented string. Missing '""'", failure, VLO);

      new_vt_parse_node(seg);

      if tok.kind = TOK_group_begin then -- Nested group (FIXME remove??)
        seg.kind := VN_group;
        free(tok);
        parse_group(VLO, seg);
      elsif tok.kind = TOK_subst_begin then -- Nested command substitution
        seg.kind := VN_cmd_list;
        seg.tok.value := VLO.line_num;
        free(tok);
        parse_subst(VLO, seg);
      elsif tok.kind = TOK_EOL then -- Preserve EOL in groups (FIXME: remove this?)
        seg.kind := VN_EOL;
        seg.tok := tok;

      elsif tok.kind = TOK_splat then
        seg.kind := VN_splat;
        seg.tok := tok;
        seg.tok.value := VLO.line_num;
        parse_splat(VLO, seg);

      else -- Ordinary word
        seg.kind := VN_word;
        seg.tok := tok;
        seg.tok.kind := TOK_string;
      end if;

      attach_child(seg, str_node);

      next_token(VLO, tok);
    end loop;

    consume_token(VLO);

    if tok.data.all'length > 0 then -- Attach terminal segment
      new_vt_parse_node(seg);
      seg.kind := VN_word;
      seg.tok := tok;
      seg.tok.kind := TOK_string;
      attach_child(seg, str_node);    
    else -- Empty string; not needed
      free(tok);
    end if;

  end procedure;


  -- // Parse an expansion operator {*}
  procedure parse_splat(VLO : inout vt_lex_acc; splat_node: inout vt_parse_node_acc) is
    variable tok : vt_token;
    variable arg, seg : vt_parse_node_acc;
  begin

    next_token(VLO, tok);
    if tok.kind /= TOK_EOB and tok.kind /= TOK_EOL then
      consume_token(VLO);

      new_vt_parse_node(arg);

      if tok.kind = TOK_group_begin then
        arg.kind := VN_group;
        arg.tok.value := VLO.line_num;
        free(tok);
        parse_group(VLO, arg);

      elsif tok.kind = TOK_subst_begin then
        arg.kind := VN_cmd_list;
        arg.tok.value := VLO.line_num;
        free(tok);
        parse_subst(VLO, arg);

      elsif tok.kind = TOK_string_seg then
        arg.kind := VN_string_seg;
        arg.tok.value := VLO.line_num;

        if tok.data.all'length > 0 then -- Start with a segment
          new_vt_parse_node(seg, VN_word);
          seg.tok := tok;
          attach_child(seg, arg);
        else -- Empty string
          free(tok);
        end if;

        parse_string_seg(VLO, arg);

      else
        arg.kind := VN_word;
        arg.tok := tok;
      end if;

      attach_child(arg, splat_node);
    end if;

  end procedure;


  -- // Parse a command substitution [...]
  -- FIXME: Can this call back to parse_command() ??
  procedure parse_subst(VLO : inout vt_lex_acc; cmd_list: inout vt_parse_node_acc) is

    variable tok : vt_token;
    variable subst_node, cmd, seg : vt_parse_node_acc;
  begin

    -- Get the elements of the command
    next_token(VLO, tok);
    while( tok.kind /= TOK_subst_end ) loop
      consume_token(VLO);
      assert_true(tok.kind /= TOK_EOB,
        "Unexpected end of buffer while parsing substitution. Missing ']'", failure, VLO);

      if tok.kind /= TOK_EOL then
        new_vt_parse_node(subst_node);
        subst_node.tok.value := VLO.line_num;

        if tok.kind = TOK_group_begin then -- Nested group
          subst_node.kind := VN_group;
          free(tok);
          parse_group(VLO, subst_node);
          
        elsif tok.kind = TOK_subst_begin then -- Nested substitution
          subst_node.kind := VN_cmd_list;
          free(tok);
          parse_subst(VLO, subst_node);

        elsif tok.kind = TOK_splat then
          subst_node.kind := VN_splat;
          subst_node.tok := tok;
          parse_splat(VLO, subst_node);

        elsif cmd = null then -- Start new command
          new_vt_parse_node(cmd, VN_command);
          attach_child(cmd, cmd_list);
          subst_node.kind := VN_word;
          subst_node.tok := tok;
          
        elsif tok.kind = TOK_string_seg then
          subst_node.kind := VN_string_seg;
          subst_node.tok.value := VLO.line_num;

          if tok.data.all'length > 0 then -- Start with a segment
            new_vt_parse_node(seg, VN_word);
            seg.tok := tok;
            seg.tok.kind := TOK_string;
            attach_child(seg, subst_node);
          else -- Empty string
            free(tok);
          end if;

          parse_string_seg(VLO, subst_node);

          
        else -- Command argument
          subst_node.kind := VN_word;
          subst_node.tok := tok;
        end if;

        
        if cmd /= null then -- Argument to a command
          attach_child(subst_node, cmd);
        else -- Subst, group, or splat
          attach_child(subst_node, cmd_list);
        end if;


      else -- EOL signals start of a new command
        cmd := null;
        free(tok);
      end if;

      next_token(VLO, tok);
    end loop;

    consume_token(VLO);
    free(tok);
  end procedure;


  -- // Parse a group {...}
  procedure parse_group(VLO : inout vt_lex_acc; group_node: inout vt_parse_node_acc; expect_end_brace : boolean := true) is

    variable tok        : vt_token;
    variable group_elem : vt_parse_node_acc;
  begin
    -- Get the elements of the group
    next_token(VLO, tok);
    while( tok.kind /= TOK_group_end ) loop
      consume_token(VLO);
      
      if expect_end_brace then
        assert_true(tok.kind /= TOK_EOB,
          "Unexpected end of buffer while parsing group. Missing '}'", failure, VLO);
      elsif tok.kind = TOK_EOB then
        free(tok);
        return;
      end if;

      new_vt_parse_node(group_elem);

      if tok.kind = TOK_group_begin then -- Nested group
        group_elem.kind := VN_group;
        free(tok);
        parse_group(VLO, group_elem);
      elsif tok.kind = TOK_subst_begin then -- Nested command substitution
        group_elem.kind := VN_cmd_list;
        group_elem.tok.value := VLO.line_num;
        free(tok);
        parse_subst(VLO, group_elem);
      elsif tok.kind = TOK_EOL then -- Preserve EOL in groups
        group_elem.kind := VN_EOL;
        group_elem.tok := tok;

      elsif tok.kind = TOK_splat then
        group_elem.kind := VN_splat;
        group_elem.tok := tok;
        group_elem.tok.value := VLO.line_num;
        parse_splat(VLO, group_elem);

      else -- Ordinary word
        group_elem.kind := VN_word;
        group_elem.tok := tok;
      end if;

      attach_child(group_elem, group_node);

      next_token(VLO, tok);
    end loop;

    consume_token(VLO);
    free(tok);

  end procedure;




-- PUBLIC procedures:
-- ==================

  -- ## Parse a file of Tcl commands. The result is a parse tree that can be passed
  -- #  to an interpreter object for execution.
  procedure parse_vertcl_file( fname : in string; parse_tree : inout vt_parse_node_acc ) is
    variable buf : text_buffer;
    variable VLO : vt_lex_acc;
    variable tracker_name : string(1 to 30);     -- %2008 DEBUG%
  begin
    -- Initialize tracker
    SF.move("parse_node tracker", tracker_name); -- %2008 DEBUG%
    track_nodes.init(tracker_name);              -- %2008 DEBUG%

 	  load_buffer(fname, buf);
    vt_lexer_init(buf, VLO);

    -- Start with a top-level command list node
    new_vt_parse_node(parse_tree, VN_cmd_list);
    parse_tree.tok.value := VLO.line_num;

    parse_command_list(VLO, parse_tree);

    free(VLO);
  end procedure;


  -- ## Parse a Tcl script string. The result is a parse tree that can be passed
  -- #  to an interpreter object for execution.
  procedure parse_vertcl_string( script : inout unbounded_string; parse_tree : inout vt_parse_node_acc ) is
    variable buf : text_buffer;
    variable VLO : vt_lex_acc;
    variable tnode : vt_parse_node_acc;
  begin
  
    append(script, buf);
    vt_lexer_init(buf, VLO);

    -- Start with a temporary top-level command node
    new_vt_parse_node(tnode, VN_command);

    parse_group(VLO, tnode, false); -- Parse as a group with no terminating '}' char
    parse_tree := tnode.child;
    tnode.child := null;
    free(tnode);
    free(buf); -- FIXME: is this right?

    free(VLO);
  
  end procedure;


  -- ## Write a parse tree to a text file
  procedure write_parse_tree( fname : in string; variable parse_tree : in vt_parse_node_acc ) is
    file fh : text;
    variable fstatus : file_open_status;
    variable cur_node : vt_parse_node_acc;
  begin

    file_open(fstatus, fh, fname, write_mode);
    assert_true(fstatus = open_ok,"Unable to open file for write_parse_tree: " & fname,
      failure, parse_tree);

    write_parse_nodes(fh, parse_tree, 0);
    file_close(fh);
  end procedure;

  -- ## Perform a deep copy to duplicate a parse tree
  procedure copy_parse_tree( variable source : in vt_parse_node_acc; dest : out vt_parse_node_acc;
    include_siblings : in boolean := true ) is

    variable cur, new_node           : vt_parse_node_acc;
    variable new_tree, new_tree_tail : vt_parse_node_acc;
    variable cur_child               : vt_parse_node_acc;
  begin
    cur := source;

    while cur /= null loop
      new_vt_parse_node(new_node, cur.kind);
      copy_token(cur.tok, new_node.tok);

      if new_tree_tail /= null then
        new_tree_tail.succ := new_node;
      else -- Start of tree
        new_tree := new_node;
      end if;

      new_tree_tail := new_node;

      if cur.child /= null then
        copy_parse_tree(cur.child, cur_child, true);
        new_node.child := cur_child;

        while cur_child /= null loop
          if cur_child.succ = null then -- Found the last child of this branch
            new_node.last_child := cur_child;
          end if;
          cur_child := cur_child.succ;
        end loop;
      end if;

      exit when not include_siblings; -- Stop copying

      cur := cur.succ;
    end loop;

    dest := new_tree;

  end procedure;


  -- ## Replace a node with another parse tree. Ownership of the spliced tree is required
  -- #  as its first node will be deallocated after the splice.
  -- #  If the spliced tree has siblings and the target node is the last child of its parent
  -- #  the last_child field of the parent will become invalid.
  procedure splice_parse_tree( source : inout vt_parse_node_acc;
    dest : inout vt_parse_node_acc ) is

    variable old_succ, cur : vt_parse_node_acc;
  begin

    -- Release allocated data not needed for this node
    if dest.tok.data /= null then
      deallocate(dest.tok.data);
    end if;
    if dest.child /= null then
      free(dest.child);
    end if;

    -- Copy contents of first node over the destination
    dest.tok := source.tok;
    dest.kind := source.kind;

    old_succ := dest.succ;
    dest.child := source.child;
    dest.last_child := null; -- last_child is no longer valid for our parent either
    dest.succ := source.succ;

    -- Splice in old successor sibling
    cur := dest;
    while cur /= null loop
      if cur.succ = null then
        cur.succ := old_succ;
        exit;
      end if;
      cur := cur.succ;
    end loop;
    
    -- The head node of the source is not needed. Detach and deallocate
    source.tok.data := null;
    source.child := null;
    source.succ := null;
    free(source);
  end procedure;


  -- ## Create a single line summary of a parse tree in a string
  procedure to_unbounded_string( variable source : in vt_parse_node_acc;
    variable dest : out unbounded_string; follow_siblings : boolean := true; sep : string := " " ) is

    variable tree_img, group_img, cmd_img, tok_str : unbounded_string;
    variable cur, cmd : vt_parse_node_acc;
  begin

    SU.initialize(tree_img);

    cur := source;
    while cur /= null loop
      if cur.kind = VN_group then
        if cur.child /= null then
          to_unbounded_string(cur.child, group_img);
        else
          SU.initialize(group_img);
        end if;
        SU.append(tree_img, "{" & group_img.all & "}");
        SU.free(group_img);
      elsif cur.kind = VN_cmd_list then
        cmd := cur.child;
        while cmd /= null loop
          to_unbounded_string(cmd.child, cmd_img);
          SU.append(group_img, cmd_img.all);
          SU.free(cmd_img);
          if cmd.succ /= null then
            SU.append(group_img, "; ");
          end if;
          cmd := cmd.succ;
        end loop;
        SU.append(tree_img, "[" & group_img.all & "]");
        SU.free(group_img);
      elsif cur.tok.kind /= TOK_UNKNOWN then
        -- FIXME: {} around strings containing spaces and empty strings?
        to_unbounded_string(cur.tok, tok_str);
        SU.append(tree_img, tok_str);
        SU.free(tok_str);
      else
        SU.append(tree_img, "(UNK)");
      end if;

      exit when not follow_siblings; -- Stop generating string

      if cur.succ /= null then
        --SU.append(tree_img, ' ');
        SU.append(tree_img, sep);
      end if;

      cur := cur.succ;
    end loop;

    dest := tree_img;
  end procedure;


  -- ## Construct a new parse tree node with the provided node kind
  procedure new_vt_parse_node( variable node : inout vt_parse_node_acc;
    kind : in vt_node_kind := VN_UNKNOWN ) is
  begin
    node := new vt_parse_node;
    node.kind := kind;
    track_nodes.new_obj(node);         -- %2008 DEBUG%
    node.id := track_nodes.get_allocs; -- %2008 DEBUG%
    report "ALLOC NODE: " & integer'image(track_nodes.get_allocs) & "  " & vt_node_kind'image(kind); -- %2008 DEBUG%
  end procedure;


  -- ## Release memory allocated to a parse tree structure
  procedure free( parse_tree : inout vt_parse_node_acc ) is
    variable cur, succ : vt_parse_node_acc;
  begin
    cur := parse_tree;

    while cur /= null loop
      if cur.child /= null then
        free(cur.child);
      end if;

      succ := cur.succ;
      free(cur.tok);
      track_nodes.free_obj(cur); -- %2008 DEBUG%
      deallocate(cur);
      cur := succ;
    end loop;

  end procedure;

end package body;
