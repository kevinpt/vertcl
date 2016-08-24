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
--# vt_expr_parser.vhdl - Expression parser for the expr command
--# Freely available from http://code.google.com/p/vertcl
--#
--# Copyright © 2015 Kevin Thibedeau
--# (kevin 'period' thibedeau 'at' gmail 'punto' com)
--#
--# This source is released under the terms of the MIT license.
--# See the LICENSE file for more details.
-------------------------------------------------------------------

-- This package implements routines to parse the following syntax:
--
-- simple-expression ::= expression<0>
-- expression<precedence> ::= term [ binary-operator expression<binary-precedence> ]
-- term ::= unary-operator expression<unary-precedence> | ( simple-expression ) | primary |
--   identifier ( simple-expression [, simple-expression]* )
-- binary-operator ::= + | - | * | / | ** | == | != | > | >= | < | <=
-- unary-operator := + | -
-- primary ::= identifier | integer-literal | float-literal
--
-- if binary-operator is right associative:
--   binary-precedence ::= the operator precedence
-- else left associative:
--   binary-precedence ::= the operator precedence + 1
--
-- The rule for expression<precedence> forms a continuous loop that is broken when the precedence of the
-- next operator in the token stream is less than the value of <precedence>.

library extras;
use extras.strings_unbounded.unbounded_string;

library vertcl;
use vertcl.vt_expr_lexer.all;

library vertcl_2008;           -- %2008 DEBUG%
use vertcl_2008.alloc_tracker; -- %2008 DEBUG%

package vt_expr_parser is

  alias SF is extras.strings_fixed;

  type vt_expr_node_kind is (
    -- Misc nodes
    EN_UNKNOWN, EN_statements, EN_identifier, EN_numeric,

    -- Unary operators

    -- Unary and binary operators
    EN_OP_unary_binary,

    -- Binary operators
    EN_OP_binary
  );

  type vt_eparse_node;
  type vt_eparse_node_acc is access vt_eparse_node;

  type vt_eparse_node is record
    kind : vt_expr_node_kind;        -- Identify type of node
    tok  : vt_etoken;                -- Token for this node
    id   : natural;                  -- Allocation sequence number to aid in debugging memory leaks -- %2008 DEBUG%

    child      : vt_eparse_node_acc; -- Children tree
    last_child : vt_eparse_node_acc; -- Last node in child
    succ       : vt_eparse_node_acc; -- Next sibling
  end record;

  -- ## Parse an expression string and return the representative parse tree
  procedure parse_expression( expr : inout unbounded_string; parse_tree : inout vt_eparse_node_acc );

  -- ## Write a parse tree to a text file
  procedure write_parse_tree( fname : in string; variable parse_tree : in vt_eparse_node_acc );

  -- ## Perform a deep copy to duplicate a parse tree
  procedure copy_parse_tree( variable source : in vt_eparse_node_acc; dest : out vt_eparse_node_acc );

  -- ## Create a single line summary of a parse tree in a string
  procedure to_unbounded_string( source : inout vt_eparse_node_acc; dest : inout unbounded_string );

  -- ## Construct a new parse tree node with the provided node kind
  procedure new_vt_eparse_node( node : inout vt_eparse_node_acc; kind : in vt_expr_node_kind );

  -- ## Release memory allocated to a parse tree structure
  procedure free( parse_tree : inout vt_eparse_node_acc );

  -- ## Test if a parse node operator is a binary operator
  function is_operator( kind : vt_etoken_kind ) return boolean;

  -- ## Test if a parse node operator is a unary operator
  function is_unary_operator( kind : vt_etoken_kind ) return boolean;


  -- PRIVATE types
  -- ==============

  -- Precedence tables for the operators. These control the order of operations for the expression
  -- parser.

  -- Left associativity:  a + b + c --> (a+b) + c
  -- Right associativity: a ** b ** c --> a ** (b ** c)
  type associativity is (left, right);
  type precedence_entry is record
    plevel : natural;       -- Higher values have higher precedence
    assoc  : associativity;
  end record;
  type op_precedence_vector is array(vt_etoken_kind range <>) of precedence_entry;

  -- Binary operator precedence
  constant B_PRECEDENCE : op_precedence_vector(vt_etoken_binary) := (
    ETOK_minus  => (12, left),
    ETOK_plus   => (12, left),
    ETOK_mul    => (13, left),
    ETOK_div    => (13, left),
    ETOK_mod    => (13, left),
    ETOK_pow    => (14, right),
    ETOK_eq     => (9, left),
    ETOK_ne     => (9, left),
    ETOK_lt     => (10, left),
    ETOK_gt     => (10, left),
    ETOK_le     => (10, left),
    ETOK_ge     => (10, left),
    ETOK_and    => (3, left),
    ETOK_or     => (2, left),
    ETOK_bit_and => (6, left),
    ETOK_bit_or  => (4, left),
    ETOK_bit_xor => (5, left)
    );

  -- Unary operator precedence
  constant U_PRECEDENCE : op_precedence_vector(vt_etoken_unary) := (
    ETOK_bit_not => (15, left),
    ETOK_not     => (15, left),
    ETOK_minus   => (15, left),
    ETOK_plus    => (15, left)
  );


  procedure is_null(variable o : in vt_eparse_node_acc; result : out boolean); -- %2008 DEBUG%
  procedure null_obj(variable o : out vt_eparse_node_acc);                     -- %2008 DEBUG%
  procedure obj_id(variable o : in vt_eparse_node_acc);                        -- %2008 DEBUG%

  package en_alloc_tracker is new vertcl_2008.alloc_tracker                    -- %2008 DEBUG%
    generic map ( obj_t => vt_expr_parser.vt_eparse_node_acc,                  -- %2008 DEBUG%
      is_null => vt_expr_parser.is_null,                                       -- %2008 DEBUG%
      null_obj => vt_expr_parser.null_obj,                                     -- %2008 DEBUG%
      obj_id => vt_expr_parser.obj_id);                                        -- %2008 DEBUG%
  use en_alloc_tracker.all;                                                    -- %2008 DEBUG%
  shared variable track_expr_nodes : en_alloc_tracker.tracker;                 -- %2008 DEBUG%


end package;


use std.textio.text; use std.textio.writeline;

package body vt_expr_parser is

-- PRIVATE procedures:
-- ===================

  procedure is_null(variable o : in vt_eparse_node_acc; result : out boolean) is -- %2008 DEBUG%
  begin                                                                          -- %2008 DEBUG%
    result := o = null;                                                          -- %2008 DEBUG%
  end procedure;                                                                 -- %2008 DEBUG%

  procedure null_obj(variable o : out vt_eparse_node_acc) is                     -- %2008 DEBUG%
  begin                                                                          -- %2008 DEBUG%
    o := null;                                                                   -- %2008 DEBUG%
  end procedure;                                                                 -- %2008 DEBUG%

  procedure obj_id(variable o : in vt_eparse_node_acc) is                        -- %2008 DEBUG%
  begin                                                                          -- %2008 DEBUG%
    report "EXPR PARSE NODE: " & integer'image(o.id) & "  " & vt_expr_node_kind'image(o.kind) & -- %2008 DEBUG%
      "  " & vt_etoken_kind'image(o.tok.kind) &                                  -- %2008 DEBUG%
      "  " & integer'image(o.tok.int);                                           -- %2008 DEBUG%
  end procedure;                                                                 -- %2008 DEBUG%


  -- // Validate test conditions with standard error message format
  procedure assert_true(test : boolean; msg : string; severity_lvl : severity_level;
    variable ELO : in expr_lex_acc) is
  begin
    if not test then
      assert false report "EXPR PARSER: " & msg
        severity severity_lvl;
    end if;
  end procedure;

  -- // Validate test conditions with standard error message format for nodes
  procedure assert_true(test : boolean; msg : string; severity_lvl : severity_level;
    variable node : in vt_eparse_node_acc) is
  begin
    if not test then
      assert false report "EXPR PARSER: " & msg severity severity_lvl;
    end if;
  end procedure;


  -- // Write a parse tree to a text file handle. Called by write_parse_tree().
  procedure write_parse_nodes( file fh : text; variable parse_tree : in vt_eparse_node_acc;
    depth : in natural ) is

    variable cur : vt_eparse_node_acc;
    variable leading_space : unbounded_string;
    variable tline : unbounded_string;
  begin

    if depth > 0 then
      leading_space := SU.to_unbounded_string(depth*2);
      for i in leading_space'range loop
        SU.replace_element(leading_space, i, ' ');
      end loop;
    else
      leading_space := SU.to_unbounded_string("");
    end if;

    cur := parse_tree;

    while cur /= null loop
      SU.append(tline, leading_space);
      SU.append(tline, vt_expr_node_kind'image(cur.kind));
      SU.append(tline, "  ");
      if cur.tok.kind /= ETOK_UNKNOWN then
        if cur.tok.data /= null then
          SU.append(tline, """" & cur.tok.data.all & """");
        elsif cur.tok.int /= integer'low then
          SU.append(tline, integer'image(cur.tok.int));
        end if;

        SU.append(tline, "  ");
        SU.append(tline, vt_etoken_kind'image(cur.tok.kind));
      end if;

      writeline(fh, tline);

      if cur.child /= null then
        write_parse_nodes(fh, cur.child, depth+1);
      end if;

      cur := cur.succ;
    end loop;

  end procedure;


  -- // Connect a new child to a parent node in a parse tree
  procedure attach_child( variable child : in vt_eparse_node_acc; parent : inout vt_eparse_node_acc ) is
  begin
    if parent.child = null then
      parent.child := child;
      parent.last_child := child;
    else
      parent.last_child.succ := child;
      parent.last_child := child;
    end if;
  end procedure;



-- Expression grammar productions
-- ------------------------------

  procedure simple_expression( ELO : inout expr_lex_acc; parse_tree : inout vt_eparse_node_acc;
    plevel : natural := 0 );


  -- // Generate terms from unary expressions and the left side of binary expressions as
  --    well as parsing sub-expressions
  procedure term( ELO : inout expr_lex_acc; parse_tree : inout vt_eparse_node_acc ) is
    variable tok : vt_etoken;
    variable op, term : vt_eparse_node_acc;
  begin
    next_token(ELO, tok);

    case tok.kind is
      when ETOK_lparen => -- Sub-expression
        consume_token(ELO);
        free(tok);
        simple_expression(ELO, parse_tree, 0);

        next_token(ELO, tok);
        if tok.kind /= ETOK_rparen then -- invalid
          assert_true(false, "Missing closing parenthesis", failure, ELO);
        end if;
        consume_token(ELO);
        free(tok);

      when ETOK_integer | ETOK_float =>
        new_vt_eparse_node(term, EN_numeric);
        term.tok := tok;
        consume_token(ELO);
        parse_tree := term;

      when ETOK_identifier =>
        new_vt_eparse_node(term, EN_identifier);
        term.tok := tok;
        consume_token(ELO);
        parse_tree := term;

        next_token(ELO, tok);
        if tok.kind = ETOK_lparen then -- Function argument list
          loop
            consume_token(ELO);
            free(tok);
            next_token(ELO, tok); -- Peek at next token; will not consume this one
            exit when tok.kind = ETOK_rparen; -- Empty arg list (like for rand())
            simple_expression(ELO, term, 0);
            attach_child(term, parse_tree);

            next_token(ELO, tok);
            exit when tok.kind /= ETOK_comma; -- No more arguments
          end loop;

          if tok.kind /= ETOK_rparen then -- Invalid
            assert_true(false, "Missing closing parenthesis", failure, ELO);
          end if;
          consume_token(ELO);
          free(tok);
        end if;

      when ETOK_EOB =>
        consume_token(ELO);
        free(tok);
        --parse_tree := null;

      when others => 
        if is_unary_operator(tok.kind) then
          new_vt_eparse_node(op, EN_OP_unary_binary);
          consume_token(ELO);
          --free(tok);
          op.tok := tok;
          simple_expression(ELO, term, U_PRECEDENCE(tok.kind).plevel);
          attach_child(term, op);
          parse_tree := op;

        else -- Invalid
          assert_true(false, "Illegal expression token: " & vt_etoken_kind'image(tok.kind),
            failure, ELO);
        end if;
    end case;

  end procedure;


  -- // Parse the token stream as an expression. This implements a precedence climbing
  --    expression parser that works recursively with parse_term().
  procedure simple_expression( ELO : inout expr_lex_acc; parse_tree : inout vt_eparse_node_acc;
    plevel : natural := 0 ) is
    variable tok : vt_etoken;
    variable lterm, rterm, op : vt_eparse_node_acc;
    variable plevel_next : natural;
  begin

    term(ELO, lterm);
    next_token(ELO, tok);

    while is_operator(tok.kind) and (B_PRECEDENCE(tok.kind).plevel >= plevel) loop
      new_vt_eparse_node(op, EN_OP_binary);
      consume_token(ELO);
      op.tok := tok;
      attach_child(lterm, op);

      -- The recursion naturally produces right associativity if the precedence level is
      -- maintained. To force left associativity we increment the plevel to abort an attempt
      -- at a binary operation if the next operator is the same level as the current one.
      if B_PRECEDENCE(tok.kind).assoc = left then
        plevel_next := B_PRECEDENCE(tok.kind).plevel+1;
      else -- Right associative
        plevel_next := B_PRECEDENCE(tok.kind).plevel;
      end if;

      simple_expression(ELO, rterm, plevel_next); -- Get right term
      attach_child(rterm, op);

      lterm := op;
      next_token(ELO, tok);
    end loop;

    parse_tree := lterm;

  end procedure;


-- PUBLIC procedures:
-- ==================

  -- ## Construct a new parse tree node with the provided node kind
  procedure new_vt_eparse_node( node : inout vt_eparse_node_acc; kind : in vt_expr_node_kind ) is
  begin
    node := new vt_eparse_node;
    node.kind := kind;

    track_expr_nodes.new_obj(node);         -- %2008 DEBUG%
    node.id := track_expr_nodes.get_allocs; -- %2008 DEBUG%
    report "ALLOC EXPR NODE: " & integer'image(track_expr_nodes.get_allocs) & "  " & vt_expr_node_kind'image(kind); -- %2008 DEBUG%
  end procedure;

  -- ## Test if a parse node operator is a binary operator
  function is_operator( kind : vt_etoken_kind ) return boolean is
  begin
    case kind is
      when ETOK_minus to ETOK_ge => return true;
      when others => return false;
    end case;
  end function;

  -- ## Test if a parse node operator is a unary operator
  function is_unary_operator( kind : vt_etoken_kind ) return boolean is
  begin
    case kind is
      when ETOK_bit_not to ETOK_plus => return true;
      when others => return false;
    end case;
  end function;


  -- ## Parse an expression string and return the representative parse tree
  procedure parse_expression( expr : inout unbounded_string; parse_tree : inout vt_eparse_node_acc ) is
    variable ELO : expr_lex_acc;
    variable tok : vt_etoken;
    variable tracker_name : string(1 to 30); -- %2008 DEBUG%
  begin
    -- Initialize tracker
    SF.move("expr_parse_node tracker", tracker_name); -- %2008 DEBUG%
    track_expr_nodes.init(tracker_name);              -- %2008 DEBUG%


    elexer_init(expr, ELO);
    simple_expression(ELO, parse_tree);
    next_token(ELO, tok);
    if tok.kind /= ETOK_EOB then -- Invalid token
      assert_true(false, "Invalid token. Expecting end of expression.", failure, ELO);
    end if;
    free(ELO);
  end procedure;


  -- ## Write a parse tree to a text file
  procedure write_parse_tree( fname : in string; variable parse_tree : in vt_eparse_node_acc ) is
    file fh : text;
    variable fstatus : file_open_status;
    variable cur_node : vt_eparse_node_acc;
  begin

    file_open(fstatus, fh, fname, write_mode);
    assert_true(fstatus = open_ok, "Unable to open file for write_parse_tree: " & fname,
      failure, parse_tree);

    write_parse_nodes(fh, parse_tree, 0);
    file_close(fh);
  end procedure;

  -- ## Perform a deep copy to duplicate a parse tree
  procedure copy_parse_tree( variable source : in vt_eparse_node_acc; dest : out vt_eparse_node_acc ) is

    variable cur, new_node : vt_eparse_node_acc;
    variable new_tree, new_tree_tail : vt_eparse_node_acc;
    variable child_copy, cur_child : vt_eparse_node_acc;
  begin
    cur := source;

    while cur /= null loop
      new_vt_eparse_node(new_node, cur.kind);
      copy_token(cur.tok, new_node.tok);

      if new_tree_tail /= null then
        new_tree_tail.succ := new_node;
      else
        new_tree := new_node;
      end if;

      new_tree_tail := new_node;

      if cur.child /= null then
        copy_parse_tree(cur.child, child_copy);
        new_node.child := child_copy;

        cur_child := child_copy;
        while cur_child /= null loop
          if cur_child.succ = null then
            new_node.last_child := cur_child;
          end if;
          cur_child := cur_child.succ;
        end loop;
      end if;

      cur := cur.succ;
    end loop;

    dest := new_tree;
  end procedure;


  -- ## Create a single line summary of a parse tree in a string
  procedure to_unbounded_string( source : inout vt_eparse_node_acc; dest : inout unbounded_string ) is

    variable tree_img, sub_expr, ETOK_str : unbounded_string;
    variable cur : vt_eparse_node_acc;
  begin

    tree_img := SU.to_unbounded_string("");

    cur := source;
    while cur /= null loop
      if is_operator(cur.tok.kind) then
        case cur.tok.kind is
          when ETOK_eq => SU.append(tree_img, "(==: ");
          when ETOK_ne => SU.append(tree_img, "(!=: ");
          when ETOK_lt => SU.append(tree_img, "(<: ");
          when ETOK_gt => SU.append(tree_img, "(>: ");
          when ETOK_le => SU.append(tree_img, "(<=: ");
          when ETOK_ge => SU.append(tree_img, "(>=: ");
          when ETOK_minus => SU.append(tree_img, "(-: ");
          when ETOK_plus => SU.append(tree_img, "(+: ");
          when ETOK_mul => SU.append(tree_img, "(*: ");
          when ETOK_div => SU.append(tree_img, "(/: ");
          when ETOK_mod => SU.append(tree_img, "(%: ");
          when ETOK_pow => SU.append(tree_img, "(**: ");
          
          when ETOK_bit_not => SU.append(tree_img, "(~: ");
          when ETOK_bit_and => SU.append(tree_img, "(&: ");
          when ETOK_bit_or  => SU.append(tree_img, "(|: ");
          when ETOK_bit_xor => SU.append(tree_img, "(^: ");
          when ETOK_not => SU.append(tree_img, "(!: ");
          when ETOK_and => SU.append(tree_img, "(&&: ");
          when ETOK_or  => SU.append(tree_img, "(||: ");
          
          when others => SU.append(tree_img, "(?: ");
        end case;
        to_unbounded_string(cur.child, sub_expr);
        SU.append(tree_img, sub_expr.all & ")");
        SU.free(sub_expr);
      else
        if cur.tok.kind /= ETOK_UNKNOWN then
          to_unbounded_string(cur.tok, ETOK_str);
          SU.append(tree_img, ETOK_str);
          SU.free(ETOK_str);
        else
          SU.append(tree_img, "(UNK)");
        end if;

        if cur.child /= null then
          to_unbounded_string(cur.child, sub_expr);
          SU.append(tree_img, "<" & sub_expr.all & ">");
          SU.free(sub_expr);
        end if;

        if cur.succ /= null then
          SU.append(tree_img, ' ');
        end if;

      end if;
      cur := cur.succ;
    end loop;

    dest := tree_img;
  end procedure;

  -- ## Release memory allocated to a parse tree structure
  procedure free( parse_tree : inout vt_eparse_node_acc ) is
    variable cur, succ : vt_eparse_node_acc;
  begin
    cur := parse_tree;

    while cur /= null loop
      if cur.child /= null then
        free(cur.child);
      end if;

      succ := cur.succ;
      free(cur.tok);
      track_expr_nodes.free_obj(cur); -- %2008 DEBUG%
      deallocate(cur);
      cur := succ;
    end loop;

  end procedure;

end package body;
