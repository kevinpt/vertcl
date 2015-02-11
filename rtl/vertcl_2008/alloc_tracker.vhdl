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
--# alloc_tracker.vhdl - Memory allocation debug support
--# Freely available from http://code.google.com/p/vertcl
--#
--# Copyright © 2015 Kevin Thibedeau
--# (kevin 'period' thibedeau 'at' gmail 'punto' com)
--#
--# This source is released under the terms of the MIT license.
--# See the LICENSE file for more details.
--#
--# DESCRIPTION:
--#  This is a generic package that implements a general purpose
--#  tracker for allocated memory. It can be used to find memory
--#  leaks when working with access types. It must be compiled
--#  with VHDL-2008 support.
-------------------------------------------------------------------

package alloc_tracker is
  generic (
    type obj_t; -- Access type to track allocations
    procedure is_null(variable o : in obj_t; result : out boolean); -- True if object is null
    procedure null_obj(variable o : out obj_t); -- Factory for null objects
    procedure obj_id(variable o : in obj_t)     -- Routine to report a unique identifier for the object
  );

  type obj_vec is array(natural range <>) of obj_t;
  type boolean_vec is array(natural range <>) of boolean;

  type obj_tracker;
  type obj_tracker_acc is access obj_tracker;
  type obj_tracker is record
    vec : obj_vec(0 to 255); -- List of allocated objects

    -- null_obj and is_null() don't seem to work on Modelsim so we will track deallocations
    -- in a separate vector.
    null_vec : boolean_vec(0 to 255); -- List tracking deallocations
    next_slot : natural;    -- Index of next unoccupied position in vec 
    succ : obj_tracker_acc; -- Next block of tracker data
  end record;


  type tracker is protected
    -- ## Configure a tacker with a specified name
    -- #  Use the strings_fixed.move() procedure to copy a shorter string to the fixed length type
    procedure init(tracker_name : in string(1 to 30));

    -- ## Generate a report on the current allocations and deallocations
    procedure report_status;

    -- ## Track a new object
    procedure new_obj(variable o : in obj_t);

    -- ## Stop tracking a deallocated object
    procedure free_obj(variable o : in obj_t);

    -- ## Release a tracker instance
    procedure free_tracker;

    -- ## Get number of allocated objects
    -- #  This can be used as a unique ID to store in new objects
    impure function get_allocs return natural;

    -- ## Get number of deallocated objects
    impure function get_deallocs return natural;
  end protected;

end package;


library extras;
use extras.strings.trim_end;
use extras.strings_fixed;

package body alloc_tracker is

  alias SF is extras.strings_fixed;

  type tracker is protected body
    variable name : string(1 to 30);
    variable top_ot : obj_tracker_acc;
    variable ot : obj_tracker_acc;
    variable total_allocs : natural;
    variable total_deallocs : natural;
    variable total_double_free : natural;

    procedure init(tracker_name : in string(1 to 30)) is
    begin
      name := tracker_name;
    end procedure;


    procedure report_status is
      variable null_count, alloc_index : natural;
      variable cur : obj_tracker_acc;
      --variable a_null_obj : boolean;
    begin
      cur := top_ot;
      while cur /= null loop
        for i in 0 to cur.next_slot-1 loop
--          is_null(cur.vec(i), a_null_obj);
--          if a_null_obj then null_count := null_count + 1; end if;
          if cur.null_vec(i) then
            null_count := null_count + 1;
          end if;
        end loop;
        cur := cur.succ;
      end loop;

      report SF.trim(name, right) & ": Total allocs = " & integer'image(total_allocs) &
        ", Total deallocs = " & integer'image(total_deallocs) &
        ", Delta = " & integer'image(total_allocs - total_deallocs) & LF &
        "Total double free = " & integer'image(total_double_free) &
        ", Null objects = " & integer'image(null_count);

      cur := top_ot;
      while cur /= null loop
        for i in 0 to cur.next_slot-1 loop
          if cur.null_vec(i) = false then
            report SF.trim(name, right) & ": Unfreed object " & integer'image(alloc_index);
            obj_id(cur.vec(i));
          end if;

          alloc_index := alloc_index + 1;
        end loop;
        cur := cur.succ;
      end loop;

    end procedure;


    procedure new_obj(variable o : in obj_t) is
      variable new_ot : obj_tracker_acc;
      variable a_null_obj : boolean;
    begin
      is_null(o, a_null_obj);
      if a_null_obj then
        report "TRACKER: Attempt to track null object" severity error;
        return;
      end if;

      total_allocs := total_allocs + 1;

      -- Initialize first tracker
      if top_ot = null then
        top_ot := new obj_tracker;
        ot := top_ot;
      end if;

      -- Check if tracker is filled
      if ot.next_slot > ot.vec'high then
        new_ot := new obj_tracker;
        ot.succ := new_ot;
        ot := new_ot;
      end if;

      -- Add object to tracker
      ot.vec(ot.next_slot) := o;
      ot.null_vec(ot.next_slot) := false;
      ot.next_slot := ot.next_slot + 1;
    end procedure;

    procedure free_obj(variable o : in obj_t) is
      variable cur : obj_tracker_acc;
      variable found : boolean := false;
      variable a_null_obj : boolean;
      variable no : obj_t;
    begin

      total_deallocs := total_deallocs + 1;

      -- Find this object in the tracker list
      cur := top_ot;
      search: while cur /= null loop
        for i in cur.vec'range loop
          if cur.vec(i) = o then -- Found object
            null_obj(no);
            cur.vec(i) := no;
            if cur.null_vec(i) then -- Double free
              report SF.trim(name, right) & ": Attempt to deallocate NULL object (" &
                integer'image(total_deallocs) & ")" severity error;
              total_double_free := total_double_free + 1;
            end if;
            cur.null_vec(i) := true;
            found := true;
            exit search;
          end if;
        end loop;
        cur := cur.succ;
      end loop;

      assert found
        report SF.trim(name, right) & ": Attempt to deallocate untracked object (" & integer'image(total_deallocs) & ")"
        severity error;

    end procedure;


    procedure free_tracker is
      variable cur, succ : obj_tracker_acc;
    begin
      cur := top_ot;
      while cur /= null loop
        succ := cur.succ;
        deallocate(cur);
        cur := succ;
      end loop;
      top_ot := null;
      ot := null;
    end procedure;


    impure function get_allocs return natural is
    begin
      return total_allocs;
    end function;


    impure function get_deallocs return natural is
    begin
      return total_deallocs;
    end function;

  end protected body;

end package body;


package demo_obj is
  type iacc is access integer;

  procedure is_null(variable o : in iacc; result : out boolean);
  procedure null_obj(variable o : out iacc);


end package;

package body demo_obj is

  procedure is_null(variable o : in iacc; result : out boolean) is
  begin
    report "### checking is_null " & boolean'image(o = null);
    result := o = null;
  end procedure;

  procedure null_obj(variable o : out iacc) is
  begin
    o := null;
  end procedure;
end package body;


--library vertcl;
--use work.demo_obj.all;

--package example is
--  package alloc_int is new vertcl.alloc_tracker
--    generic map ( obj_t => iacc,
--      is_null => work.demo_obj.is_null,
--      null_obj => work.demo_obj.null_obj);

--  use work.alloc_int.all;

----  procedure dosomething(m : inout obj_map);
--  shared variable x : tracker;
--end package;


--package body example is
--  shared variable t : tracker;
--end package body;

