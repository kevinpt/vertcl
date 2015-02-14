**VerTcl is in active development. The core functionality is implemented but most builtin commands are not complete yet.**

VerTcl is a Tcl interpreter implemented in VHDL. It provides a way to execure Tcl scripts from within a VHDL simulation to enhance design verification. In the simplest case VerTcl lets you read and parse configuration data used to alter testbench stimulus. In more complex testbenches VerTcl allows interaction with a design using portable Tcl scripts within the simulaton environment rather than from the outside. This can streamline complex simulations because Tcl scripts are reloaded after resetting a simulation without having to reelaborate a design.

VerTcl is implemented in VHDL-93 with some optional elements using VHDL-2008 features. A significant subset of Tcl is supported with some additional commands to handle simulation control. It is largely compatible with standard Tcl but some departures from the official language are made to simplify the implementation.

VerTcl is useful **for simulation only**. It depends heavily on non-synthesizable code and cannot be converted into real hardware.

Requirements
============

VerTcl is a self-contained VHDL library. It uses the string and character libraries from `vhdl-extras <http://code.google.com/p/vhdl-extras>`_ which are included. If you are using other parts of vhdl-extras in a design then you can compile it separately and ignore the string libraries included with VerTcl. You will need a VHDL simulator. Modelsim and GHDL are supported but others will work as well.

The optional build scripts depend on Python, bash, and GNU make. These are present on most Linux systems and can be installed on Windows using one of Msys, gnuwin32, or Cygwin.

Download
========

You can access the VerTcl Mercurial repository from `Google Code <https://code.google.com/p/vertcl/source/checkout>`_.

.. [https://drive.google.com/folderview?id=0B5jin2146-EXd0hBTlAzem1ybmM&usp=sharing Packaged source code] is also available for download.

Installation
============

VerTcl comes with build scripts for use with the Modelsim and GHDL simulators. You will have to manually compile for other simulators.

Modelsim
--------

You need to initialize the project by sourcing the ``start_proj.sh`` script from a Bourne compatible shell.

::

  > . start_proj.sh
  
This will set the ``MGC_WD`` environment variable to the current directory and construct a fresh ``modelsim.ini`` if it doesn't exist. The ``modelsim.ini`` is modified to use external library mapping info from the ``modelsim.map`` file. If you are using a separately compiled vhdl-extras library you need to modify the mapping for "extras" to point to the correct directory.

After setup you can build the Modelsim libraries using the ``make`` command. The compiled output is in the ``vertcl`` library located in ``build/lib/vertcl``. You can refer to this library from other Modelsim projects by mapping the ``vertcl`` library to this directory.

GHDL
----


Manually compiling VerTcl
-------------------------

If you wish to use another simulator or you're incorporating the VerTcl source into another project you will have to compile it manually. The required files are contained within the ``rtl/extras`` and ``rtl/vertcl`` directories. You must compile these into logical libraries named "extras" and "vertcl" respectively. The sources must be compiled in the following order:

* extras:

  + characters_latin_1.vhdl
  + strings_maps.vhdl
  + strings_maps_constants.vhdl
  + characters_handling.vhdl
  + strings.vhdl
  + strings_fixed.vhdl
  + strings_unbounded.vhdl
  
* vertcl:

  + vt_expr_lexer.vhdl
  + vt_expr_parser.vhdl
  + vt_expr_interpreter.vhdl
  + vt_lexer.vhdl
  + vt_parser.vhdl
  + vt_interpreter_core.vhdl
  + vt_commands.vhdl
  + vt_interpreter.vhdl

Using VerTcl
============

The VerTcl interpreter is represented as a variable of type ``vt_interp_acc`` conventionally named "VIO": VerTcl Interpreter Object. The VIO variable tracks all internal state of the interpreter including Tcl variables and the execution stack of Tcl scripts. At any time the interpreter can be halted with the ``yield`` command to return control back to the calling process. When the interpreter has finished executing a script or is halted you can retrieve and set Tcl variables through the VerTcl API. Execution will resume at the next Tcl command the next time the interpreter is run. This system turns the VerTcl interpreter into a sort of coroutine that permits interaction with other processes without needing any signal drivers in the VerTcl procedures.

If you will only be using the interpreter from a single process then it can be declared as a local variable in that process. If multiple processes need to access the interpreter then it needs to be made a shared variable. It is best to use the VHDL-93 style shared variable for this as there are no restrictions on the use of access types. If VHDL-2002 or newer is being used, '93 style shared variables are not permitted but there is an optional protected type that exposes a subset of the VerTcl API so it can be used with the newer language standards.

Unfortunately, methods of protected types are not permitted to return access type values which puts some limitations on how VerTcl can be used. With a protected type for a shared VIO variable, non-numeric Tcl variables representing strings and lists cannot be directly returned using their internal representation with dynamically allocated access types. They must instead be converted to fixed length strings and potentially reparsed into their dynamic form inside the calling process. If your simulator supports designs using mixed language standards then it is possible to instantiate VHDL-93 style shared variables in a testbench while using newer standards in the rest of the design.

Setting things up
-----------------

Executing Tcl scripts
---------------------

Interacting with processes
--------------------------

Cleaning up
-----------

Using the protected type
------------------------

Debug mode
----------

VerTcl includes an optional memory allocation tracker to help detect memory leaks. It is implemented as a VHDL-2008 generic package in the ``vertcl_2008/alloc_tracker.vhdl`` file. It has been implemented to track allocation of parse node objects for the main and expression parsers. The relevant source lines have been commented out in the affected files to make them compatible with VHDL-93. If you wish to enable the tracker you need to run the ``uncomment_2008.sh`` script. This will find lines ending with "%2008 DEBUG%" and uncomment them. After modifying the source you must edit the ``proj.mk`` file to use VHDL-2008 as the default standard with the "DEFAULT_STD" variable. Then you can recompile the library using a VHDL-2008 compliant simulator.

Supported Tcl commands
======================

VerTcl enhancements
-------------------

VerTcl provides additional builtin commands to promote interaction with other processes in a simulation:

* wait
* yield

wait
~~~~

**wait** *delay_value* *units*
  
The ``wait`` command provides a variable time delay using VHDL time units. It is equivalent to a "wait for" VHDL statement. The ``delay_value`` can be any numeric value and the ``units`` must be one defined for the VHDL ``time`` type. The simulation time is advanced by the requested delay after this command is executed.

::

  wait 10 ns ;# Delay for 10ns

yield
~~~~~

**yield** *?value?*

The ``yield`` command is analogous to the Tcl 8.6 ``yield`` command except that it results in suspending the entire VerTcl interpreter rather than a coroutine. This makes the interpreter work like a coroutine within the VHDL simulation environment. Tcl execution can be halted at any time, returning control back to the VHDL process that last invoked the interpreter. This provides an opportunity for the calling process to carry out actions that the VerTcl process can't do on its own, such as assigning to signals. When the VerTcl interpreter is invoked again, the Tcl script will resume execution after the ``yield`` command.

The optional value argument is assigned to the ``VIO.return_value field``. This value can be changed by the VHDL process. When the interpreter resumes the current, potentially changed, ``return_value`` is returned in the Tcl script.

::

  set ack [yield $result] ;# Suspend with the value of result and resume with a new value assigned to ack

Differences from standard Tcl
=============================

VerTcl has some differences from standard Tcl.

The following Tcl features are not supported:

* Unicode
* Namespaces
* Hash objects
* System I/O (files, channels, etc.)
* Sub-interpreters
* Coroutines (yield is supported for other use)
* Custom math functions in expr
* Tcl stdlib packages


At a low level VerTcl works by parsing scripts into an internal tree data structure. This helps minimize the churn of repeatedly building up and reparsing strings or maintaining dual-ported objects. In some cases this results in behavior that doesn't match the string oriented processing of Tcl. Most notably, whitespace isn't preserved within curly-braced quotes "{...}". This simplfies handling the most common case where this is used for quoting lists for which internal whitespace isn't significant. If whitespace must be preserved you will need to use double-quoted strings.

VerTcl handles double-quoted strings less flexibly than Tcl. It will not accept them in place of curly-braced groups for control structures. In practice this is not an issue as it is rare to use quoted strings in this way with Tcl.

::

  # This is not allowed:
  if "$x < 10" "puts foo"
  
  # Curly-braces are mandatory for the script block:
  if "$x < 10" {puts foo}


The front end parser will convert numeric values into tokens that represent integer or float values internally. They are not treated as strings after parsing. Hexadecimal literals are converted to integers and will print out in their decimal representation. Octal literals are not supported.

::

  set a 0xA5
  puts $a
  
  # Prints "165" in VerTcl, "0xA5" in Tcl

Integer values are restricted to the range of the VHDL ``integer`` type. Unsigned integers are not supported and the smallest signed value is ``-(2**31-1)`` rather than ``-2**31`` on simulators with 32-bit integers. Modular arithmetic (silent overflow and underflow) is not supported. Bitwise operations are handled by converting between the ``unsigned`` type from ``numeric_std`` and ``integer``.

The word expansion operator "{`*`}" introduced in Tcl 8.5 is supported with one minor difference. Standard Tcl only recognises the "{`*`}" character sequence as a word expansion if there is no whitespace following it. VerTcl accepts whitespace after this sequence. Consequently the standalone "{`*`}" string will never be parsed as a single element group containing the "`*`" string. If this is needed you must add space around the asterisk "{ `*` }".

