﻿XMCStaticStackAnalyzer
======================
Static stack analyser for Infineon XMC4 µControllers based on assembler list file


--------------------------------------------------------------------------------
Abstract
--------
This program parses a compiler list file and calculates the estimated stack usage of it.
It is build for assembler list files generated by ARM gcc in general and
XMC4-µControllers from Infineon in special with Cortex M4 architecture.
May be also functional for Infineon XMC1 / Cortex M0 (not extensively tested).


--------------------------------------------------------------------------------
How to use
----------
This is a GUI program; so
- simply start it
- select
  - Display mode:
    - Markdown table
    - hierarchical function list (call tree)
  - in the latter case (call tree): \n
    select max. call deepth shown
  - include functions (excluded by default):
    - with no stack usage
    - system calls (function beginning with an underline "_")

  Note that this affects only the display, not the calculation of stack usage!
- select display sort order:
  - alphabetic
  - by own stack usage
  - by deepest stack usage
- load an assembler list file (*.lst) file via button
- if display mode is "Markdown table":<br>
  save it as markdown table via save button

### Program output
The output consits of following columns:
- **Friendly Name:** function name in a friendly spelling<br>
  Compiler decorates function names in various ways; these decorations are
  deleted here to
  - make names more readable
  - have a better relation to C-Source-Code

  See column "Name" for original (decorated) name.
- **Own**: the function's stack usage
- **Deepest**: Deepest estimated stack usage
- **CallDeepth**: Deepth of call tree
- **CallCnt**: number of functions calling thes functions
- **Flags** to indicate some situations the stack usage couldn't calculated:
  - 'P': function uses pointers to call other functions
  - 'I': called functions are using indirect function calls, too
  - 'V': stack is manipulated by variable's values
  - 'R': function is called (directly or indirectly) recursively called by itself
- **Name**: Original name from assembler listing


--------------------------------------------------------------------------------
Build the program
-----------------
This program is written in Delphi using Embarcadero Rad Studio 10.3.3 / Common eddition.

To build the application simply load the project file into the IDE and compile
the configuration as you need (Win32/64, Debug/Build). An additional configuration
"Doxygen" is also available to build the documentation using [DelphDox-ALIASES](https://sys-thos.de/DelphiDox/).
This was tested with Doxygen V.1.9.1 but should run with older versions, too.


--------------------------------------------------------------------------------
Contact
-------

### Author
Dipl.Inf. Thomas Schneider (c) 2020-2021

Homepage: https://sys-thos.de

eMail:    Thomsa.Schneider@sys-thos.de

### Copyright
GPLv3 (https://www.gnu.org/licenses/gpl-3.0)

Doxygen dokumentation created using DelphiDox-ALIASES, details on\n
https://sys-thos.de/DelphiDox/


--------------------------------------------------------------------------------
Credits:
--------
Based on [MipsStaticStackAnalyzer](https://github.com/SentinelSw/MipsStaticStackAnalyzer)
by Florian Kaup, [Magnetic Sense GmbH](https://magnetic-sense.com/),
for mips gcc in general and xc32 from Microchip Technology Inc. in
special with MIPS32 release 5 target architecture.

