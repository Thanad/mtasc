ODLL v1.0
==========
Last Version: http://tech.motion-twin.com

What is ODLL ?
--------------

If you're having a boss asking you to write C librairies but if you prefer to write them in OCaml, this tool is for you ! ODLL is creating a Win32 DLL from an OCaml library, and automatically generating the C stubs needed to interface the OCaml code and the C code and the .H interface. Simply run ODLL with you CMA/CMXA and one or several CMI interfaces and ODLL will do the job for you.

Contents of the distribution :
------------------------------

README.txt        : this file
odll.exe          : precompiled ODLL Win32 binary
src/Makefile      : the ODLL Makefile
src/odll.ml       : ODLL sources
sample/sample.ml  : a sample OCaml source code
sample/sample.mli : a sample OCaml interface
sample/sample.c   : a sample C source code
sample/make.bat   : compile sample.exe

Installation:
-------------

Since ODLL require one part of the OCaml sources, you'll need to get them from the INRIA CVS if you want to recompile ODLL. For convenience, a precompiled version is included in the distribution, which only require OCaml to be installed.
You only need the odll.exe file to use odll.
A sample is provided in the /sample directory.

Usage:
------

- First, create an OCaml library (CMA or CMXA) using ocaml compilers (ocamlc or ocamlopt).
- Then, write one or several MLI interfaces for the OCaml functions you want to export into the DLL. Currently only the following basic ocaml types are supported : unit, int, float, string, char, bool and arrays.
- Compile your interfaces into CMI files using Ocamlc.
- run ODLL -o mylib.dll myfile.cm[x]a myintf1.cmi myintf2.cmi ... to create the DLL.
   The following flags are provided :
   -header : generate a .H header file containing exported functions.
   -v : verbose mode, print commands run by ODLL.
   -keep : (debug only) do no delete intermediate files produced by ODLL.
- you can now write your C code using the DLL produced by ODLL, see the sample.c file for more informations.

Credits:
-------
Author : Nicolas Cannasse
ncannasse@motion-twin.com

(c)2003 Motion-Twin
(c)2003 Lexifi