** OCam'OLE ReadME **
Updated 2003-11-15 : corrected olegen problem (with not supported type)
Updated 2003-10-22 : Build with OCaml 3.07

(c)2002 Nicolas Cannasse (ncannasse@motion-twin.com)
(c)2002 Motion-Twin
(c)2002 Lexifi

(You can get the latest version of this program at http://tech.motion-twin.com)

What is OCam'OLE ?
------------------

OCam'OLE is an OLE binding for OCaml. OLE is a COM-based technology that 
enable Automation. Automation is a way to create and manipulate remote
objects or applications that exposes COM interfaces.

To take an easy example, Excel, Word and most of today's big applications
installed on your computer can be controled by an Automation client. Visual
Basic is such a client, and can control an Excel application (this is called
"scripting"). Now with OCam'OLE, you can do the same, with the additional
feature of type-safe operations ( by using OLEGen ) !

How does it works ?
-------------------

Each OLE application have a Type-Library that contains list of objects. Each
object have a list of methods with type information on arguments and returned
values. OCam'OLE enable the browsing of such Type-Libraries for any
application.

What is OLEGen ?
----------------

OLEGen is an ocaml program that is using OCam'OLE to browse Type-Libraries and
generate the corresponding ML/MLI code. The code generated this way is hidding
OCam'OLE usage to call methods and is then providing a type-safe interface
to manipulate objects.

Installation
------------

To install this program, you need :
- ocaml 3.04 or better
- MSVC compiler & linker & nmake
- Excel installed to try the sample

The following files are included in this package:

- ocamole.cpp : C stubs for OCamole
- ocamole.ml : OCaml OLE interface
- olegen.ml : OLEGen sources
- excel1.ml : an Excel manipulation sample based on the generation
              of the Excel module with OLEGen
- dllocamole.dll : binary ocamole DLL ( for people who doesn't have MSVC)

To install, simply run "NMAKE" is the directory where you have put OCam'OLE.
This will generate the following files :

- dllocamole.dll + ocamole.cma : the OCam'OLE binaries
- olegen.exe : OLEGen binary, run it to get some help
- excel.cma : the Excel ocaml library
- excel1.exe : run it to test Excel control with OCaml

If you're experiencing some problems, check the following
environnement variables on your system :

- PATH : must include your Visual Studio /BIN directory
- LIB : must include Visual Studio /LIB directory
        and your OCaml distribution /LIB directory
- INCLUDE : must include Visual Studio /INCLUDE directory
            and your OCaml distribution /LIB directory

If you don't have Excel installed, you can run "NMAKE NOSAMPLE" to build
only OCam'OLE & OLEGen binaries.

NEW : added native code compilation with Pre.2. Olegen.exe is working now
but a Stack overflow bug in ocamlopt (3.05) prevents from compiling excel.ml


How do I use it ?
-----------------

You can now do the following :
- use "raw" Automation by directly using OCam'OLE
- first generate ML/MLI code for the application you want to use by
using OLEGen and then watch the generated MLI to manipulate it.

Licence
-------

OCam'OLE and OLEGen are under GPL.
Contact the author for more informations

Author / Copyrights
-------------------

This software is copyrighted by the followings :

- The author : Nicolas Cannasse ( ncannasse@motion-twin.com )
- The author's company : Motion-Twin
- This software wouldn't exists without : Lexifi

Thanks to Jean-Marc Eber & Xavier Leroy
And especially to Lexifi for supporting this project
