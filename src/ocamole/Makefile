#############################
# OCam'OLE Makefile         #
#############################

.SUFFIXES : .ml .mli .cmo .cmx

LIBS=ocamlrun.lib ole32.lib oleaut32.lib
LFLAGS=/NOLOGO
CFLAGS=/c /W3 /YX /nologo

#############################
# TARGETS
#############################
# BYTECODE 

all: ocamole.cma olegen.exe excel1.exe

dllocamole.dll: ocamole.dobj
    -@del $@ 2>NUL
    link $(LIBS) $(LFLAGS) /dll /out:$@ $**

ocamole.cma: dllocamole.dll ocamole.cmo
    ocamlc -o $@ -a ocamole.cmo -dllib -locamole

olegen.exe: ocamole.cma olegen.cmo
    ocamlc -o $@ $**

excel.ml: olegen.exe
    olegen Excel.Application -o excel

excel.mli: excel.ml

excel.cmo: excel.cmi

excel.cma: excel.cmo
    ocamlc -a -o $@ $**

excel1.exe: ocamole.cma excel.cma excel1.cmo
    ocamlc -o $@ $**

#############################
# OPT

opt: ocamole.cmxa olegen.opt.exe excel1.opt.exe

libocamole.lib: ocamole.sobj
    -@del $@ 2>NUL
    lib $(LFLAGS) /out:$@ $**

ocamole.cmxa: libocamole.lib ocamole.cmx
    ocamlopt -o $@ -a $**

olegen.opt.exe: ocamole.cmxa olegen.cmx
    ocamlopt -o $@ $**

excel.cmx: excel.cmi
    @echo ** This may cause Stack Overflow with 3.05 (ocamlopt bug reported) **
    ocamlopt -c $*.ml

excel.cmxa: excel.cmx
    ocamlopt -a -o $@ $**

excel1.opt.exe: ocamole.cmxa excel.cmxa excel1.cmx
    ocamlc -o $@ $**

#############################
# CLEAN

clean:
    -@del dllocamole.* ocamole.dobj vc60.pch 2>NUL
    -@del ocamole.cm* 2>NUL
    -@del olegen.cmo olegen.cmi olegen.exe 2>NUL
    -@del excel.ml excel.mli excel.cmi excel.cmo excel.cma 2>NUL
    -@del excel1.cmi excel1.cmo excel1.exe 2>NUL
    -@del libocamole.lib ocamole.obj olegen.obj ocamole.lib ocamole.sobj 2>NUL
    -@del olegen.cmx olegen.opt.exe 2>NUL
    -@del excel1.cmx excel.cmx excel1.opt.exe 2>NUL

#############################
# SUFFIXES
#############################

.cpp.sobj :
    cl $(CFLAGS) /Fo$@ $*.cpp

.cpp.dobj :
    cl $(CFLAGS) /Fo$@ /DCAML_DLL $*.cpp

.ml.cmo :
    ocamlc -c $*.ml

.ml.cmx :
    ocamlopt -c $*.ml

.mli.cmi:
    ocamlc $*.mli
