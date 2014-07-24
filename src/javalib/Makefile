#
#  This file is part of JavaLib
#  Copyright (c)2004 Nicolas Cannasse
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  You should have received a copy of the GNU General Public License
#  along with this program; if not, write to the Free Software
#  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
#

INSTALL_DIR = `ocamlc -where`/javaLib
EXTLIB_PATH = +extLib

# ------ 

.SUFFIXES : .cmo .cmx .cmi .ml .mli

FILES = jCode.+ jParse.+ jDump.+

all: javaLib.cma sample

opt: javaLib.cmxa sample.opt

install: all opt
	-mkdir $(INSTALL_DIR)
	cp -f javaLib.cma javaLib.cmxa javaLib.a jClass.cmi jParse.cmi jCode.cmi jDump.cmi $(INSTALL_DIR)
	cp -f jClass.mli jCode.mli jParse.mli jDump.mli $(INSTALL_DIR)

sample:
	ocamlc -I $(EXTLIB_PATH) extLib.cma javaLib.cma sample.ml -o sample.exe

sample.opt:
	ocamlopt -I $(EXTLIB_PATH) extLib.cmxa javaLib.cmxa sample.ml -o sample.opt.exe

javaLib.cma: jClass.cmi $(FILES:+=cmi) $(FILES:+=cmo)
	ocamlc -a $(FILES:+=cmo) -o javaLib.cma

javaLib.cmxa: jClass.cmi $(FILES:+=cmi) $(FILES:+=cmx)
	ocamlopt -a $(FILES:+=cmx) -o javaLib.cmxa

clean:
	rm -rf $(FILES:+=cmo) $(FILES:+=cmx) $(FILES:+=cmi) $(FILES:+=o) $(FILES:+=obj)
	rm -rf sample.cmo sample.cmi sample.cmx sample.o sample.obj sample.exe sample.opt.exe 
	rm -rf jClass.cmi javaLib.cma javaLib.cmxa javaLib.lib javaLib.a
	

.ml.cmo:
	ocamlc -I $(EXTLIB_PATH) -c $*.ml

.ml.cmx:
	ocamlopt -I $(EXTLIB_PATH) -c $*.ml

.mli.cmi:
	ocamlc -I $(EXTLIB_PATH) -c $*.mli