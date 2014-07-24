@ocamlc -a sample.mli sample.ml -o sample.cma
@..\odll -header sample.cma sample.cmi
@echo Compiling C sample.exe
@ocamlc sample.c
@link /nologo /OUT:sample.exe sample.lib sample.obj 