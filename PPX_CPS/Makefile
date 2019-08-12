# Nome de ficheiros
NATIVE = ppxCPS
FILE = Testes/test
DUMP = Dump
RESULT = Resultados/result

# Comandos
clean = rm -f Resultados/*.cm[io] Resultados/*.o
native = ocamlbuild -ocamlopt 'ocamlopt str.cmxa' -package compiler-libs.common  $(NATIVE).native  
build = ocamlfind ppx_tools/rewriter ./$(NATIVE).native  $(FILE).ml > $(RESULT).ml
dump = ocamlfind ppx_tools/dumpast $(FILE).ml > $(DUMP).ml
exec = ocamlc -o $(RESULT) $(RESULT).ml  && ./$(RESULT)
show = cat $(RESULT).ml

all:
	$(clean) 
	$(native)  	
	$(build)
	$(dump)
	$(exec)

native: 
	$(native)
build: 
	$(build)
dump: 
	$(dump)
exec: 
	$(exec)
show: 
	$(show)
clean:
	$(clean)


