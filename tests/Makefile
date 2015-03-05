P4OPTS = $(shell ocamlfind query str dyntype dyntype.syntax re re.str ulex xmlm ezjsonm -predicates syntax,preprocessor,byte -r -format "-I %d %a")
LINK   = $(shell ocamlfind query dyntype re re.str ulex oUnit uri xmlm ezjsonm omd -predicates archives,byte -r -format "-I %d %a")
TESTS  = render extension
MAIN   = test

PP     = $(TESTS:%=%_pp.ml)
CMO    = $(TESTS:%=%.cmo)
INCL   = -I ../_build/lib cow.cma

.PHONY: all

all: $(TESTS:%=%_pp.cmo) $(MAIN)
	@

$(MAIN): $(MAIN).ml $(CMO) $(PP)
	ocamlfind ocamlc -package oUnit -linkpkg $(LINK) $(INCL) $(CMO)\
	  $(MAIN).ml -o $@

%_pp.ml: %.ml ../_build/syntax/pa_cow.cma
	camlp4o $(P4OPTS) -I ../_build/syntax pa_cow.cma $< -o $@ -printer o

%.cmo: %.ml ../_build/syntax/pa_cow.cma
	ocamlc -g -pp 'camlp4o $(P4OPTS) -I ../_build/syntax pa_cow.cma'\
	  $(LINK) $(INCL) $^

clean:
	rm -f $(TESTS) $(MAIN) *.cmo *.cmi *~ $(PP)
