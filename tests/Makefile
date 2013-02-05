P4OPTS = $(shell ocamlfind query str dyntype dyntype.syntax re re.str ulex -predicates syntax,preprocessor,byte -r -format "-I %d %a")
OPTS   = $(shell ocamlfind query dyntype re re.str ulex ounit -predicates archives,byte -r -format "-I %d %a")
TEST = test

PP   = $(TEST:%=%_pp.ml)
CMO  = $(TEST:%=%_pp.cmo)

.PHONY: all

all: $(TEST) $(PP)
	@

%_pp.ml: %.ml ../_build/syntax/pa_cow.cma
	camlp4o $(P4OPTS) -I ../_build/syntax pa_cow.cma $< -o $@

%: %_pp.ml
	ocamlc $(OPTS) -I ../_build/lib cow.cma $^ -o $@