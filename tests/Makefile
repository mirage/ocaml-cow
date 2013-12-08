P4OPTS = $(shell ocamlfind query str dyntype dyntype.syntax re re.str ulex xmlm -predicates syntax,preprocessor,byte -r -format "-I %d %a")
OPTS   = $(shell ocamlfind query dyntype re re.str ulex oUnit uri xmlm omd -predicates archives,byte -r -format "-I %d %a")
TEST = render #tc_marshall

PP   = $(TEST:%=%_pp.ml)
CMO  = $(TEST:%=%_pp.cmo)

.PHONY: all

all: $(TEST) $(PP)
	@

%_pp.ml: %.ml ../_build/syntax/pa_cow.cma
	camlp4o $(P4OPTS) -I ../_build/syntax pa_cow.cma $< -o $@

%: %.ml ../_build/syntax/pa_cow.cma
	ocamlc -g -pp 'camlp4o $(P4OPTS) -I ../_build/syntax pa_cow.cma' $(OPTS) -I ../_build/lib cow.cma $^ -o $@

clean:
	rm -f $(TEST) $(PP)
