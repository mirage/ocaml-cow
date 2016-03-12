PREFIX ?= $(shell opam config var prefix)

SETUP = ocaml setup.ml

build: setup.data
	$(SETUP) -build $(BUILDFLAGS)

all: setup.data
	$(SETUP) -all $(ALLFLAGS)

setup.ml: _oasis
	rm -f _tags myocamlbuild.ml
	oasis setup
	echo 'true: debug, bin_annot' >> _tags
	echo 'true: warn_error(+1..49), warn(A-4-41-44)' >> _tags

doc: setup.data build
	$(SETUP) -doc $(DOCFLAGS)

test:
	$(SETUP) -configure --enable-tests --prefix $(PREFIX)
	$(MAKE) build
	$(SETUP) -test $(TESTFLAGS)

install: setup.data
	$(SETUP) -install $(INSTALLFLAGS)

uninstall: setup.data
	$(SETUP) -uninstall $(UNINSTALLFLAGS)

reinstall: setup.data
	$(SETUP) -reinstall $(REINSTALLFLAGS)

clean:
	if [ -f setup.ml ]; then $(SETUP) -clean $(CLEANFLAGS); fi
	rm -f setup.data setup.ml myocamlbuild.ml _tags configure
	rm -f lib/*.odocl lib/META setup.log lib/*.mldylib lib/*.mllib lib/*.mlpack
	rm -rf examples/*.byte examples/_tests
	rm -rf _tests

setup.data: setup.ml
	$(SETUP) -configure --prefix $(PREFIX)

RVERSION = $(shell grep 'VERSION=' _vars | sed 's/VERSION=//')
RNAME    = $(shell grep 'LIB=' _vars    | sed 's/LIB=//')
RARCHIVE = https://github.com/mirage/ocaml-$(RNAME)/archive/v$(RVERSION).tar.gz

release:
	git tag -a v$(RVERSION) -m "Version $(RVERSION)."
	git push upstream v$(RVERSION)
	$(MAKE) pr

pr:
	opam publish prepare $(RNAME).$(RVERSION) $(RARCHIVE)
	opam publish submit $(RNAME).$(RVERSION) && rm -rf $(RNAME).$(RVERSION)
