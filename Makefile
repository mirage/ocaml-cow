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
	echo 'true: warn_error(+1..49-3), warn(A-4-41-44)' >> _tags
#	echo 'Ocamlbuild_plugin.mark_tag_used "tests"' >> myocamlbuild.ml

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

RVERSION = $(shell grep 'Version:' _oasis | sed 's/Version: *//')
RNAME    = $(shell grep 'Name:' _oasis | sed 's/Name: *//')
RARCHIVE = https://github.com/mirage/ocaml-$(RNAME)/archive/v$(RVERSION).tar.gz

release:
	git tag -a v$(RVERSION) -m "Version $(RVERSION)."
	git push upstream v$(RVERSION)
	$(MAKE) pr

pr:
	opam publish prepare $(RNAME).$(RVERSION) $(RARCHIVE)
	opam publish submit $(RNAME).$(RVERSION) && rm -rf $(RNAME).$(RVERSION)

init-pages:
	mkdir -p doc/html
	cd doc/html && ( \
	  git init && \
	  git remote add origin git@github.com:mirage/ocaml-cow.git && \
	  git fetch && \
	  git checkout gh-pages && \
	  git pull)

gh-pages: doc
	rm -f doc/html/*.html
	cd doc/html && cp ../../cow.docdir/*.html .
	cd doc/html && git add * && git commit -a -m "Update docs"
	cd doc/html && git push
