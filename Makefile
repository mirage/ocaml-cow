.PHONY: all clean depend install

all:
	./cmd configure
	./cmd build

install:
	./cmd install

clean:
	./cmd clean
	$(MAKE) -C tests clean

tests: all
	$(MAKE) -C tests clean
	$(MAKE) -C tests
	tests/test

VERSION = $(shell grep 'VERSION=' _vars | sed 's/VERSION=//')
NAME    = $(shell grep 'LIB=' _vars    | sed 's/LIB=//')
ARCHIVE = https://github.com/mirage/ocaml-$(NAME)/archive/v$(VERSION).tar.gz

release:
	git tag -a v$(VERSION) -m "Version $(VERSION)."
	git push upstream v$(VERSION)
	$(MAKE) pr

pr:
	opam publish prepare $(NAME).$(VERSION) $(ARCHIVE)
	opam publish submit $(NAME).$(VERSION) && rm -rf $(NAME).$(VERSION)

doc: all
	ocamlfind ocamldoc -package xmlm,ezjsonm,uri -I _build -I _build/lib -html -d doc lib/xml.mli lib/css.mli lib/html.mli lib/json.mli lib/markdown.mli lib/atom.mli lib/code.mli lib/xhtml.mli
