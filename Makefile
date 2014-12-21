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
	tests/render

VERSION = $(shell grep 'VERSION=' _vars | sed 's/VERSION=//')
NAME    = $(shell grep 'LIB=' _vars    | sed 's/LIB=//')
ARCHIVE = https://github.com/mirage/ocaml-$(NAME)/archive/$(VERSION).tar.gz

release:
	git tag -a $(VERSION) -m "Version $(VERSION)."
	git push upstream $(VERSION)
	$(MAKE) pr

pr:
	opam publish prepare $(NAME).$(VERSION) $(ARCHIVE)
	opam publish submit $(NAME).$(VERSION) && rm -rf $(NAME).$(VERSION)
