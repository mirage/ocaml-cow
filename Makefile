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
