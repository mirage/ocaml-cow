.PHONY: all clean depend install

all:
	@./cmd configure
	@./cmd build

install:
	@./cmd install

clean:
	@./cmd clean
	make -C tests clean

tests: all
	make -C tests clean
	make -C tests
	tests/render
