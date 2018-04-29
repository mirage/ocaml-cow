.PHONY: all test clean

all:
	jbuilder build --dev

test:
	jbuilder runtest

clean:
	jbuilder clean
