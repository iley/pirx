.PHONY: default build test clean install-pirxc

PIRXC_BIN := _build/default/bin/pirxc/main.exe
PIRXC_INSTALL := ../pirxc

default: build install-pirxc

build:
	dune build

test:
	dune runtest

clean:
	dune clean

install-pirxc: build
	cp $(PIRXC_BIN) $(PIRXC_INSTALL)
