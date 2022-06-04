.PHONY: build test doc clean all-supported-ocaml-versions

all:
	dune build @all

build:
	dune build

test:
	dune runtest

doc:
	dune build @doc

clean:
	dune clean

all-supported-ocaml-versions:
	dune build @install @runtest --workspace dune-workspace.dev
