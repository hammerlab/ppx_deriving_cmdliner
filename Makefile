build:
	cp pkg/META.in pkg/META
	ocaml pkg/build.ml native=true native-dynlink=true

clean:
	ocamlbuild -clean

.PHONY: build test doc clean

VERSION      := $$(opam query --version)
NAME_VERSION := $$(opam query --name-version)
ARCHIVE      := $$(opam query --archive)

release:
	git tag -a v$(VERSION) -m "Version $(VERSION)."
	git push origin v$(VERSION)
	opam publish prepare $(NAME_VERSION) $(ARCHIVE)
	opam publish submit $(NAME_VERSION)
	rm -rf $(NAME_VERSION)

TEST_PACKAGES=ppx_deriving_cmdliner,ppx_driver,ppx_deriving,ppx_deriving.show,ppx_deriving.main,ppx_tools.metaquot,ppx_deriving.api,ppx_deriving.runtime,alcotest
tests:
	ocamlbuild -use-ocamlfind -tag thread -I test/ -I src/ \
	  -pkgs $(TEST_PACKAGES) tests.native
	./tests.native

.PHONY: release test tests
