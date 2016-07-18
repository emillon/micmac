OCAMLBUILD=ocamlbuild -use-ocamlfind
.PHONY: build
build:
	$(OCAMLBUILD) rogue.native

.PHONY: watch
watch:
	while true; do inotifywait -qe close_write *.ml *.mli _tags ; clear ; $(MAKE) build ; done

.PHONY: clean
clean:
	$(OCAMLBUILD) -clean
