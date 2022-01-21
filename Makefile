bold=$(shell tput bold)
normal=$(shell tput sgr0)

SRC_DIR := heptc/src
EXTRACTED := heptc/extraction/extracted

FLAGS=-use-ocamlfind -Is heptagon/compiler/ \
      -pkgs str,unix,menhirLib,ocamlgraph,js_of_ocaml,js_of_ocaml-ppx,js_of_ocaml-tyxml,ezjs_ace \
	  -no-hygiene

SRC := \
	page.ml \
	tryhept.ml

all: tryhept.js

tryhept.byte: $(SRC)
	@echo "${bold}Building tryhept...${normal}"
	ocamlbuild ${FLAGS} tryhept.byte
	@echo "${bold}Done.${normal}"

tryhept.js: tryhept.byte
	js_of_ocaml $^

clean:
	rm -rf _build/ *.byte tryhept.js

.PHONY:
	all clean extraction
