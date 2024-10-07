bold=$(shell tput bold)
normal=$(shell tput sgr0)

SRC_DIR := heptc/src
EXTRACTED := heptc/extraction/extracted

FLAGS=-use-ocamlfind -Is heptagon/compiler/ \
      -pkgs str,unix,menhirLib,ocamlgraph,js_of_ocaml,js_of_ocaml-ppx,js_of_ocaml-tyxml,js_of_ocaml-lwt,ezjs_ace \
	  -no-hygiene

SRC := \
	page.ml \
	tryhept.ml \
	pervasives.ml

all: tryhept.js pervasives.epci

tryhept.byte: $(SRC)
	@echo "${bold}Building tryhept...${normal}"
	ocamlbuild ${FLAGS} tryhept.byte
	@echo "${bold}Done.${normal}"

tryhept.js: tryhept.byte
	js_of_ocaml $^

pervasives.epci: heptagon/lib/pervasives.epi
	cd heptagon/lib && make
	cp heptagon/lib/pervasives.epci .

pervasives.ml: pervasives.epci embed_epci.ml
	ocaml embed_epci.ml $^ > $@

clean:
	rm -rf _build/ *.byte tryhept.js pervasives.epci

.PHONY:
	all clean extraction
