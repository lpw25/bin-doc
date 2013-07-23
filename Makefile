
BINDIR=./bin

OCAMLC=ocamlc
OCAMLLINK=ocamlc
OCAMLLEX=ocamllex
OCAMLYACC=ocamlyacc

OCAMLCFLAGS=-g -I +compiler-libs #-annot
OCAMLLINKFLAGS=-g -I +compiler-libs ocamlcommon.cma
OCAMLLEXFLAGS=
OCAMLYACCFLAGS=

%.ml: %.mll
	${OCAMLLEX} ${OCAMLLEXFLAGS} $<

%.ml %.mli: %.mly
	${OCAMLYACC} ${OCAMLYACCFLAGS} $<

%.cmi: %.mli
	${OCAMLC} ${OCAMLCFLAGS} -c $<

%.cmo %.cmi: %.ml
	${OCAMLC} ${OCAMLCFLAGS} -c $<

.PRECIOUS: info_lexer.ml info_parser.ml info_parser.mli

all: bin-doc

clean:
	-rm *.cmi *.cmo
	-rm info_lexer.ml
	-rm info_parser.ml
	-rm info_parser.mli
	-rm bin-doc

install: all
	-mkdir -p ${BINDIR}
	cp bin-doc ${BINDIR}/bin-doc

bin-doc: info.cmo errors.cmo doctree.cmo printdoctree.cmo info_parser.cmo info_lexer.cmo comments.cmo inlinedoc.cmo driver.cmo 
	${OCAMLLINK} ${OCAMLLINKFLAGS} -o $@ $^

info.cmo: info.cmi

errors.cmo: errors.cmi

doctree.cmo: info.cmo doctree.cmi

printdoctree.cmo: printdoctree.cmi info.cmo doctree.cmo inlinedoc.cmo

info_parser.cmo: info.cmo info_parser.cmi

info_lexer.cmo: info.cmo info_parser.cmo info_lexer.cmi

comments.cmo: comments.cmi

inlinedoc.cmo: doctree.cmo info_parser.cmo info_lexer.cmo comments.cmo inlinedoc.cmi

driver.cmo: doctree.cmo inlinedoc.cmo printdoctree.cmo


doctree.cmi: info.cmi

printdoctree.cmi: info.cmo doctree.cmo

info_parser.cmi: info.cmi

info_lexer.cmi: info_parser.cmi

inlinedoc.cmi: doctree.cmi



