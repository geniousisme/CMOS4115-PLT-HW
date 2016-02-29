objs = parser.cmo scanner.cmo float.cmo

all: scanner.ml parser.ml parser.mli parser.cmi scanner.cmo parser.cmo float.cmo float

parser.ml parser.mli:
	ocamlyacc parser.mly

%.cmi: %.mli
	ocamlc -c $<

%.cmo: %.ml
	ocamlc -c $<

scanner.ml: scanner.mll
	ocamllex $<

float: $(objs)
	ocamlc -o $@ $(objs)

clean:
	rm -f float parser.ml parser.mli scanner.ml *.cmo *.cmi