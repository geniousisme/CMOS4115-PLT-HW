all:
	ocamllex wordcount.mll
	ocamlc -o wordcount wordcount.ml
	./wordcount < wordcount.mll

clean:
	rm -f wordcount *.cmi *.cmo *.ml