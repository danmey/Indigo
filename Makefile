all:
	ocamlbuild -use-ocamlfind indigo.byte.otarget
	ocamlbuild -use-ocamlfind indigo.native.otarget

clean:
	ocamlbuild -clean
