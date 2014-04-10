
sim:
	ocamlbuild -use-ocamlfind -pkgs camlzip -cflag -g -lib str fastsim.native
	cp fastsim.native fastsim

clean:
	rm -rf _build fastsim.native fastsim
