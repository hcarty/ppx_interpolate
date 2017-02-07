all:
	ocamlbuild src/ppx_interp.native
	ocamlbuild -cflags -ppx,./src/ppx_interp.native x.native

clean:
	ocamlbuild -clean
