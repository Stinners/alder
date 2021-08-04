.PHONY: build
build:
	idris2 --build alder.ipkg
	idris2 src/Main.idr -o adler

run:  
	./build/exec/adler
