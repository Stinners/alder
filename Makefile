
generate: 
	cd scripts; python3 make_tags.py > TagList.idr
	mv scripts/TagList.idr src/

.PHONY: build
build:
	idris2 --build alder.ipkg
	idris2 src/Main.idr -o adler

run:  
	./build/exec/adler
