
generate: 
	cd scripts; python3 make_tags.py > TagList.idr
	mv scripts/TagList.idr src/

.PHONY: build
build:
	idris2 --build alder.ipkg
	idris2 src/Main.idr -o adler

.PHONY: build
js:
	idris2 --build alder.ipkg
	idris2 --codegen javascript src/Main.idr -o alder.js

run:  
	./build/exec/adler
