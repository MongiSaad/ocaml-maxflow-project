.PHONY: all build format edit demo clean

src?=1
dst?=8
graph?=moneysharing2.txt

all: build

build:
	@echo "\n   🚨  COMPILING  🚨 \n"
	dune build src/ftest.exe
	ls src/*.exe > /dev/null && ln -fs src/*.exe .

format:
	ocp-indent --inplace src/*

edit:
	code . -n

demo: build
	@echo "\n   ⚡  EXECUTING  ⚡\n"
	./ftest.exe graphs/${graph} $(src) $(dst) outfile
	@echo "\n   🥁  RESULT (content of outfile)  🥁\n"
	@cat outfile
	@dot -Tpng outfile -o outfile.png
	@display outfile.png

clean:
	find -L . -name "*~" -delete
	rm -f *.exe
	dune clean
