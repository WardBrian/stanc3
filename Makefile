all:
	dune build

.PHONY: doc test

test:
	dune runtest

testcoverage:
	@find . -name '*.coverage' | xargs rm -f
	dune clean
	BISECT_FILE=`pwd`/bisect dune runtest --instrument-with bisect_ppx --force
	bisect-ppx-report html --expect src/ --do-not-expect src/stancjs/
	bisect-ppx-report summary --expect src/ --do-not-expect src/stancjs/
	@rm *.coverage

format:
	dune build @fmt

cross:
	dune build -x windows

static:
	dune build --profile static

clean:
	dune clean

doc:
	dune build @doc

re: clean all
