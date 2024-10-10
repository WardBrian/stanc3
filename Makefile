all:
	dune build src/stanc/stanc.exe

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

mutaml-report.json:
	@rm -rf _mutations
	dune clean
	dune build src/stanc --instrument-with mutaml --force
	@rm -rf ./stanc.exe
	cp _build/default/src/stanc/stanc.exe .
	mutaml-runner "./mutaml.sh"

mutation-test: mutaml-report.json
	mutaml-report

format:
	dune build @fmt

cross:
	dune build src/stanc/stanc.exe -x windows

static:
	dune build src/stanc/stanc.exe --profile static

clean:
	dune clean

doc:
	dune build @doc

re: clean all
