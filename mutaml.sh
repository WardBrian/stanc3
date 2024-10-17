#!/bin/bash

rm -rf _build/.lock || true
find _build/default/test -name '*.output' -delete || true
dune runtest test/integration/ --force -j 12
exit $?
