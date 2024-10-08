#!/bin/bash

find _build/default/test -name '*.output' -delete || true
dune runtest test/integration/ --force
exit $?
