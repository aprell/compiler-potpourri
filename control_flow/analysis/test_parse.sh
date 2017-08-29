#!/bin/bash

qtest -o test_parse.ml extract parse.ml
ocamlbuild -cflags -warn-error,+26 -use-ocamlfind -use-menhir -pkg oUnit,qcheck -build-dir _test test_parse.native
_test/test_parse.native

rm -f qtest.targets.log oUnit-anon.cache test_parse.ml
#rm -rf _test
