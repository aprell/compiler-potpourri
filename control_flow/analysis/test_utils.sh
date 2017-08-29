#!/bin/bash

qtest -o test_utils.ml extract utils.ml
ocamlbuild -cflags -warn-error,+26 -use-ocamlfind -pkg oUnit,qcheck -build-dir _test test_utils.native
_test/test_utils.native

rm -f qtest.targets.log oUnit-anon.cache test_utils.ml
#rm -rf _test
