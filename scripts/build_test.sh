#!/bin/bash
cd src/ && ocamlbuild -use-menhir -tag thread -use-ocamlfind -quiet -pkg core test.native
