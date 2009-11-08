#!/bin/sh

./sbcl-cl-gtk2 --noinform --eval "(progn (load \"PassStorage.lisp\") (main) (quit))"
#clisp -q -x "(progn (load \"PassStorage.lisp\") (main) (quit))"
