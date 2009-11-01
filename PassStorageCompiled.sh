#!/bin/sh

sbcl --noinform --core PassStorage.mem --end-runtime-options --eval "(progn (main) (quit))"
#clisp -q -M PassStorage.mem -x "(progn (main) (quit))"
