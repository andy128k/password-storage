DUMP=PassStorage.mem

SCRIPT="(progn (load \"PassStorage.lisp\") (gc) \#+clisp (ext:saveinitmem \"PassStorage.mem\") \#+sbcl (sb-ext:save-lisp-and-die \"PassStorage.mem\") (quit))"

compile: $(DUMP)

build: clean compile

$(DUMP): PassStorage.lisp
	sbcl --noinform --eval $(SCRIPT)
#	clisp -x $(SCRIPT)

core:
	sbcl --eval "(progn (asdf:oos 'asdf:load-op :cl-gtk2-gtk) (sb-ext:save-lisp-and-die \"sbcl-cl-gtk2\" :executable t))"

