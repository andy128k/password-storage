core:
	sbcl --eval "(progn (asdf:oos 'asdf:load-op :cl-gtk2-gtk) (asdf:oos 'asdf:load-op :ironclad) (sb-ext:save-lisp-and-die \"sbcl-cl-gtk2\" :executable t))"

