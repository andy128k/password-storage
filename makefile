core: core-sbcl

core-sbcl:
	sbcl --eval "(progn (asdf:oos 'asdf:load-op :zlib) (asdf:oos 'asdf:load-op :cl-fad) (asdf:oos 'asdf:load-op :cl-gtk2-gtk) (asdf:oos 'asdf:load-op :ironclad) (sb-ext:save-lisp-and-die \"sbcl-cl-gtk2\" :executable t))"

core-ecl:
	ecl -eval "(progn (push #p\"/home/andy/.sbcl/systems/\" asdf:*central-registry*) (asdf:oos 'asdf:load-op :cl-gtk2-gtk) (asdf:oos 'asdf:load-op :ironclad) (sb-ext:save-lisp-and-die \"sbcl-cl-gtk2\" :executable t))"

core-clozure64:
	CCL_DEFAULT_DIRECTORY=/home/andy/apps/ccl /home/andy/apps/ccl/lx86cl64 --eval "(progn (asdf:oos 'asdf:load-op :zlib) (asdf:oos 'asdf:load-op :cl-fad) (asdf:oos 'asdf:load-op :cl-gtk2-gtk) (asdf:oos 'asdf:load-op :ironclad) (ccl:save-application \"clozure-cl-gtk2\" :prepend-kernel t))"

