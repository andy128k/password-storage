core: core-sbcl-linux-any

core-sbcl-linux-any:
	sbcl --eval "(progn (asdf:oos 'asdf:load-op :zlib) (asdf:oos 'asdf:load-op :cl-fad) (asdf:oos 'asdf:load-op :cl-gtk2-gtk) (asdf:oos 'asdf:load-op :ironclad) (sb-ext:save-lisp-and-die \"sbcl-cl-gtk2\" :executable t))"

core-ecl:
	ecl -eval "(progn (push #p\"/home/andy/.sbcl/systems/\" asdf:*central-registry*) (asdf:oos 'asdf:load-op :cl-gtk2-gtk) (asdf:oos 'asdf:load-op :ironclad) (sb-ext:save-lisp-and-die \"sbcl-cl-gtk2\" :executable t))"

core-clozure-linux-64:
	CCL_DEFAULT_DIRECTORY=/home/andy/apps/ccl /home/andy/apps/ccl/lx86cl64 --eval "(progn (asdf:oos 'asdf:load-op :zlib) (asdf:oos 'asdf:load-op :cl-fad) (asdf:oos 'asdf:load-op :cl-gtk2-gtk) (asdf:oos 'asdf:load-op :ironclad) (ccl:save-application \"clozure-cl-gtk2\" :prepend-kernel t))"

core-clozure-windows-32:
	wx86cl --eval "(progn (asdf:oos 'asdf:load-op :zlib) (asdf:oos 'asdf:load-op :cl-fad) (asdf:oos 'asdf:load-op :cl-gtk2-gtk) (asdf:oos 'asdf:load-op :ironclad) (ccl:save-application \"clozure-cl-gtk2.exe\" :prepend-kernel t))"



ps-sbcl-linux-any:
	sbcl --eval "(progn (push #p\"/home/andy/projects/PassStorage/\" asdf:*central-registry*) (asdf:oos 'asdf:load-op :pass-storage))" --eval "(sb-ext:save-lisp-and-die \"PassStorage\" :executable t :toplevel 'pass-storage::main-and-quit)"

ps-sbcl-windows-32:
	sbcl --eval "(progn (push #p\"/home/andy/projects/PassStorage/\" asdf:*central-registry*) (asdf:oos 'asdf:load-op :pass-storage))" --eval "(sb-ext:save-lisp-and-die \"PassStorage.exe\" :executable t :toplevel 'pass-storage::main-and-quit)"

ps-clozure-linux-64:
	CCL_DEFAULT_DIRECTORY=/home/andy/apps/ccl /home/andy/apps/ccl/lx86cl64 --eval "(progn (push #p\"/home/andy/projects/PassStorage/\" asdf:*central-registry*) (asdf:oos 'asdf:load-op :pass-storage))" --eval "(ccl:save-application \"PassStorage\" :prepend-kernel t :toplevel-function 'pass-storage::main-and-quit)"

ps-clozure-windows-32:
	wx86cl --eval "(progn (push #p\"/home/andy/projects/PassStorage/\" asdf:*central-registry*) (asdf:oos 'asdf:load-op :pass-storage))" --eval "(ccl:save-application \"PassStorage.exe\" :prepend-kernel t :toplevel-function 'pass-storage::main-and-quit)"
