#!/bin/sh

# defaults
LISP=sbcl

usage () {
    echo "Usage: $0 [-l <common lisp implementation>] [...]"
    echo ""
    echo "Supported Common Lisp implementations:"
    echo "    sbcl (default)"
    echo "    ccl"
    echo ""
    exit $1
}

# parse parameters
while getopts "hl:" opt
do
    case $opt in
	l)
	    LISP="$OPTARG"
	    ;;
	h)
	    usage 0
	    ;;
    esac
done
shift $(($OPTIND - 1))

# detect platform
case `uname -s` in
    Linux)
	EXECUTABLE_SUFFIX=
	;;
    MINGW32*)
	EXECUTABLE_SUFFIX=.exe
	;;
    *)
        echo 'Unsupported platform' >&2
        exit 1
	;;
esac

case $LISP in
    sbcl|ccl)
	EVAL_KEY=--eval
	;;
    *)
	echo "Unknown Common Lisp implementation." >&2
	usage 1
	;;
esac

./lisp-cl-gtk2$EXECUTABLE_SUFFIX \
    $EVAL_KEY "(pushnew #p\"./\" asdf:*central-registry*)" \
    $EVAL_KEY "(asdf:load-system :pass-storage)" \
    $EVAL_KEY "(pass-storage:main-and-quit)" \
    $*

