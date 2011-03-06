#!/bin/sh

# defaults
LISP=sbcl
TARGET=core

usage () {
    echo "Usage: $0 [-l <common lisp implementation>] [-t <target>]"
    echo ""
    echo "Supported Common Lisp implementations:"
    echo "    sbcl (default)"
    echo "    ccl"
    echo ""
    echo "Known targets:"
    echo "    core (default)"
    echo "    ps"
    echo ""
    exit $1
}

# parse parameters
while getopts "hl:t:" opt
do
    case $opt in
	l)
	    LISP="$OPTARG"
	    ;;
	t)
	    TARGET="$OPTARG"
	    ;;
	h)
	    usage 0
	    ;;
	\?)
	    usage 1
	    ;;
    esac
done
shift $(($OPTIND - 1))
if [ "x$*" != "x" ]
then
    usage 1
fi

# validate lisp
case "$LISP" in
    sbcl|ccl)
	EVAL_KEY=--eval
	;;
    *)
	echo "Unknown Common Lisp implementation." >&2
	exit 1
	;;
esac

# detect platform
case `uname -s` in
    Linux)
        PLATFORM=linux
        EXECUTABLE_SUFFIX=
        case $LISP in
	    sbcl)
		LISP_EXECUTABLE=sbcl
		;;
	    ccl)
		LISP_EXECUTABLE=ccl
		;;
	esac
	;;
    MINGW32*)
        PLATFORM=win32
	EXECUTABLE_SUFFIX=.exe
	case $LISP in
	    sbcl)
		LISP_EXECUTABLE=sbcl
		;;
	    ccl)
		LISP_EXECUTABLE=wx86cl
		;;
	esac
	;;
    *)
        echo 'Unsupported platform' >&2
        exit 1
	;;
esac

make_target () {
    PATHS=$1
    PACKAGES=$2
    TARGET_NAME=$3
    ENTRY_POINT=$4
    
    EV="(progn "
    for path in $PATHS
    do
	EV="$EV (push #p\"$path\" asdf:*central-registry*)"
    done

    for package in $PACKAGES
    do
    	EV="$EV (asdf:load-system :$package)"
    done
    EV="$EV)"

    case $LISP in
	sbcl)
	    if [ -z $ENTRY_POINT ]
	    then
    		EV2="(sb-ext:save-lisp-and-die \"$TARGET_NAME$EXECUTABLE_SUFFIX\" :executable t)"
	    else
		EV2="(sb-ext:save-lisp-and-die \"$TARGET_NAME$EXECUTABLE_SUFFIX\" :executable t :toplevel '$ENTRY_POINT)"
	    fi
	    ;;
	ccl)
	    if [ -z $ENTRY_POINT ]
	    then
    		EV2="(ccl:save-application \"$TARGET_NAME$EXECUTABLE_SUFFIX\" :prepend-kernel t)"
	    else
		EV2="(ccl:save-application \"$TARGET_NAME$EXECUTABLE_SUFFIX\" :prepend-kernel t :toplevel-function '$ENTRY_POINT)"
	    fi
	    ;;
    esac
    $LISP_EXECUTABLE $EVAL_KEY "$EV" $EVAL_KEY "$EV2"
}

case $TARGET in
    core)
#	make_target "" "cl-z cl-fad ironclad cl-gtk2-gdk sb-queue cl-gtk2-gtk" lisp-cl-gtk2
	make_target "" "cl-z cl-fad ironclad cl-gtk2-gdk cl-gtk2-gtk" lisp-cl-gtk2
	;;
    ps)
	make_target /home/andy/projects/PassStorage/ pass-storage PassStorage pass-storage::main-and-quit
	;;
esac

