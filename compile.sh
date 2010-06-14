#!/bin/sh

# defaults
LISP=sbcl
TARGET=core

function usage () {
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
	;;
    *)
	echo "Unknown Common Lisp implementation." >&2
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
        echo 'Unsupported platform'
        exit 1
	;;
esac

function make_target () {
    PATHS=$1
    PACKAGES=$2
    TARGET_NAME=$3
    ENTRY_POINT=$4

    EV="(progn "
    for path in $PATHS
    do
	EV=`echo "$EV (push #p\"$path\" asdf:*central-registry*)"`
    done

    for package in $PACKAGES
    do
	EV=`echo "$EV (asdf:oos 'asdf:load-op :$package)"`
    done
    EV=`echo "$EV)"`

    case $LISP in
	sbcl)
	    if [ -z $ENTRY_POINT ]
	    then
		EV2=`echo "(sb-ext:save-lisp-and-die \"$TARGET_NAME$EXECUTABLE_SUFFIX\" :executable t)"`
	    else
		EV2=`echo "(sb-ext:save-lisp-and-die \"$TARGET_NAME$EXECUTABLE_SUFFIX\" :executable t :toplevel '$ENTRY_POINT)"`
	    fi
	    $LISP_EXECUTABLE --eval "$EV" --eval "$EV2"
	    ;;
	ccl)
	    if [ -z $ENTRY_POINT ]
	    then
		EV2=`echo "(ccl:save-application \"$TARGET_NAME$EXECUTABLE_SUFFIX\" :prepend-kernel t)"`
	    else
		EV2=`echo "(ccl:save-application \"$TARGET_NAME$EXECUTABLE_SUFFIX\" :prepend-kernel t :toplevel-function '$ENTRY_POINT)"`
	    fi
	    $LISP_EXECUTABLE --eval "$EV" --eval "$EV2"
	    ;;
    esac
}

case $TARGET in
    core)
	make_target "" "cl-z cl-fad ironclad cl-gtk2-gdk sb-queue cl-gtk2-gtk" lisp-cl-gtk2
	;;
    ps)
	make_target /home/andy/projects/PassStorage/ pass-storage PassStorage pass-storage::main-and-quit
	;;
esac

