# Makefile for CLISP regression tests
# ifneq/else/endif requires GNU Make, so we cannot use them

# build directory
BD=..
# executable extension (.exe on win32, .run everywhere else)
LEXE=.run

LISP=$(BD)/lisp$(LEXE) -E utf-8 -norc -B $(BD) -N $(BD)/locale -M $(BD)/lispinit.mem
# LISP=../clisp -norc
# LISP=clisp -norc

RM = rm -f

all : clean tests compare

clean : force
	$(RM) *.erg

tests : force
	$(LISP) -C -i tests.lisp -x '(time (run-all-tests))'

complete : force
	$(LISP) -C -i tests.lisp -x '(time (run-all-tests nil))'

compare : force
	(echo *.erg | grep '*' >/dev/null) || (echo "Test failed:" ; ls -l *erg; echo "To see which tests failed, type" ; echo "    cat "`pwd`"/*.erg" ; exit 1)
	echo "Test passed."

%.erg: %.tst $(BD)/lispinit.mem $(BD)/lisp$(LEXE)
	$(LISP) -i tests.lisp -x '(run-test "$<")'

force :

