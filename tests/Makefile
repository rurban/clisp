# Makefile for CLISP regression tests
# ifneq/else/endif requires GNU Make

OS=$(shell uname);
BD=../
ifneq (,$(filter CYGWIN%,$(OS)))
RUN=exe
else
RUN=run
endif

LISP=$(BD)/lisp.$(RUN) -norc -B $(BD) -M $(BD)/lispinit.mem
# LISP=../clisp -norc
# LISP=clisp -norc

RM = rm -f

all : clean tests compare

clean : force
	$(RM) *.erg

tests : force
	$(LISP) -C -i tests.lisp -x '(time (run-all-tests))'

compare : force
	(echo *.erg | grep '*' >/dev/null) || (echo "Test failed:" ; ls -l *erg; echo "To see which tests failed, type" ; echo "    cat "`pwd`"/*.erg" ; exit 1)
	echo "Test passed."

%.erg: %.tst $(BD)/lispinit.mem $(BD)/lisp.$(RUN)
	$(LISP) -i tests.lisp -x '(run-test "$<")'

force :

