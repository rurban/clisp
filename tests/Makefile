OS=$(shell uname);
ifneq (,$(filter CYGWIN%,$(OS)))
BD=../src/
EXE=$(BD)lisp.exe
else
BD=../build/
EXE=$(BD)lisp.run
endif
LISP=$(EXE) -norc -q -B $(BD) -M $(BD)lispinit.mem
# LISP = clisp

RM = rm -f

all : clean tests compare

clean : force
	$(RM) *.erg

tests : force
	$(LISP) -i tests.lisp -x '(run-all-tests)'

compare : force
	(echo *.erg | grep '\*') >/dev/null || (echo "Test failed." ; echo "To see which tests failed, type" ; echo "    cat *.erg" ; exit 1)
	echo "Test passed."

force :

