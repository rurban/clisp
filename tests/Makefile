#  the ifneq/else/endif requires GNU Make
#  and is only necessary for CYGWIN
#  which cannot accomodate a separate build directory
# OS=$(shell uname);
# ifneq (,$(filter CYGWIN%,$(OS)))
# BD=../src/
# EXE=$(BD)lisp.exe
# else
BD=../build/
EXE=$(BD)lisp.run
# endif

LISP=$(EXE) -norc -q -B $(BD) -M $(BD)lispinit.mem

# LISP = clisp -norc

RM = rm -f

all : clean tests compare

clean : force
	$(RM) *.erg

tests : force
	$(LISP) -C -i tests.lisp -x '(time (run-all-tests))'

compare : force
	(echo *.erg | grep '\*') >/dev/null || (echo "Test failed." ; echo "To see which tests failed, type" ; echo "    cat "`pwd`"/*.erg" ; exit 1)
	echo "Test passed."

force :

