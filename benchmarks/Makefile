# Makefile for Gabriel's Benchmarks

# CLISP
CLISP=../build/clisp -norc

# CMUCL
CMUCL=lisp -noinit

LOG=benchmarks.log
EVAL=no

SOURCES=acker bfib boyer browse ctak dderiv deriv destru div2 fac fft fprint \
    fread frpoly puzzle stak stream tak takl takr tprint traverse triang

all:
	@echo "chose your compiler and run 'make clisp' or 'make cmucl'"

%.fas: %.lisp
	$(CLISP) -c $<

%.x86f: %.lisp
	$(CMUCL) -eval '(progn (compile-file "$<") (quit 0))'

clisp: run-all.fas $(addsuffix .lisp ,$(SOURCES))
	$(CLISP) -i $< -x '(benchmarks :compiled "$(LOG)")'
	@if [ "$(EVAL)" = "yes" ]; then $(CLISP) -i $< -x '(benchmarks :interpreted "$(LOG)")'; fi

cmucl: run-all.x86f $(addsuffix .lisp ,$(SOURCES))
	$(CMUCL) -load $< -eval '(progn (benchmarks :compiled "$(LOG)") (quit 0))'
	@if [ "$(EVAL)" = "yes" ]; then $(CMUCL) -load $< -eval '(progn (benchmarks :interpreted "$(LOG)") (quit 0))'; fi

clean: force
	rm -f *.fas *.lib *.tst *.x86f

distclean: force
	rm -f *.log

force:
