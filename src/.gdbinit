# -*- sh -*-
# CLISP .gdbinit

define base
       file lisp.run
       set args -B . -M lispinit.mem -q -norc
end
document base
         debug the base linking set
end

define full
       file full/lisp.run
       set args -B . -M full/lispinit.mem -q -norc -i clx/new-clx/demos/clx-demos -p CLX-DEMOS
       break my_type_error
       break closed_display_error
end
document full
         debug the full linking set
end

define interpreted
       file lisp.run
       set args -B . -M interpreted.mem -q -norc
end
document interpreted
         debug the base linking set with the interpreted memory image
end

# usually we are debugging the base set
base

define zout
       output object_out($arg0)
       echo \n
end
document zout
         print the specified object with PRIN1
end

define xout
       output nobject_out(0,$arg0)
       echo \n
end
document xout
         print the specified object with nobject_out()
end

define run_test
       run -B . -M lispinit.mem -q -norc -C -i suite/tests.lisp -x "(run-test \"suite/$arg0.tst\")"
end
document run_test
         run the specified test in the test suite
end

define run_all_tests
       run -B . -M lispinit.mem -q -norc -C -i suite/tests.lisp -x "(cd \"suite/\") (run-all-tests)"
end
document run_all_tests
         run the specified test in the test suite
end

define ansi_tests
       run -B . -M lispinit.mem -q -norc -ansi -x "(cd \"../../../gcl/ansi-tests/\") (load \"gclload\")"
end
document ansi_tests
         run the gcl/ansi-test suite
end


define stack
       set $idx = $arg1
       while $idx >= $arg0
         echo ***** STACK_
         output $idx
         echo \ *****\n
         output object_out(STACK[-1-$idx])
         echo \n
         set $idx = $idx-1
       end
end
document stack
         print the section of STACK
end

define zbacktrace
       p back_trace_out(0,0)
end
document zbacktrace
         print the CLISP backtrace
end

break funcall
commands
        zout fun
end

break apply
commands
        zout fun
end

break eval
commands
        zout form
end

break gar_col
break fehler_notreached
break SP_ueber
break STACK_ueber

# disable breaks in funcall, apply, eval and gar_col
disable 1 2 3 4

watch back_trace
commands
        zbacktrace
        continue
end
disable 8

info break

# these should come last:
# without GENERATIONAL_GC there is no sigsegv_handler_failed(),
# so the next `break' command will fail,
# thus the two last `handle' commands will not be executed,
# thus we _will_ see the backtrace
# ergo: `break sigsegv_handler_failed' must come _before_
#       `handle SIG*'
#ifdef GENERATIONAL_GC
break sigsegv_handler_failed
handle SIGSEGV noprint nostop
handle SIGBUS noprint nostop
#endif
