# CLISP .gdbinit
set language c

define boot
  file lisp.run
  set args -B . -M lispinit.mem -q -norc
end
document boot
         debug the boot linking set
end

define base
  file base/lisp.run
  set args -B . -M base/lispinit.mem -q -norc
end
document base
         debug the base linking set
end

define full
  file full/lisp.run
  set args -B . -M full/lispinit.mem -q -norc
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
         debug the boot linking set with the interpreted memory image
end

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
         run the whole test suite
end

define ansi_tests
  run -B . -M lispinit.mem -q -norc -ansi -x "(cd \"ansi-tests/\") (load \"clispload.lsp\") (in-package \"CL-TEST\") (time (regression-test:do-tests))"
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
        xout fun
end

break apply
commands
        xout fun
end

break eval
commands
        xout form
end

break interpret_bytecode_
commands
        xout closure
end

break gar_col

watch back_trace
commands
        zbacktrace
        continue
end

# disable all the above breaks
disable 1 2 3 4 5 6

break fehler_notreached
break SP_ueber
break STACK_ueber

break fehler
break prepare_error

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

# cut and paste when you stop in interpret_bytecode_()
watch byteptr
commands
        output byteptr-byteptr_in
        echo \n
end
