# CLISP .gdbinit

file lisp.run
set args -B . -M lispinit.mem -q -norc

define zout
        output object_out($arg0)
end
document zout
        print the specified object with PRIN1
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

break funcall
commands
        zout fun
end

break eval
commands
        zout form
end

break gar_col
break fehler_notreached

disable 1 2 3

maint info break

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
