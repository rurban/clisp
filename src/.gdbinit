# CLISP .gdbinit

file lisp.run
set args -B . -M lispinit.mem -q -norc

define zout
        output object_out($arg0)
        echo \n
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

define xbacktrace
  set $bt = back_trace
  while $bt
    output object_out($bt->caller)
    echo \ [num_ar=
    output $bt->num_arg
    echo ] [stack=
    output (int)$bt->stack
    echo ]
    if $bt->next
      echo \ [stack diff=
      output (($bt->stack)-($bt->next->stack))
      echo ]
    end
    echo \n
    set $bt = $bt->next
  end
end
document xbacktrace
         print the backtrace from back_trace
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
        xbacktrace
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
