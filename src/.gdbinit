handle SIGSEGV noprint nostop
handle SIGBUS noprint nostop
set args -B . -M lispinit.mem -q -norc
define zout
        print object_out($arg0)
end
document zout
        print the specified object with PRIN1
end
break sigsegv_handler_failed
