;; Disassemble some machine code.
;; Bruno Haible 1995

;; This is in a separate file so you can easily customize it to your needs.

(in-package "COMPILER")

#+UNIX
(defun disassemble-machine-code (program-name pid address)
  ; This uses gdb.
  (unless (stringp address) (setq address (format nil "~A" address)))
  (let ((tempfilename (format nil "/tmp/gdbcomm~D" pid))
        (outfilename (format nil "/tmp/gdbdis~D" pid)))
    (with-open-file (f tempfilename :direction :output)
      (format f "set height 100000~%")   ; inhibit pausing after every 23 lines
                                         ; (remove this if your gdb doesn't understand it)
      (format f "set width 1000~%")      ; inhibit line breaking
                                         ; (because we filter the lines later)
      (format f "attach ~D~%" pid)       ; attach to the lisp.run process
      (if (digit-char-p (char address 0))
        (format f "disassemble ~A~%" address) ; disassemble at numerical address
        (format f "x/10000i ~A~%" address)    ; disassemble at symbolic address
      )                                       ; (the "disassemble" command doesn't
                                              ; always work for symbolic arguments)
      (format f "detach~%")              ; let lisp.run continue
      (format f "quit~%")                ; quit the debugger
    )
    ; Run gdb, capture only the lines beginning with 0x.
    ; Let lisp.run continue (just in case the debugger didn't detach properly).
    (shell (format nil "~A -n -batch -x ~A ~A < /dev/null | grep '^0' > ~A ; kill -CONT ~D"
                        "gdb" tempfilename program-name outfilename pid
    )      )
    (delete-file tempfilename)
    ; Now let the user view the listing.
    (shell (format nil "~A ~A" (or (sys::getenv "PAGER") "more") outfilename))
    (delete-file outfilename)
  )
  #| ; This uses SunOS dbx. (Untested.)
  (let ((tempfilename (format nil "/tmp/dbxcomm~D" pid)))
    (with-open-file (f tempfilename :direction :output)
      (format f "~A/100i~%" address) ; disassemble
      (format f "detach~%")          ; let lisp.run continue
      (format f "quit~%")            ; quit the debugger
    )
    (shell (format nil "~A -s ~A ~A ~D" "dbx" tempfilename program-name pid))
  )
  |#
  #| ; This uses AIX dbx. (Untested.)
  (let ((tempfilename (format nil "/tmp/dbxcomm~D" pid)))
    (with-open-file (f tempfilename :direction :output)
      (format f "~A/100i~%" address) ; disassemble
      (format f "detach~%")          ; let lisp.run continue
      (format f "quit~%")            ; quit the debugger
    )
    (shell (format nil "~A -c ~A -a ~D ~A" "dbx" tempfilename pid program-name))
  )
  |#
  (values)
)
