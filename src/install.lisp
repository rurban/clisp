;; load this file in the directory where your CLISP distribution is located
;; to set the Registry appropriately

(defvar *clisp-home* (namestring (default-directory)))
(defvar *clisp-base-cmd*
  (concatenate 'string *clisp-home* "lisp.exe -B " *clisp-home* " -M "))
(defvar *clisp-cmd*
  (concatenate 'string *clisp-base-cmd* *clisp-home* "lispinit.mem"))

(defvar *eflags*
  (make-array 4 :element-type '(unsigned-byte 8) :initial-contents '(0 0 0 0)))

(format t "~& * Installing CLISP to run from ~a~%" *clisp-home*)

(defun add-lisp-file (dkey)
  (format t "associating CLISP with Lisp files under ~s~%" (dir-key-path dkey))
  (with-dir-key-open (lf dkey "lispfile" :direction :output)
    (setf (dir-key-value lf "") "Lisp source file"
          (dir-key-value lf "EditFlags") *eflags*)
    (with-dir-key-open (ic lf "DefaultIcon" :direction :output)
      (setf (dir-key-value ic "") "%SystemRoot%\\system32\\SHELL32.dll,41"))
    (with-dir-key-open (cc lf "shell\\Compile_with_CLISP" :direction :output)
      (setf (dir-key-value cc "") "Compile with CLISP")
      (with-dir-key-open (cmd cc "command" :direction :output)
        (setf (dir-key-value cmd "")
              (concatenate 'string *clisp-cmd* " -c \"%1\""))))))

(defun add-fas-file (dkey)
  (format t "associating CLISP with FAS files under ~s~%" (dir-key-path dkey))
  (with-dir-key-open (ff dkey "fasfile" :direction :output)
    (setf (dir-key-value ff "") "CLISP compiled file"
          (dir-key-value ff "EditFlags") *eflags*)
    (with-dir-key-open (ic ff "DefaultIcon" :direction :output)
      (setf (dir-key-value ic "") "%SystemRoot%\\system32\\SHELL32.dll,21"))
    (with-dir-key-open (ex ff "Shell\\Execute_with_CLISP" :direction :output)
      (setf (dir-key-value ex "") "Execute with CLISP")
      (with-dir-key-open (cmd ex "command" :direction :output)
        (setf (dir-key-value cmd "")
              (concatenate 'string *clisp-cmd* " \"%1\""))))
    (with-dir-key-open (lo ff "Shell\\Load_into_CLISP" :direction :output)
      (setf (dir-key-value lo "") "Load into CLISP")
      (with-dir-key-open (cmd lo "command" :direction :output)
        (setf (dir-key-value cmd "")
              (concatenate 'string *clisp-cmd* " -i \"%1\""))))))

(defun add-mem-file (dkey)
  (format t "associating CLISP with MEM files under ~s~%" (dir-key-path dkey))
  (with-dir-key-open (mf dkey "memfile" :direction :output)
    (setf (dir-key-value mf "") "CLISP memory image"
          (dir-key-value mf "EditFlags") *eflags*)
    (with-dir-key-open (ic mf "DefaultIcon" :direction :output)
      (setf (dir-key-value ic "") "%SystemRoot%\\system32\\SHELL32.dll,21"))
    (with-dir-key-open (ex mf "Shell\\Run_with_CLISP" :direction :output)
      (setf (dir-key-value ex "") "Run with CLISP")
      (with-dir-key-open (cmd ex "command" :direction :output)
        (setf (dir-key-value cmd "")
              (concatenate 'string *clisp-base-cmd* " \"%1\""))))))


(with-dir-key-open (c1 :win32 "HKEY_CLASSES_ROOT")
  (add-lisp-file c1)
  (add-fas-file c1)
  (add-mem-file c1))
(with-dir-key-open (c2 :win32 "HKEY_LOCAL_MACHINE\\SOFTWARE\\Classes")
  (add-lisp-file c2)
  (add-fas-file c2)
  (add-mem-file c2))

