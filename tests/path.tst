;; -*- lisp -*-

(setf string "test-pathname.abc" symbol 'test-pathname.abc)
test-pathname.abc

;; PATHNAME: argument type: pathname,string,symbol,stream
;;           result: pathname

(SETF PATHSTRING (PATHNAME STRING))
#+XCL
#S(PATHNAME SYSTEM::HOST NIL
SYSTEM::DEVICE "DISK00$ABT43" DIRECTORY "XCL.MAIN" SYSTEM::NAME "TEST-PATHNAME"
TYPE "ABC" SYSTEM::VERSION NIL)
#+CLISP
#S(PATHNAME :HOST NIL :DEVICE NIL :DIRECTORY (:RELATIVE)
            :NAME "test-pathname" :TYPE "abc" :VERSION NIL)

(SETF PATHSYMBOL (PATHNAME symbol))
#+XCL
#S(PATHNAME SYSTEM::HOST
NIL SYSTEM::DEVICE "DISK00$ABT43" DIRECTORY "XCL.MAIN" SYSTEM::NAME
"TEST-PATHNAME" TYPE "ABC" SYSTEM::VERSION NIL)
#+CLISP
#S(PATHNAME :HOST NIL :DEVICE NIL :DIRECTORY (:RELATIVE)
            :NAME "test-pathname" :TYPE "abc" :VERSION NIL)

(SETF PATHPATH (PATHNAME PATHSYMBOL))
#+XCL
#S(PATHNAME SYSTEM::HOST NIL
SYSTEM::DEVICE "DISK00$ABT43" DIRECTORY "XCL.MAIN" SYSTEM::NAME "TEST-PATHNAME"
TYPE "ABC" SYSTEM::VERSION NIL)
#+CLISP
#S(PATHNAME :HOST NIL :DEVICE NIL :DIRECTORY (:RELATIVE)
            :NAME "test-pathname" :TYPE "abc" :VERSION NIL)

(SETF STREAM (OPEN STRING :DIRECTION :OUTPUT #+SBCL :IF-EXISTS #+SBCL :SUPERSEDE)
      a nil)
nil

;(SETF PATHSTREAM (PATHNAME STREAM))
;"test-pathname.lsp"

(MAPCAR (FUNCTION PATHNAMEP)
        (LIST PATHSTRING PATHSYMBOL PATHPATH ;PATHSTREAM
))
(T T T ;T
)


;; function truename returns filename for pathname or stream
;
;(MAPCAR (FUNCTION TRUENAME) (LIST PATHSTRING PATHSYMBOL PATHPATH STREAM
;                                                               ;PATHSTREAM
;                                                                 ))
;  ERROR



(PARSE-NAMESTRING STRING)
#+XCL
#S(PATHNAME SYSTEM::HOST NIL
SYSTEM::DEVICE "DISK00$ABT43" DIRECTORY "XCL.MAIN" SYSTEM::NAME "TEST-PATHNAME"
TYPE "ABC" SYSTEM::VERSION NIL)
#+CLISP
#S(PATHNAME :HOST NIL :DEVICE NIL :DIRECTORY (:RELATIVE)
            :NAME "test-pathname" :TYPE "abc" :VERSION NIL)

(PARSE-NAMESTRING SYMBOL)
#+XCL
#S(PATHNAME SYSTEM::HOST NIL
SYSTEM::DEVICE "DISK00$ABT43" DIRECTORY "XCL.MAIN" SYSTEM::NAME "TEST-PATHNAME"
TYPE "ABC" SYSTEM::VERSION NIL)
#+CLISP
#S(PATHNAME :HOST NIL :DEVICE NIL :DIRECTORY (:RELATIVE)
            :NAME "test-pathname" :TYPE "abc" :VERSION NIL)

#+XCL
(PARSE-NAMESTRING "bab:test-pathname.abc")
#+XCL
#S(PATHNAME SYSTEM::HOST NIL
SYSTEM::DEVICE "$1$DUA70" DIRECTORY "43.BABYLON.REL2" SYSTEM::NAME
"TEST-PATHNAME" TYPE "ABC" SYSTEM::VERSION NIL)

#+XCL
(PARSE-NAMESTRING "bab:test-pathname.abc;3")
#+XCL
#S(PATHNAME SYSTEM::HOST NIL
SYSTEM::DEVICE "$1$DUA70" DIRECTORY "43.BABYLON.REL2" SYSTEM::NAME
"TEST-PATHNAME" TYPE "ABC" SYSTEM::VERSION 3)

(PARSE-NAMESTRING PATHSTRING)
#+XCL
#S(PATHNAME SYSTEM::HOST NIL SYSTEM::DEVICE
"DISK00$ABT43" DIRECTORY "XCL.MAIN" SYSTEM::NAME "TEST-PATHNAME" TYPE "ABC"
SYSTEM::VERSION NIL)
#+CLISP
#S(PATHNAME :HOST NIL :DEVICE NIL :DIRECTORY (:RELATIVE)
            :NAME "test-pathname" :TYPE "abc" :VERSION NIL)

(PARSE-NAMESTRING "test-pathname.abc" NIL)
#+XCL
#S(PATHNAME SYSTEM::HOST NIL
SYSTEM::DEVICE "DISK00$ABT43" DIRECTORY "XCL.MAIN" SYSTEM::NAME "TEST-PATHNAME"
TYPE "ABC" SYSTEM::VERSION NIL)
#+CLISP
#S(PATHNAME :HOST NIL :DEVICE NIL :DIRECTORY (:RELATIVE)
            :NAME "test-pathname" :TYPE "abc" :VERSION NIL)

#+XCL
(PARSE-NAMESTRING "sirius::disk00$abt43:[heicking]test-pathname.abc")
#+XCL
#S(PATHNAME
SYSTEM::HOST "SIRIUS" SYSTEM::DEVICE "DISK00$ABT43" DIRECTORY "HEICKING"
SYSTEM::NAME "TEST-PATHNAME" TYPE "ABC" SYSTEM::VERSION NIL)

#+XCL
(PARSE-NAMESTRING "sirius::disk00$abt43:[heicking]test-pathname.abc" "sirius")
#+XCL
#S(PATHNAME
SYSTEM::HOST "SIRIUS" SYSTEM::DEVICE "DISK00$ABT43" DIRECTORY "HEICKING"
SYSTEM::NAME "TEST-PATHNAME" TYPE "ABC" SYSTEM::VERSION NIL)

#+XCL
(PARSE-NAMESTRING "sirius::disk00$abt43:[heicking]test-pathname.abc" "orion")
#+XCL
ERROR

(PARSE-NAMESTRING "abc.123" NIL NIL :START 0 :END 5)
#+XCL
#S(PATHNAME SYSTEM::HOST
NIL SYSTEM::DEVICE "DISK00$ABT43" DIRECTORY "XCL.MAIN" SYSTEM::NAME "ABC" TYPE
"1" SYSTEM::VERSION NIL)
#+CLISP
#S(PATHNAME :HOST NIL :DEVICE NIL :DIRECTORY (:RELATIVE)
            :NAME "abc" :TYPE "1" :VERSION NIL)

(PARSE-NAMESTRING "abc.123" NIL NIL :START 2 :END 5)
#+XCL
#S(PATHNAME SYSTEM::HOST
NIL SYSTEM::DEVICE "DISK00$ABT43" DIRECTORY "XCL.MAIN" SYSTEM::NAME "C" TYPE "1"
SYSTEM::VERSION NIL)
#+CLISP
#S(PATHNAME :HOST NIL :DEVICE NIL :DIRECTORY (:RELATIVE)
            :NAME "c" :TYPE "1" :VERSION NIL)

#+XCL
(PARSE-NAMESTRING "babylon" NIL NIL :START 0 :END 3)
#+XCL
#S(PATHNAME SYSTEM::HOST
NIL SYSTEM::DEVICE "$1$DUA70" DIRECTORY "43.BABYLON.REL2" SYSTEM::NAME NIL TYPE
NIL SYSTEM::VERSION NIL)

#+XCL
(PARSE-NAMESTRING "babylon" NIL NIL :START 0 :END 7)
#+XCL
#S(PATHNAME SYSTEM::HOST
NIL SYSTEM::DEVICE "DISK00$ABT43" DIRECTORY "XCL.MAIN" SYSTEM::NAME "BABYLON"
TYPE NIL SYSTEM::VERSION NIL)

#+XCL
(PARSE-NAMESTRING "babylon" NIL *DEFAULT-PATHNAME-DEFAULTS* :START 0 :END 7)
#+XCL
#S(PATHNAME
SYSTEM::HOST NIL SYSTEM::DEVICE "DISK00$ABT43" DIRECTORY "XCL.MAIN" SYSTEM::NAME
"BABYLON" TYPE NIL SYSTEM::VERSION NIL)

(make-pathname :device nil :defaults *DEFAULT-PATHNAME-DEFAULTS*)
#+XCL
#S(PATHNAME SYSTEM::HOST NIL SYSTEM::DEVICE NIL
DIRECTORY NIL SYSTEM::NAME NIL TYPE "lsp" SYSTEM::VERSION :NEWEST)
#+CLISP
#S(PATHNAME :HOST NIL :DEVICE NIL :DIRECTORY (:RELATIVE)
            :NAME NIL :TYPE NIL :VERSION NIL)

#+XCL
(PARSE-NAMESTRING "babylon" NIL *DEFAULT-PATHNAME-DEFAULTS* :START 0 :END 3)
#+XCL
#S(PATHNAME
SYSTEM::HOST NIL SYSTEM::DEVICE "$1$DUA70" DIRECTORY "43.BABYLON.REL2"
SYSTEM::NAME NIL TYPE NIL SYSTEM::VERSION NIL)

;(PARSE-NAMESTRING "babylon.c.c" NIL NIL :JUNK-ALLOWED T)
;#S(PATHNAME
;SYSTEM::HOST NIL SYSTEM::DEVICE "DISK00$ABT43" DIRECTORY "XCL.MAIN" SYSTEM::NAME
;"BABYLON" TYPE "C" SYSTEM::VERSION NIL)

;(PARSE-NAMESTRING "babylon;c.c" NIL NIL :JUNK-ALLOWED T)
;#S(PATHNAME
;SYSTEM::HOST NIL SYSTEM::DEVICE "DISK00$ABT43" DIRECTORY "XCL.MAIN" SYSTEM::NAME
;"BABYLON" TYPE NIL SYSTEM::VERSION NIL)

#+XCL
(PARSE-NAMESTRING "babylon;c.c" NIL NIL :JUNK-ALLOWED NIL)
#+XCL
ERROR

#+XCL
(PARSE-NAMESTRING "babylon.c.c" NIL NIL :JUNK-ALLOWED NIL)
#+XCL
ERROR

#+XCL
(PARSE-NAMESTRING "babylon.c;c" NIL NIL :JUNK-ALLOWED NIL)
#+XCL
ERROR

#+XCL
(PARSE-NAMESTRING "babylon.c;" NIL NIL :JUNK-ALLOWED NIL)
#+XCL
#S(PATHNAME
SYSTEM::HOST NIL SYSTEM::DEVICE "DISK00$ABT43" DIRECTORY "XCL.MAIN" SYSTEM::NAME
"BABYLON" TYPE "C" SYSTEM::VERSION NIL)

#+XCL
(PARSE-NAMESTRING "babylon.c;5" NIL NIL :JUNK-ALLOWED NIL)
#+XCL
#S(PATHNAME
SYSTEM::HOST NIL SYSTEM::DEVICE "DISK00$ABT43" DIRECTORY "XCL.MAIN" SYSTEM::NAME
"BABYLON" TYPE "C" SYSTEM::VERSION 5)

;(MERGE-PATHNAME "test$$" SYMBOL 10)   ERROR
;;
;(MERGE-PATHNAME "test$$" SYMBOL)   ERROR
;
;(MERGE-PATHNAME "test$$" PATH)   ERROR
;
;(MERGE-PATHNAME "test$$")   ERROR

#+XCL
(MERGE-PATHNAMES "test$$")
#+XCL
#S(PATHNAME SYSTEM::HOST NIL SYSTEM::DEVICE
"DISK00$ABT43" DIRECTORY "XCL.MAIN" SYSTEM::NAME "TEST$$" TYPE "lsp"
SYSTEM::VERSION :NEWEST)

#+XCL
(MERGE-PATHNAMES "test$$" SYMBOL)
#+XCL
#S(PATHNAME SYSTEM::HOST NIL SYSTEM::DEVICE
"DISK00$ABT43" DIRECTORY "XCL.MAIN" SYSTEM::NAME "TEST$$" TYPE "ABC"
SYSTEM::VERSION :NEWEST)

#+XCL
(MERGE-PATHNAMES "test$$" SYMBOL 2)
#+XCL
#S(PATHNAME SYSTEM::HOST NIL
SYSTEM::DEVICE "DISK00$ABT43" DIRECTORY "XCL.MAIN" SYSTEM::NAME "TEST$$" TYPE
"ABC" SYSTEM::VERSION 2)

#+XCL
(MERGE-PATHNAMES "test$$" (PATHNAME SYMBOL) 2)
#+XCL
#S(PATHNAME SYSTEM::HOST NIL
SYSTEM::DEVICE "DISK00$ABT43" DIRECTORY "XCL.MAIN" SYSTEM::NAME "TEST$$" TYPE
"ABC" SYSTEM::VERSION 2)

#+XCL
(MERGE-PATHNAMES "test$$" STREAM 2)
#+XCL
#S(PATHNAME SYSTEM::HOST 16 SYSTEM::DEVICE
"DISK00$ABT43" DIRECTORY "XCL.MAIN" SYSTEM::NAME "TEST$$" TYPE :ESCAPE
SYSTEM::VERSION 2)


;(MERGE-PATHNAME STRING SYMBOL)   ERROR

#+XCL
(MAKE-PATHNAME :NAME "a" :HOST (QUOTE ORION))
#+XCL
#S(PATHNAME SYSTEM::HOST ORION
SYSTEM::DEVICE NIL DIRECTORY NIL SYSTEM::NAME "a" TYPE NIL SYSTEM::VERSION
:NEWEST)

#+XCL
(DEFMACRO TEST (&REST BODY) (\` (APPLY (FUNCTION MAKE-PATHNAME) (\,@ BODY))))
#+XCL
TEST

#+XCL
(setf a '(:host "sirius" :name "a"))
#+XCL
(:host "sirius" :name "a")

#+XCL
(TEST A)
#+XCL
#S(PATHNAME SYSTEM::HOST "sirius" SYSTEM::DEVICE NIL DIRECTORY NIL
SYSTEM::NAME "a" TYPE NIL SYSTEM::VERSION :NEWEST)

#+XCL
(SETF A (LIST* :DEVICE "disk00$abt43" A))
#+XCL
(:DEVICE "disk00$abt43" :HOST "sirius" :NAME "a")

#+XCL
(TEST A)
#+XCL
#S(PATHNAME SYSTEM::HOST "sirius" SYSTEM::DEVICE "disk00$abt43"
DIRECTORY NIL SYSTEM::NAME "a" TYPE NIL SYSTEM::VERSION :NEWEST)

#+XCL
(SETF A (LIST* :DIRECTORY "[heicking.comlisp]" A))
#+XCL
(:DIRECTORY
"[heicking.comlisp]" :DEVICE "disk00$abt43" :HOST "sirius" :NAME "a")

#+XCL
(TEST A)
#+XCL
#S(PATHNAME SYSTEM::HOST "sirius" SYSTEM::DEVICE "disk00$abt43"
DIRECTORY "[heicking.comlisp]" SYSTEM::NAME "a" TYPE NIL SYSTEM::VERSION
:NEWEST)

#+XCL
(SETF A (LIST* :TYPE "raf" A))
#+XCL
(:TYPE "raf" :DIRECTORY "[heicking.comlisp]"
:DEVICE "disk00$abt43" :HOST "sirius" :NAME "a")

#+XCL
(TEST A)
#+XCL
#S(PATHNAME SYSTEM::HOST "sirius" SYSTEM::DEVICE "disk00$abt43"
DIRECTORY "[heicking.comlisp]" SYSTEM::NAME "a" TYPE "raf" SYSTEM::VERSION
:NEWEST)

#+XCL
(SETF A (LIST* :VERSION 3 A))
#+XCL
(:VERSION 3 :TYPE "raf" :DIRECTORY
"[heicking.comlisp]" :DEVICE "disk00$abt43" :HOST "sirius" :NAME "a")

#+XCL
(TEST A)
#+XCL
#S(PATHNAME SYSTEM::HOST "sirius" SYSTEM::DEVICE "disk00$abt43"
DIRECTORY "[heicking.comlisp]" SYSTEM::NAME "a" TYPE "raf" SYSTEM::VERSION 3)

(MAPCAR (FUNCTION PATHNAMEP) (LIST PATHSYMBOL PATHPATH PATHSTRING))
(T T T)

#+XCL
(SETF PATH (TEST A))
#+XCL
#S(PATHNAME SYSTEM::HOST "sirius" SYSTEM::DEVICE
"disk00$abt43" DIRECTORY "[heicking.comlisp]" SYSTEM::NAME "a" TYPE "raf"
SYSTEM::VERSION 3)

#+XCL
(MAPCAR (FUNCTION PATHNAME-HOST) (LIST SYMBOL STRING STREAM PATH))
#+XCL
(NIL NIL NIL NIL)

#+XCL
(MAPCAR (FUNCTION PATHNAME-DEVICE) (LIST SYMBOL STRING STREAM PATH))
#+XCL
("DISK00$ABT43" "DISK00$ABT43" "DISK00$ABT43" "DISK00$ABT43")

#+XCL
(MAPCAR (FUNCTION PATHNAME-DIRECTORY) (LIST SYMBOL STRING STREAM PATH))
#+XCL
("XCL.MAIN" "XCL.MAIN" "XCL.MAIN" "XCL.MAIN")

(PROGN (CLOSE STREAM) T)
T

#+XCL
(USER-HOMEDIR-PATHNAME)
#+XCL
#S(PATHNAME SYSTEM::HOST NIL SYSTEM::DEVICE
"DISK00$ABT43" DIRECTORY "HEICKING" SYSTEM::NAME NIL TYPE NIL SYSTEM::VERSION
NIL)

(PATHNAME "*.*")
#+XCL
#S(PATHNAME SYSTEM::HOST NIL SYSTEM::DEVICE "DISK00$ABT43"
DIRECTORY "HEICKING" SYSTEM::NAME "*" TYPE :WILD SYSTEM::VERSION NIL)
#+CLISP
#S(PATHNAME :HOST NIL :DEVICE NIL :DIRECTORY (:RELATIVE)
            :NAME :WILD :TYPE :WILD :VERSION NIL)

(progn (setf file (open "nicht-vorhandenes-file.non"
                        :direction :input
                        :element-type 'string-char
                        :if-does-not-exist :create)) t)
t

(null (probe-file "nicht-vorhandenes-file.non"))
NIL

(progn (close file) t)
t

(setf file (open "nicht-vorhandenes-file.non"
                        :direction :io
                        :element-type 'string-char
                        :if-exists :error))
error

(progn (close file) t)
t

(null (setf file (open "nicht-vorhandenes-file.non"
                        :direction :io
                        :element-type 'string-char
                        :if-exists :new-version)))
nil

(progn (close file) t)
t

(null (setf file (open "nicht-vorhandenes-file.non"
                        :direction :io
                        :element-type 'string-char
                        :if-exists :rename)))
nil

(progn (close file) t)
t

(null (setf file (open "nicht-vorhandenes-file.non"
                        :direction :io
                        :element-type 'string-char
                        :if-exists :rename-and-delete)))
nil

(progn (close file) t)
t

(null (setf file (open "nicht-vorhandenes-file.non"
                        :direction :io
                        :element-type 'string-char
                        :if-exists :overwrite)))
nil

(progn (close file) t)
t

(null (setf file (open "nicht-vorhandenes-file.non"
                        :direction :io
                        :element-type 'string-char
                        :if-exists :append)))
nil

(progn (close file) t)
t

(null (setf file (open "nicht-vorhandenes-file.non"
                        :direction :io
                        :element-type 'string-char
                        :if-exists :supersede)))
nil

(progn (close file) t)
t

(setf file (open "nicht-vorhandenes-file.non"
                        :direction :io
                        :element-type 'string-char
                        :if-exists nil))
nil

(progn (close file) t)
error

(setf file (open "nicht-vorhandenes-file.new"
                        :direction :io
                        :element-type 'string-char
                        :if-does-not-exist :error))
error

(progn (close file) t)
error

(truename "~/no/ such / path /  nicht-vorhandenes-file.new")
error

(null (setf file (open "nicht-vorhandenes-file.new"
                        :direction :io
                        :element-type 'string-char
                        :if-does-not-exist :create)))
nil

(progn (close file) t)
t

(null (setf file (open "nicht-vorhandenes-file.non"
                        :direction :io
                        :element-type 'string-char
                        :if-does-not-exist nil)))
nil

(progn (close file) t)
t

(let ((*default-pathname-defaults* #p""))
  (namestring
   (multiple-value-setq (new-name pathname truename)
     (rename-file "nicht-vorhandenes-file.non" "file.da"))))
"file.da"

(namestring new-name)
"file.da"

(null pathname)
nil

(null truename)
nil

(progn (delete-file "test-pathname.abc") t)
t

(progn (mapc #'delete-file (directory "nicht-vorhandenes-file.*")) t)
t

(progn (delete-file "file.da") t)
t

(progn
 (setf (logical-pathname-translations "clocc")
       '(("**;*" "/usr/local/src/clocc/**/*"))
       (logical-pathname-translations "CL-LIBRARY")
       '((";**;*.*.*" "/tmp/clisp/"))
       (logical-pathname-translations "cl-systems")
       '((";**;*.*.*"  "/usr/share/common-lisp/systems/**/*.*")
         ("**;*.*.*"  "/usr/share/common-lisp/systems/**/*.*")
         (";*.*.*"  "/usr/share/common-lisp/systems/*.*")
         ("*.*.*"  "/usr/share/common-lisp/systems/*.*"))
       (logical-pathname-translations "TEST-SIMPLE")
       '(("*.*.*" "/usr/local/tmp/*.*.*")
         ("*.*" "/usr/local/tmp/*.*"))
       (logical-pathname-translations "TEST-SUBDIR")
       '(("**;*.*" "/usr/local/share/**/*.*")
         ("**;*.*.*" "/usr/local/share/**/*.*.*")
         (";**;*.*" "/usr/local/share/r/**/*.*")
         (";**;*.*.*" "/usr/local/share/r/**/*.*.*")))
 nil)
nil

(translate-logical-pathname "clocc:src;port;")
#P"/usr/local/src/clocc/src/port/"

(translate-pathname "foobar" "foo*" "*baz")
#P"barbaz"

(translate-pathname "foobarbazquux" "foo*baz*" "*baq*zot")
#P"barbaqquuxzot"

(translate-pathname "foobarbazquuxfff" "foo*baz*f?" "*baq*zot*")
#P"barbaqquuxfzotf"

(translate-pathname "uufoobarbazquuxfff" "u?foo*baz*f?" "**baq*zot*")
#P"ubarbaqquuxfzotf"

(translate-pathname "test.txt" "*.txt" "*.text")
#P"test.text"

(translate-pathname "foo/bar" "*/bar" "*/baz")
#P"foo/baz"

(translate-pathname "bar/foo" "bar/*" "baz/*")
#P"baz/foo"

(make-pathname :defaults "**/*.FASL" :host "CL-LIBRARY")
#+CLISP
#S(LOGICAL-PATHNAME :HOST "CL-LIBRARY" :DEVICE NIL
   :DIRECTORY (:RELATIVE :WILD-INFERIORS)
   :NAME :WILD :TYPE "FASL" :VERSION NIL)
#-CLISP
FIXME

(make-pathname :defaults "/**/*.FASL" :host "CL-LIBRARY")
#+CLISP
#S(LOGICAL-PATHNAME :HOST "CL-LIBRARY" :DEVICE NIL
   :DIRECTORY (:ABSOLUTE :WILD-INFERIORS)
   :NAME :WILD :TYPE "FASL" :VERSION NIL)
#-CLISP
FIXME

(logical-pathname ":")
#+CLISP
#S(LOGICAL-PATHNAME :HOST "" :DEVICE NIL :DIRECTORY (:ABSOLUTE)
                    :NAME NIL :TYPE NIL :VERSION NIL)
#-CLISP
FIXME

(merge-pathnames (logical-pathname "cl-systems:") "metering.system")
#+CLISP
#S(LOGICAL-PATHNAME :HOST "CL-SYSTEMS" :DEVICE NIL :DIRECTORY (:ABSOLUTE)
                    :NAME "METERING" :TYPE "SYSTEM" :VERSION :NEWEST)
#-CLISP
FIXME

(merge-pathnames (logical-pathname "cl-systems:") #P"metering.system")
#+CLISP
#S(LOGICAL-PATHNAME :HOST "CL-SYSTEMS" :DEVICE NIL :DIRECTORY (:ABSOLUTE)
                    :NAME "METERING" :TYPE "SYSTEM" :VERSION :NEWEST)
#-CLISP
FIXME

(merge-pathnames (logical-pathname "clocc:clocc.lisp"))
#+CLISP
#S(logical-pathname :host "CLOCC" :device nil :directory (:absolute)
                    :name "CLOCC" :type "LISP" :version :newest)
#-CLISP
FIXME

(merge-pathnames ".fas" (logical-pathname "clocc:src;cllib;xml.lisp"))
#+CLISP
#S(LOGICAL-PATHNAME :HOST "CLOCC" :DEVICE NIL :DIRECTORY
   (:ABSOLUTE "SRC" "CLLIB") :NAME "XML" :TYPE "FAS" :VERSION :NEWEST)
#-CLISP
FIXME

(merge-pathnames (logical-pathname "clocc:;foo;bar;")
                 (logical-pathname "clocc:baz;quux.lisp.3"))
#+CLISP
#S(LOGICAL-PATHNAME :HOST "CLOCC" :DEVICE NIL :DIRECTORY
   (:ABSOLUTE "BAZ" "FOO" "BAR") :NAME "QUUX" :TYPE "LISP" :VERSION 3)
#-CLISP
FIXME

(compile-file-pathname (logical-pathname "clocc:clocc.lisp"))
#+CLISP
#S(logical-pathname :host "CLOCC" :device nil :directory (:absolute)
                    :name "CLOCC" :type "FAS" :version :newest)
#-CLISP
FIXME

(compile-file-pathname (logical-pathname "clocc:src;cllib;xml.lisp"))
#+CLISP
#S(LOGICAL-PATHNAME :HOST "CLOCC" :DEVICE NIL :DIRECTORY
   (:ABSOLUTE "SRC" "CLLIB") :NAME "XML" :TYPE "FAS" :VERSION :NEWEST)
#-CLISP
FIXME

(parse-namestring "foo;bar;baz.fas.3" "clocc")
#+CLISP
#S(LOGICAL-PATHNAME :HOST "CLOCC" :DEVICE NIL
   :DIRECTORY (:ABSOLUTE "FOO" "BAR") :NAME "BAZ" :TYPE "FAS" :VERSION 3)
#-CLISP
FIXME

(parse-namestring "foo;bar;baz.fas.3" nil (logical-pathname "clocc:"))
#+CLISP
#S(LOGICAL-PATHNAME :HOST "CLOCC" :DEVICE NIL
   :DIRECTORY (:ABSOLUTE "FOO" "BAR") :NAME "BAZ" :TYPE "FAS" :VERSION 3)
#-CLISP
FIXME

(let* ((s "abcdefghijk")
       (d (make-array 5 :displaced-to s :displaced-index-offset 3
                      :element-type 'character)))
  (parse-namestring d nil nil :start 2 :end 4))
#+CLISP #S(PATHNAME :HOST NIL :DEVICE NIL :DIRECTORY (:RELATIVE)
                    :NAME "fg" :TYPE NIL :VERSION NIL)
#-CLISP #P"fg"                  ; same as above

;; Relative
(translate-logical-pathname
 (merge-pathnames (logical-pathname "TEST-SUBDIR:;FOO;BAR;")
                  (logical-pathname "TEST-SIMPLE:ZOT.LISP")))
#+CLISP #S(PATHNAME :HOST NIL :DEVICE NIL :DIRECTORY
                    (:ABSOLUTE "usr" "local" "share" "r" "foo" "bar")
                    :NAME "zot" :TYPE "lisp" :VERSION :NEWEST)
#-CLISP #p"/usr/local/share/r/foo/bar/zot.lisp"

;; Absolute
(translate-logical-pathname
 (merge-pathnames (logical-pathname "TEST-SUBDIR:FOO;BAR;")
                  (logical-pathname "TEST-SIMPLE:ZOT.LISP")))
#+CLISP #S(PATHNAME :HOST NIL :DEVICE NIL :DIRECTORY
                    (:ABSOLUTE "usr" "local" "share" "foo" "bar")
                    :NAME "zot" :TYPE "lisp" :VERSION :NEWEST)
#-CLISP #p"/usr/local/share/foo/bar/zot.lisp"

(make-pathname :defaults "a.b" :name "c" :type nil)
#+CLISP #S(PATHNAME :HOST NIL :DEVICE NIL :DIRECTORY (:RELATIVE)
                    :NAME "c" :TYPE NIL :VERSION :NEWEST)
#-CLISP #p"c"

#+CLISP
(make-pathname :defaults #S(LOGICAL-PATHNAME :HOST "CL-LIBRARY" :DEVICE NIL
                            :DIRECTORY (:ABSOLUTE "FOO")
                            :NAME "BAR" :TYPE "BAZ" :VERSION 3))
#+CLISP
#S(LOGICAL-PATHNAME :HOST "CL-LIBRARY" :DEVICE NIL :DIRECTORY (:ABSOLUTE "FOO")
   :NAME "BAR" :TYPE "BAZ" :VERSION 3)

(defun foo (x host)
  (let ((dflt (make-pathname :directory '(:relative :wild-inferiors)
                             :type x :case :common)))
    (if host
        (make-pathname :defaults dflt :host host :case :common)
        (make-pathname :defaults dflt :case :common))))
foo

;; :defaults arg is not subject to :case conversion
(string= "c" (pathname-type (foo "c" nil) :case :common))
t
(string= "C" (pathname-type (foo "C" nil) :case :common))
t

;; :case is ignored for logical pathnames
(string= "C" (pathname-type (foo "c" "CLOCC") :case :common))
t
(string= "c" (pathname-type (foo "C" "CLOCC") :case :common))
t

(namestring (logical-pathname "foo:bar;baz"))
"FOO:BAR;BAZ"

(let* ((foo (copy-seq "abcdefghijkl"))
       (bar (make-array 5 :displaced-to foo :displaced-index-offset 2
                        :element-type 'character))
       (path (make-pathname :directory bar)))
  (setf (aref foo 3) #\/)
  (equalp path (make-pathname :directory (pathname-directory path))))
t

(string= (namestring (make-pathname :name "FOO" :case :common
                                    :defaults #P"/home/kent/"))
         (namestring #P"/home/kent/foo"))
t

(make-pathname :directory '(:absolute :wild) :host nil :device nil
               :name nil :type nil :version nil)
#P"/*/"

(pathname-match-p "foo" "foo.*")           T
(translate-pathname "foo" "foo.*" "bar")   #p"bar"
(translate-pathname "foo" "foo.*" "bar.*") #p"bar"
(progn
  (setf (logical-pathname-translations "FOO") '(("FOO:**;*" "/foo/**/*")))
  (translate-logical-pathname "foo:bar;baz;zot.txt"))
#P"/foo/bar/baz/zot.txt"
(progn
  (setf (logical-pathname-translations "FOO") '(("**;*" "/foo/**/*")))
  (translate-logical-pathname "foo:bar;baz;zot.txt"))
#P"/foo/bar/baz/zot.txt"

(pathname "/foo/bar/../baz///zot//.././zoo")
#P"/foo/baz/zoo"

(pathname-directory "../../../")
(:RELATIVE :UP :UP :UP)

(listp (directory (make-pathname :version :wild
                                 :defaults (logical-pathname "FOO:"))))
T

;; <http://www.lisp.org/HyperSpec/Body/sec_19-3-2-1.html>
(pathname-device (logical-pathname "FOO:"))
:UNSPECIFIC

(let* ((old "foo-bar.old")
       (new (make-pathname :type "new" :defaults old)))
  (with-open-file (s old :direction :output #+SBCL :if-exists #+SBCL :supersede) (write-line "to be renamed" s))
  (unwind-protect
       (list (list (not (not (probe-file old))) (probe-file new))
             (length (multiple-value-list (rename-file old new)))
             (list (probe-file old) (not (not (probe-file new)))))
    (delete-file new)))
((T NIL) 3 (NIL T))

(wild-pathname-p (make-pathname :version :wild))   T

(pathname-version (merge-pathnames (make-pathname)
                                   (make-pathname :version :newest)
                                   nil))
:NEWEST

(pathname-version (merge-pathnames (make-pathname)
                                   (make-pathname :version nil)
                                   :newest))
:NEWEST

;; directory may not return wild pathnames
(remove-if-not #'wild-pathname-p
               (directory (make-pathname :name :wild :type :wild
                                         :version :wild)))
NIL

(let ((file "this-is-a-temp-file-to-be-removed-immediately"))
  (unwind-protect
       (let ((d (directory (make-pathname
                            :defaults (open file :direction :probe
                                        :if-does-not-exist :create)
                            :version :wild))))
         (list (= (length d) 1)
               (notany #'wild-pathname-p d)
               (equalp (car d) (truename file))))
    (delete-file file)))
(T T T)

(first (pathname-directory (translate-pathname
                            "foo/bar/baz" #p"" #p"" :absolute t)))
:ABSOLUTE

(let ((file "this-is-a-temp-file-to-be-removed-immediately.lisp"))
  (unwind-protect
       (let* ((p (pathname (open file :direction :probe
                                 :if-does-not-exist :create)))
              (p1 (make-pathname :type nil :defaults p)))
         (when (probe-file p1) (delete-file p1)) ; just in case
         (list (not (null (probe-file p))) ; just created
               (null (probe-file p1))      ; just deleted
               (let ((*default-pathname-defaults* ; 19.2.3 !!!
                      (make-pathname :type "lisp")))
                 (not (null (probe-file p1))))))
    (delete-file file)))
(T T T)

(let ((file "this-is-a-temp-file-to-be-removed-immediately"))
  (unwind-protect
       (let* ((p (pathname (open file :direction :probe
                                 :if-does-not-exist :create))))
         (list (not (null (probe-file p))) ; just created
               (with-open-file (s p)
                 (let ((*default-pathname-defaults*
                        (make-pathname :type "lisp")))
                   ;; despite 19.2.3, S is not subject to
                   ;; *DEFAULT-PATHNAME-DEFAULTS*!
                   (not (null (probe-file s)))))))
    (delete-file file)))
(T T)

(let ((file "this-is-a-temp-file-to-be-removed-immediately"))
  (unwind-protect
       (with-open-file (s file :direction :output)
         (list (not (null (probe-file file)))
               (not (null (probe-file s)))
               (equalp (truename s) (truename file))))
    (delete-file file)))
(T T T)

(multiple-value-list
 (parse-namestring (make-array 0 :element-type 'character
                               :displaced-to "foo"
                               :displaced-index-offset 1)))
(#P"" 0)

#+(and clisp win32)
(absolute-pathname (make-pathname :device :wild))
#+(and clisp win32)
error

(let ((home (user-homedir-pathname)))
  (or (null home) (not (not (pathnamep home)))))
T

(let ((home (user-homedir-pathname nil)))
  (or (null home) (not (not (pathnamep home)))))
T

(let ((home (user-homedir-pathname :unspecific)))
  (or (null home) (not (not (pathnamep home)))))
T

