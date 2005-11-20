;;; Copyright (C) 2000-2004 by Sam Steingold
;;; This file is a part of CLISP (http://clisp.cons.org), and, as such,
;;; is distributed under the GNU GPL (http://www.gnu.org/copyleft/gpl.html)

(in-package "EXT")

(export '(clhs clhs-root read-from-file browse-url open-http with-http-input))

(in-package "SYSTEM")

(defvar *browsers*
  '((:netscape "netscape" "~a")
    (:netscape-remote "netscape" "-remote" "openURL(~a,new-window)")
    (:mozilla "mozilla" "~a")
    (:mozilla-remote "mozilla" "-remote" "openURL(~a,new-window)")
    (:konqueror "kfmclient" "openURL" "~a")
    (:lynx "lynx" "~a")
    #+unix (:lynx-xterm "xterm" "-e" "lynx" "~a")
    (:links "links" "~a")
    #+unix (:links-xterm "xterm" "-e" "links" "~a")
    (:w3m "w3m" "~a")
    #+unix (:w3m-xterm "xterm" "-e" "w3m" "~a")
    #+cygwin (:default "cygstart" "~a")
    #+macos (:default "open" "~a")
    (:mmm "mmm" "-external" "~a")
    (:mosaic "xmosaic" "~a")
    (:emacs-w3 "gnudoit" "-q" "(w3-fetch \"~a\")"))
  "Alist of browsers and commands that invoke them.
`~a' will be replaced with the URL to view.")
(defvar *browser* nil
  "The default browser - a key in `*browsers*' or a list of strings.")

(defun read-from-file (file &key (out *standard-output*) (package "KEYWORD")
                       repeat)
  "Read an object from a file.
The keyword argument KEYWORD specifies the package to read in.
The keyword argument OUT specifies the output for log messages.
The keyword argument REPEAT specifies how many objects to read:
 If NIL (default), read once and return the object read;
 if a number, read that many times and return a list of objects read,
 if T, read until end of file and return a list of objects read"
  (let ((beg-real (get-internal-real-time)))
    (prog1 (with-open-file (str file :direction :input)
             (when out
               (format out "~&;; Reading `~a' [~:d bytes]..."
                       file (file-length str))
               (force-output (if (eq out t) *standard-output* out)))
             (with-standard-io-syntax
               (let ((*package* (find-package package)))
                 (cond ((null repeat) (read str))
                       ((numberp repeat)
                        (loop :repeat repeat :collect (read str)))
                       (t (loop :for obj = (read str nil str)
                            :until (eq obj str) :collect obj))))))
      (when out
        (format out "done [~,2f sec]~%"
                (/ (- (get-internal-real-time) beg-real)
                   internal-time-units-per-second))))))

(defun browse-url (url &key (browser *browser*) (out *standard-output*))
  "Run the browser (a keyword in `*browsers*' or a list) on the URL."
  #+WIN32
  (when (eq browser :default) ; feed url to ShellExecute
    (when out
      (format out "~&;; starting the default system browser with url ~s..." url)
      (force-output (if (eq out t) *standard-output* out)))
    (ext::shell-execute "open" url nil nil) ;to start default browser
    (when out (format out "done~%"))
    (return-from browse-url))
  (let* ((command
          (etypecase browser
            (list browser)
            (symbol (or (cdr (assoc browser *browsers* :test #'eq))
                        (error "unknown browser: `~s' (must be a key in `~s')"
                               browser '*browsers*)))))
         (args (mapcar (lambda (arg) (format nil arg url)) (cdr command))))
    (cond (command
           (when out
             (format out "~&;; running [~s~{ ~s~}]..." (car command) args)
             (force-output (if (eq out t) *standard-output* out)))
           (#+WIN32 ext::launch #-WIN32 run-program (car command) :arguments args :wait nil)
           (when out
             (format out "done~%")))
          (t (format t "~s: no browser specified; please point your browser at
 --> <URL:~a>~%" 'browse-url url)))))

;;; see also clocc/cllib/net.lisp

(defmacro with-http-input ((var url) &body body)
  (if (symbolp var)
      `(with-open-stream (,var (open-http ,url)) ,@body)
      (multiple-value-bind (body-rest declarations) (SYSTEM::PARSE-BODY body)
        `(multiple-value-bind ,var (open-http ,url)
           (DECLARE (READ-ONLY ,@var) ,@declarations)
           (UNWIND-PROTECT
                (MULTIPLE-VALUE-PROG1 (PROGN ,@body-rest)
                  (when ,(first var) (CLOSE ,(first var))))
             (when ,(first var) (CLOSE ,(first var) :ABORT T)))))))
(defun open-http (url &key (if-does-not-exist :error))
  (unless (string-equal #1="http://" url
                        :end2 (min (length url) #2=#.(length #1#)))
    (error "~S: ~S is not an HTTP URL" 'open-http url))
  (format t "~&;; connecting to ~S..." url) (force-output)
  (let* ((host-end (position #\/ url :start #2#)) status code
         (host (subseq url #2# host-end)) content-length
         (path (if host-end (subseq url host-end) "/"))
         (sock (handler-bind ((error (lambda (c)
                                       (unless (eq if-does-not-exist :error)
                                         (format t "cannot connect to ~S: ~A~%"
                                                 host c)
                                         (return-from open-http nil)))))
                 (socket:socket-connect 80 host :external-format :dos))))
    (format t "connected...") (force-output)
    (format sock "GET ~A HTTP/1.0~%User-agent: ~A~%Host: ~A~%Accept: */*~%Connection: close~2%" path (lisp-implementation-type) host) ; request
    (write-string (setq status (read-line sock))) (force-output)
    (let* ((pos1 (position #\Space status))
           (pos2 (position #\Space status :start (1+ pos1))))
      (setq code (parse-integer status :start pos1 :end pos2)))
    (when (>= code 400)
      (case if-does-not-exist
        (:error (error (TEXT "~S: error ~D: ~S") 'open-http code status))
        (t (close sock)
           (return-from open-http nil))))
    (if (>= code 300)        ; redirection
        (loop :for res = (read-line sock)
          :until (string-equal #3="Location: " res
                               :end2 (min (length res) #4=#.(length #3#)))
          :finally (let ((new-url (subseq res #4#)))
                     (format t " --> ~S~%" new-url)
                     (unless (string-equal #1# new-url
                                           :end2 (min (length new-url) #2#))
                       (setq new-url (string-concat "http://" host new-url)))
                     (return-from open-http (open-http new-url))))
        ;; drop response headers
        (loop :for line = (read-line sock) :while (plusp (length line)) :do
          (when (string-equal #5="Content-Length: " line
                              :end2 (min (length line) #6=#.(length #5#)))
            (format t "...~:D bytes"
                    (setq content-length (parse-integer line :start #6#))))
          :finally (terpri)))
    (values sock content-length)))
(defun get-clhs-map (stream)
  "Download and install the CLHS map."
  (format t "~&;; ~S(~S)..." 'get-clhs-map stream) (force-output)
  (loop :with good = 0 :for symbol-name = (read-line stream nil nil)
    :and destination = (read-line stream nil nil)
    :and total :upfrom 0
    :while (and symbol-name destination) :do
    (multiple-value-bind (symbol status) (find-symbol symbol-name "COMMON-LISP")
      (cond (status
             (incf good)
             (setf (documentation symbol 'sys::clhs)
                   (subseq destination #.(length "../"))))
            (t (warn (TEXT "~S is not found") symbol-name))))
    :finally (format t "~:D/~:D symbol~:P~%" good total)))
(let ((clhs-map-source nil) (clhs-map-good nil))
  ;; if clhs-map-source is the same as (clhs-root), do nothing and
  ;; return (clhs-root); otherwise set clhs-map-source to (clhs-root)
  ;; and try to get the clhs map from (clhs-root)
  ;; nil return value means no map exists
  (defun ensure-clhs-map ()
    "make sure that the CLHS map is present"
    (let ((clhs-root (clhs-root)) opener)
      (when (and clhs-root (string/= clhs-map-source clhs-root))
        (setq clhs-map-source clhs-root
              opener (if (string-equal #1="http://" clhs-root
                                       :end2 (min (length clhs-root)
                                                  #.(length #1#)))
                         #'open-http #'open))
        (with-open-stream (s (or (funcall opener (string-concat clhs-root "Data/Map_Sym.txt") :if-does-not-exist nil)
                                 (funcall opener (string-concat clhs-root "Data/Symbol-Table.text") :if-does-not-exist nil)))
          (unless s
            (warn (TEXT "~S returns invalid value ~S, fix it, ~S or ~S")
                  'clhs-root clhs-root '(getenv "CLHSROOT")
                  '*clhs-root-default*)
            (return-from ensure-clhs-map))
          (get-clhs-map s))
        (setq clhs-map-good t))
      (and clhs-map-good clhs-root))))
(defun get-string-map (stream &aux (table (make-hash-table :test 'equal)))
  (format t "~&;; ~S(~S)..." 'get-string-map stream) (force-output)
  (loop :for total :upfrom 0 :and id = (read-line stream nil nil)
    :and destination = (read-line stream nil nil)
    :while (and id destination) :do
    (when (or (find-if #'whitespacep id) (find-if #'whitespacep destination)
              (zerop (length id)) (zerop (length destination)))
      (warn "~S: invalid map ~S --> ~S" 'get-string-map id destination)
      (return-from get-string-map nil))
    (let ((old (gethash id table)))
          (when old (warn "~S: remapping ~S: ~S --> ~S"
                          'get-string-map id old destination)))
    (setf (gethash id table) destination)
    :finally (format t "~:D ID~:P~%" total))
  table)
(let ((impnotes-map-source nil) (impnotes-map-good nil))
  ;; if impnotes-map-source is the same as (impnotes-root), do nothing
  ;; and return (and impnotes-map-good (impnotes-root));
  ;; otherwise set impnotes-map-source to (impnotes-root) and try to get
  ;; the impnotes map from (impnotes-root)
  ;; nil return value means no map exists
  (defun ensure-impnotes-map ()
    "make sure that the impnotes map is present"
    (let ((impnotes-root (impnotes-root))
          (dest (lambda (id) (string-concat "#" id))))
      (when (and impnotes-root (string/= impnotes-map-source impnotes-root))
        (setq impnotes-map-source impnotes-root)
        (case (char impnotes-root (1- (length impnotes-root)))
          ((#\/ #+win32 #\\) ; chunked impnotes ==> get id-href
           (let ((opener (if (string-equal #1="http://" impnotes-root
                                           :end2 (min (length impnotes-root)
                                                      #.(length #1#)))
                             #'open-http #'open)))
             (with-open-stream (s (funcall opener (string-concat impnotes-root
                                                                 "id-href.map")
                                           :if-does-not-exist nil))
               (unless s
                 #2=(warn (TEXT "~S returns invalid value ~S, fix it, ~S or ~S")
                          'impnotes-root impnotes-root '(getenv "IMPNOTES")
                          '*impnotes-root-default*)
                 (return-from ensure-impnotes-map))
               (let ((table (get-string-map s)))
                 (unless table   ; no table --> bail out
                   #2# (return-from ensure-impnotes-map))
                 (setq dest (lambda (id) (gethash id table))))))))
        (with-open-file (in (clisp-data-file "Symbol-Table.text"))
          (format t "~&;; ~S(~S)..." 'ensure-impnotes-map (truename in))
          (force-output)
          (loop :for count :upfrom 0 :and sym = (read-line in nil nil)
            :and id = (read-line in nil nil)
            :while (and sym id) :do
            (let ((destination (funcall dest id)))
              (if destination
                  (setf (documentation (read-from-string sym) 'sys::impnotes)
                        destination)
                  (warn (TEXT "~S: invalid id ~S for symbol ~S")
                        'ensure-impnotes-map id sym)))
            :finally (format t "~:D ID~:P~%" count)))
        (setq impnotes-map-good t)) ; success
      (and impnotes-map-good impnotes-root))))

(defmethod documentation ((obj symbol) (type (eql 'sys::clhs)))
  (when (and (eq (symbol-package obj) #,(find-package "CL")) (ensure-clhs-map))
    (let ((doc (call-next-method)))
      (when doc (string-concat (clhs-root) doc)))))

(defmethod documentation ((obj symbol) (type (eql 'sys::impnotes)))
  (when (ensure-impnotes-map)
    (let ((doc (or (call-next-method)
                   (documentation (symbol-package obj) 'sys::impnotes))))
      (when doc (string-concat (impnotes-root) doc)))))

(defmethod documentation ((obj package) (type (eql 'sys::impnotes)))
  (let ((doc (sys::package-documentation obj)))
    (and (consp doc) (second doc))))

(defmethod (setf documentation) (new-value (obj package)
                                 (type (eql 'sys::impnotes)))
  (let ((doc (sys::package-documentation obj)))
    (if (consp doc)
        (setf (second doc) new-value)
        (setf (sys::package-documentation obj)
              (list "see the implementation notes" new-value)))))

;; what is the right place for these?
(setf (documentation (find-package "GRAY") 'sys::impnotes) "gray")
(setf (documentation (find-package "CL-USER") 'sys::impnotes) "clupack")
(setf (documentation (find-package "CS-CL-USER") 'sys::impnotes) "cs-clu")
(setf (documentation (find-package "CS-CL") 'sys::impnotes) "package-case")
#+screen (setf (documentation (find-package "SCREEN") 'sys::impnotes) "screen")
(setf (documentation (find-package "SOCKET") 'sys::impnotes) "socket")
#+generic-streams
(setf (documentation (find-package "GSTREAM") 'sys::impnotes) "gstream")
(setf (documentation (find-package "I18N") 'sys::impnotes) "i18n")
(setf (documentation (find-package "FFI") 'sys::impnotes) "dffi")
(setf (documentation (find-package "CUSTOM") 'sys::impnotes) "customize")
(setf (documentation (find-package "CHARSET") 'sys::impnotes) "charset")

(defun clhs (symbol &key (browser *browser*) (out *standard-output*))
  "Dump the CLHS doc for the symbol."
  (warn "function ~S is deprecated, set ~S and use ~S instead"
        'clhs '*browser* 'describe)
  (browse-url (or (documentation symbol 'sys::clhs)
                  (error "No HyperSpec doc for ~S" symbol))
              :browser browser :out out))
