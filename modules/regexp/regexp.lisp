;; Module for regular expression searching/matching in CLISP
;; Bruno Haible 14.4.1995, 18.4.1995
;; Sam Steingold 1999-10-28

(defpackage "REGEXP"
  (:documentation
   "POSIX Regular Expressions - matching, compiling, executing.")
  (:use "LISP" "FFI")
  (:export "MATCH" "MATCH-START" "MATCH-END" "MATCH-STRING" "REGEXP-QUOTE"
           "REGEXP-COMPILE" "REGEXP-EXEC" "REGEXP-SPLIT" "WITH-LOOP-SPLIT"))

(in-package "REGEXP")

(default-foreign-language :stdc)

;; Common OS definitions:
(def-c-type size_t uint)

#|
; Intermediate types not actually exported by regex.h:
(def-c-type reg_syntax_t uint)
(def-c-struct re_pattern_buffer
  (buffer c-pointer)
  (allocated ulong)
  (used ulong)
  (syntax reg_syntax_t)
  (fastmap c-pointer)
  (translate c-pointer)
  (re_nsub size_t)
  (flags uint8)
)
(def-c-type %regex_t re_pattern_buffer)
(eval-when (load compile eval) (defconstant sizeof-%regex_t (sizeof '%regex_t)))
(def-c-type regex_t (c-array uchar #.sizeof-%regex_t))
|#
(def-c-type regex_t-ptr c-pointer)

; Types exported by regex.h:
(def-c-type regoff_t int)
(def-c-struct regmatch_t
  (rm_so regoff_t)
  (rm_eo regoff_t)
)

;; Functions exported by regex.h:

#| ;; This documentation comes from regex.h and regex.c.

extern int regcomp (regex_t *preg, const char *pattern, int cflags);

   regcomp takes a regular expression as a string and compiles it.

   PREG is a regex_t *.  We do not expect any fields to be initialized,
   since POSIX says we shouldn't.  Thus, we set

     `buffer' to the compiled pattern;
     `used' to the length of the compiled pattern;
     `syntax' to RE_SYNTAX_POSIX_EXTENDED if the
       REG_EXTENDED bit in CFLAGS is set; otherwise, to
       RE_SYNTAX_POSIX_BASIC;
     `newline_anchor' to REG_NEWLINE being set in CFLAGS;
     `fastmap' and `fastmap_accurate' to zero;
     `re_nsub' to the number of subexpressions in PATTERN.

   PATTERN is the address of the pattern string.

   CFLAGS is a series of bits which affect compilation.

     If REG_EXTENDED is set, we use POSIX extended syntax; otherwise, we
     use POSIX basic syntax.

     If REG_NEWLINE is set, then . and [^...] don't match newline.
     Also, regexec will try a match beginning after every newline.

     If REG_ICASE is set, then we considers upper- and lowercase
     versions of letters to be equivalent when matching.

     If REG_NOSUB is set, then when PREG is passed to regexec, that
     routine will report only success or failure, and nothing about the
     registers.

   It returns 0 if it succeeds, nonzero if it doesn't.  (See regex.h for
   the return codes and their meanings.)


extern int regexec (const regex_t *preg, const char *string, size_t nmatch,
                    regmatch_t pmatch[], int eflags);

   regexec searches for a given pattern, specified by PREG, in the
   string STRING.

   If NMATCH is zero or REG_NOSUB was set in the cflags argument to
   `regcomp', we ignore PMATCH.  Otherwise, we assume PMATCH has at
   least NMATCH elements, and we set them to the offsets of the
   corresponding matched substrings.

   EFLAGS specifies `execution flags' which affect matching: if
   REG_NOTBOL is set, then ^ does not match at the beginning of the
   string; if REG_NOTEOL is set, then $ does not match at the end.

   We return 0 if we find a match and REG_NOMATCH if not.


extern size_t regerror (int errcode, const regex_t *preg,
                        char *errbuf, size_t errbuf_size);

   Returns a message corresponding to an error code, ERRCODE, returned
   from either regcomp or regexec.   We don't use PREG here.


extern void regfree (regex_t *preg);

   Free dynamically allocated space used by PREG.


(def-call-out regcomp
    (:arguments (preg (c-ptr regex_t) :out)
                (pattern c-string)
                (cflags int))
  (:return-type int))
(def-call-out regexec
    (:arguments (preg (c-ptr regex_t))
                (string c-string)
                (nmatch size_t)
                (pmatch (c-ptr (c-array regmatch_t 0)))
                (eflags int))
  (:return-type int))
(def-call-out regerror
    (:arguments (errcode int)
                (preg (c-ptr regex_t))
                (errbuf (c-ptr character))
                (errbuf_size size_t))
  (:return-type size_t))
(def-call-out regfree (:arguments (preg (c-ptr regex_t)))
  (:return-type nil))

;|#

;; This interface is not exactly adapted to our needs. We introduce
;; slightly modified functions.
#|
extern int mregcomp (regex_t **ppreg, const char *pattern, int cflags);
extern int regexec (const regex_t *preg, const char *string, size_t nmatch,
                    regmatch_t pmatch[], int eflags);
extern const char *mregerror (int errcode, const regex_t *preg);,
extern void mregfree (regex_t *preg);
;|#

(eval-when (compile load eval) (defconstant num-matches 10))
(def-call-out mregcomp
    (:arguments (ppreg (c-ptr regex_t-ptr) :out)
                (pattern c-string)
                (cflags int))
  (:return-type int))
(def-call-out regexec
    (:arguments (preg regex_t-ptr)
                (string c-string)
                (nmatch size_t)
                (pmatch (c-ptr (c-array regmatch_t #.num-matches)) :out)
                (eflags int))
  (:return-type int))
(def-call-out mregerror (:arguments (errcode int) (preg regex_t-ptr))
  (:return-type c-string :malloc-free))
(def-call-out mregfree (:arguments (preg regex_t-ptr))
  (:return-type nil))
; cflags values
(defconstant REG_EXTENDED 1)
(defconstant REG_ICASE    2)
(defconstant REG_NEWLINE  4)
(defconstant REG_NOSUB    8)
; eflags values
(defconstant REG_NOTBOL   1)
(defconstant REG_NOTEOL   2)

(defun mregfree-finally (compiled-pattern)
  ;; beware: compiled-pattern could come from a previous session
  (when (validp compiled-pattern)
    (mregfree compiled-pattern)))

(defun regexp-compile (pattern &optional (case-sensitive t))
  (let (errcode compiled-pattern)
    (assert (zerop (setf (values errcode compiled-pattern)
                         (mregcomp pattern (if case-sensitive 0 REG_ICASE))))
            (pattern)
            "~s: ~a" 'regexp-compile (mregerror errcode compiled-pattern))
    ;; Arrange that when compiled-pattern is garbage-collected,
    ;; mregfree will be called.
    (ext:finalize compiled-pattern #'mregfree-finally)
    compiled-pattern))

(setf (fdefinition 'match-start) (fdefinition 'regmatch_t-rm_so))
(setf (fdefinition '(setf match-start))
      (lambda (new-value match) (setf (regmatch_t-rm_so match) new-value)))

(setf (fdefinition 'match-end) (fdefinition 'regmatch_t-rm_eo))
(setf (fdefinition '(setf match-end))
      (lambda (new-value match) (setf (regmatch_t-rm_eo match) new-value)))

(defun regexp-exec (compiled-pattern string &key (start 0) (end nil))
  (assert (stringp string) (string)
          "~s: the second argument must be a string, not ~s"
          'regexp-exec string)
  (let* ((len (length string))
         (end (cond ((null end) len)
                    ((>= end len) len)
                    ((minusp end) (+ len end))
                    (t end)))
         ;; Prepare the string.
         (string
           (if (and (eql start 0) (eql end len))
             string
             (make-array (- end start)
                         :element-type 'character
                         :displaced-to string
                         :displaced-index-offset start))))
    (declare (string string))
    (multiple-value-bind (errcode matches)
        (regexec compiled-pattern string #.num-matches 0)
      ;; Compute return values.
      (if (zerop errcode)
        (values-list          ; the first value will be non-NIL
         (map 'list (if (eql start 0)
                      #'identity
                      (lambda (match)
                        (incf (match-start match) start)
                        (incf (match-end match) start)
                        match))
              (delete-if #'minusp matches :key #'match-start)))
        nil))))

;; The following implementation of MATCH compiles the pattern
;; once for every search.
(defun match-once (pattern string &key (start 0) (end nil) (case-sensitive t))
  (regexp-exec (regexp-compile pattern case-sensitive)
               string :start start :end end))

;; The following implementation of MATCH compiles the pattern
;; only once per Lisp session, if it is a literal string.
(defmacro match (pattern string &rest more-forms)
  (if (stringp pattern)
    `(%MATCH (MATCHER ,pattern) ,string ,@more-forms)
    `(MATCH-ONCE ,pattern ,string ,@more-forms)
) )

(defmacro matcher (pattern)
  (declare (string pattern))
  `(LOAD-TIME-VALUE (%MATCHER ,pattern))
)
(defun %matcher (pattern)
  (list* pattern nil nil)
  ; car = pattern,
  ; cadr = compiled pattern, case sensitive,
  ; cddr = compiled pattern, case insensitive.
)

(defun %match (patternbox string &key (start 0) (end nil) (case-sensitive t))
  ;; Compile the pattern, if not already done.
  (let ((compiled-pattern
          (if case-sensitive (cadr patternbox) (cddr patternbox))))
    (unless (and compiled-pattern (validp compiled-pattern))
      (setq compiled-pattern (regexp-compile (car patternbox) case-sensitive))
      (if case-sensitive
        (setf (cadr patternbox) compiled-pattern)
        (setf (cddr patternbox) compiled-pattern)))
    (regexp-exec compiled-pattern string :start start :end end)))

; Convert a match (of type regmatch_t) to a substring.
(defun match-string (string match)
  (let ((start (match-start match))
        (end (match-end match)))
    (make-array (- end start)
                :element-type 'character
                :displaced-to string
                :displaced-index-offset start
) ) )

; Utility function
(defun regexp-quote (string)
  (let ((qstring (make-array 10 :element-type 'character
                                :adjustable t :fill-pointer 0)))
    (map nil (lambda (c)
               (case c
                 ((#\$ #\^ #\. #\* #\[ #\] #\\) ; #\+ #\?
                   (vector-push-extend #\\ qstring)))
               (vector-push-extend c qstring))
         string)
    qstring))

(defun regexp-split (pattern string &key (start 0) end (case-sensitive t))
  "Split the STRING by the regexp PATTERN.
Return a list of substrings of STRINGS."
  (loop
    :with compiled =
            (if (stringp pattern)
              (regexp-compile pattern case-sensitive)
              pattern)
    :for match = (regexp-exec compiled string :start start :end end)
    :collect
      (make-array (- (if match (match-start match) (length string)) start)
                  :element-type 'character
                  :displaced-to string
                  :displaced-index-offset start)
    :while match
    :do (setq start (match-end match))))

(defmacro with-loop-split ((var stream pattern
                            &key (start 0) end (case-sensitive t))
                           &body forms)
  "Read from STREAM one line at a time, binding VAR to the split line.
The line is split with REGEXP-SPLIT using PATTERN."
  (let ((compiled-pattern (gensym "WLS-")) (line (gensym "WLS-")))
    `(loop
       :with ,compiled-pattern =
         (if (stringp ,pattern)
           (regexp-compile ,pattern ,case-sensitive)
           ,pattern)
       :and ,var
       :for ,line = (read-line ,stream nil nil)
       :while ,line
       :do (setq ,var
             (regexp-split ,compiled-pattern ,line :start ,start :end ,end))
      ,@forms)))
