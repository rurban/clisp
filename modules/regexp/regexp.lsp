;; Module for regular expression searching/matching in CLISP
;; Bruno Haible 14.4.1995, 18.4.1995

(in-package "REGEXP")

(export '(match match-start match-end match-string regexp-quote))

(use-package "FFI")

; Common OS definitions:
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


(def-c-call-out regcomp (:arguments (preg (c-ptr regex_t) :out)
                                    (pattern c-string)
                                    (cflags int)
                        )
                        (:return-type int)
)
(def-c-call-out regexec (:arguments (preg (c-ptr regex_t))
                                    (string c-string)
                                    (nmatch size_t)
                                    (pmatch (c-ptr (c-array regmatch_t 0)))
                                    (eflags int)
                        )
                        (:return-type int)
)
(def-c-call-out regerror (:arguments (errcode int)
                                     (preg (c-ptr regex_t))
                                     (errbuf (c-ptr character))
                                     (errbuf_size size_t)
                         )
                         (:return-type size_t)
)
(def-c-call-out regfree (:arguments (preg (c-ptr regex_t)))
                        (:return-type nil)
)

|#

;; This interface is not exactly adapted to our needs. We introduce
;; slightly modified functions.
#|
extern int mregcomp (regex_t **ppreg, const char *pattern, int cflags);
extern int regexec (const regex_t *preg, const char *string, size_t nmatch,
                    regmatch_t pmatch[], int eflags);
extern const char *mregerror (int errcode, const regex_t *preg);,
extern void mregfree (regex_t *preg);
|#

(eval-when (compile load eval) (defconstant num-matches 10))
(def-c-call-out mregcomp (:arguments (ppreg (c-ptr regex_t-ptr) :out)
                                     (pattern c-string)
                                     (cflags int)
                         )
                         (:return-type int)
)
(def-c-call-out regexec (:arguments (preg regex_t-ptr)
                                    (string c-string)
                                    (nmatch size_t)
                                    (pmatch (c-ptr (c-array regmatch_t #.num-matches)) :out)
                                    (eflags int)
                        )
                        (:return-type int)
)
(def-c-call-out mregerror (:arguments (errcode int)
                                      (preg regex_t-ptr)
                          )
                          (:return-type c-string :malloc-free)
)
(def-c-call-out mregfree (:arguments (preg regex_t-ptr))
                         (:return-type nil)
)
; cflags values
(defconstant REG_EXTENDED 1)
(defconstant REG_ICASE    2)
(defconstant REG_NEWLINE  4)
(defconstant REG_NOSUB    8)
; eflags values
(defconstant REG_NOTBOL   1)
(defconstant REG_NOTEOL   2)

;; The following implementation of MATCH compiles the pattern once for every
;; search.
(defun match-once (pattern string &key (start 0) (end nil) (case-insensitive nil))
  ; Prepare the string.
  (unless (and (eql start 0) (null end))
    (unless end (setq end (length string)))
    (setq string (make-array (- end start) :element-type 'character
                                           :displaced-to string
                                           :displaced-index-offset start
  ) )            )
  ; Compile the pattern.
  (multiple-value-bind (errcode compiled-pattern)
      (mregcomp pattern (if case-insensitive REG_ICASE 0))
    (unless (zerop errcode)
      (error "~S: ~A" 'match (mregerror errcode compiled-pattern))
    )
    ; Do the search.
    (multiple-value-bind (errcode matches)
        (regexec compiled-pattern string #.num-matches 0)
      ; Free the compiled pattern.
      (mregfree compiled-pattern)
      ; Compute return values.
      (if (zerop errcode)
        (values-list ; the first value will be non-NIL
          (map 'list (if (eql start 0)
                       #'identity
                       #'(lambda (match)
                           (incf (regmatch_t-rm_so match) start)
                           (incf (regmatch_t-rm_eo match) start)
                           match
                         )
                     )
                     (delete-if #'minusp matches :key #'regmatch_t-rm_so)
        ) )
        nil
) ) ) )

;; The following implementation of MATCH compiles the pattern only once per
;; Lisp session, if it is a literal string.
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
(defun mregfree-finally (compiled-pattern)
  (when (validp compiled-pattern) ; beware: compiled-pattern could come from a previous session
    (mregfree compiled-pattern)
) )
(defun %match (patternbox string &key (start 0) (end nil) (case-insensitive nil))
  ; Compile the pattern, if not already done.
  (let ((compiled-pattern (if case-insensitive (cddr patternbox) (cadr patternbox))))
    (unless (and compiled-pattern (validp compiled-pattern))
      (setq compiled-pattern
        (multiple-value-bind (errcode compiled-pattern)
            (mregcomp (car patternbox) (if case-insensitive REG_ICASE 0))
          (unless (zerop errcode)
            (error "~S: ~A" 'match (mregerror errcode compiled-pattern))
          )
          ; Arrange that when compiled-pattern is garbage-collected,
          ; mregfree will be called.
          (finalize compiled-pattern #'mregfree-finally)
          (if case-insensitive
            (setf (cddr patternbox) compiled-pattern)
            (setf (cadr patternbox) compiled-pattern)
    ) ) ) )
    ; Prepare the string.
    (unless (and (eql start 0) (null end))
      (unless end (setq end (length string)))
      (setq string (make-array (- end start) :element-type 'character
                                             :displaced-to string
                                             :displaced-index-offset start
    ) )            )
    ; Do the search.
    (multiple-value-bind (errcode matches)
        (regexec compiled-pattern string #.num-matches 0)
      ; Compute return values.
      (if (zerop errcode)
        (values-list ; the first value will be non-NIL
          (map 'list (if (eql start 0)
                       #'identity
                       #'(lambda (match)
                           (incf (regmatch_t-rm_so match) start)
                           (incf (regmatch_t-rm_eo match) start)
                           match
                         )
                     )
                     (delete-if #'minusp matches :key #'regmatch_t-rm_so)
        ) )
        nil
) ) ) )

(setf (fdefinition 'match-start) (fdefinition 'regmatch_t-rm_so))
(setf (fdefinition '(setf match-start))
      #'(lambda (new-value match) (setf (regmatch_t-rm_so match) new-value))
)

(setf (fdefinition 'match-end) (fdefinition 'regmatch_t-rm_eo))
(setf (fdefinition '(setf match-end))
      #'(lambda (new-value match) (setf (regmatch_t-rm_eo match) new-value))
)

; Convert a match (of type regmatch_t) to a substring.
(defun match-string (string match)
  (let ((start (regmatch_t-rm_so match))
        (end (regmatch_t-rm_eo match)))
    (make-array (- end start) :element-type 'character
                              :displaced-to string
                              :displaced-index-offset start
) ) )

; Utility function
(defun regexp-quote (string)
  (let ((qstring (make-array 10 :element-type 'character
                                :adjustable t :fill-pointer 0)))
    (map nil #'(lambda (c)
                 (case c
                   ((#\$ #\^ #\. #\* #\[ #\] #\\) ; #\+ #\?
                    (vector-push-extend #\\ qstring)
                 ) )
                 (vector-push-extend c qstring)
               )
             string
    )
    qstring
) )

