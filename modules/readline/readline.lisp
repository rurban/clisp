;;; readline & history interface
;;; http://cnswww.cns.cwru.edu/~chet/readline/readline.html
;;; http://cnswww.cns.cwru.edu/~chet/readline/history.html
;;;
;;; Copyright (C) 2005 by Sam Steingold
;;; This is Free Software, covered by the GNU GPL (v2)
;;; See http://www.gnu.org/copyleft/gpl.html

(defpackage "READLINE"
  (:use "CL" "EXT" "FFI")
  (:shadowing-import-from "EXPORTING"
     #:defconstant #:defun #:defmacro #:defvar
     #:def-c-type #:def-c-enum #:def-c-struct #:def-c-var #:def-call-out))

(in-package "READLINE")

;;; types and constants

;;; foreign function definitions
(default-foreign-language :stdc)

(c-lines "#include <stdio.h>~%")

;;; ------ readline ------

(c-lines "#include <readline/readline.h>~%")

(def-c-type readline-command (c-function (:arguments (rep int) (char int))))

(def-call-out readline (:name "readline")
  (:arguments (prompt c-string))
  (:return-type c-string))

(def-call-out set-prompt (:name "rl_set_prompt")
  (:arguments (prompt c-string))
  (:return-type int))

(def-call-out initialize (:name "rl_initialize")
  (:arguments) (:return-type int))

(def-call-out add-defun (:name "rl_add_defun")
  (:arguments (name c-string) (callback readline-command) (key int))
  (:return-type int))

;;; Binding Keys

(def-call-out bind-key (:name "rl_bind_key")
  (:arguments (key int) (command readline-command))
  (:return-type int))

(def-call-out unbind-key (:name "rl_unbind_key")
  (:arguments (key int))
  (:return-type int))

(def-call-out variable-bind (:name "rl_variable_bind")
  (:arguments (variable c-string) (value c-string))
  (:return-type int))

;;; Associating Function Names and Bindings

(def-call-out named-function (:name "rl_named_function")
  (:arguments (name c-string))
  (:return-type readline-command))

;;; Redisplay

(def-call-out redisplay (:name "rl_redisplay")
  (:arguments) (:return-type int))

;;; variables

(def-c-var library-version (:name "rl_library_version") (:type c-string))
(def-c-var readline-version (:name "rl_readline_version") (:type int))
(def-c-var editing-mode (:name "rl_editing_mode") (:type int))
(def-c-var insert-mode (:name "rl_insert_mode") (:type int))
(def-c-var readline-name (:name "rl_readline_name") (:type c-string)) ;"CLISP"

;;; ------ history ------

(c-lines "#include <readline/history.h>~%")

;;; History List Management

(def-call-out using-history (:name "using_history")
  (:arguments) (:return-type nil))

(def-call-out add-history (:name "add_history")
  (:arguments (line c-string)) (:return-type nil))

(def-call-out clear-history (:name "clear_history")
  (:arguments) (:return-type nil))

(def-call-out stifle-history (:name "stifle_history")
  (:arguments (count int)) (:return-type nil))

(def-call-out unstifle-history (:name "unstifle_history")
  (:arguments) (:return-type int))

(def-call-out history-stifled-p (:name "history_is_stifled")
  (:arguments) (:return-type int))

;;; Information About the History List

(def-call-out where-history (:name "where_history")
  (:arguments) (:return-type int))

(def-call-out history-total-bytes (:name "history_total_bytes")
  (:arguments) (:return-type int))

;;; Moving Around the History List

(def-call-out history-set-pos (:name "history_set_pos")
  (:arguments (pos int)) (:return-type int))

;;; Searching the History List

(def-call-out history-search (:name "history_search")
  (:arguments (string c-string) (direction int)) (:return-type int))

(def-call-out history-search-prefix (:name "history_search_prefix")
  (:arguments (string c-string) (direction int)) (:return-type int))

(def-call-out history-search-pos (:name "history_search_pos")
  (:arguments (string c-string) (direction int) (pos int)) (:return-type int))

;;; Managing the History File

(def-call-out read-history (:name "read_history")
  (:arguments (file c-string)) (:return-type int))

(def-call-out read-history-range (:name "read_history_range")
  (:arguments (file c-string) (start int) (end int)) (:return-type int))

(def-call-out write-history (:name "write_history")
  (:arguments (file c-string)) (:return-type int))

(def-call-out append-history (:name "append_history")
  (:arguments (count int) (file c-string)) (:return-type int))

(def-call-out history-truncate-file (:name "history_truncate_file")
  (:arguments (file c-string) (nlines int)) (:return-type int))

;;; done

(pushnew :readline *features*)
(provide "readline")
(pushnew "READLINE" custom:*system-package-list* :test #'string=)
