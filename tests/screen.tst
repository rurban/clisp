;; -*- Lisp -*- vim:filetype=lisp
;; demo for the SCREEN package
;; <http://clisp.cons.org/impnotes/screen.html>
;; usage:
;;  (welcome-simple)
;;  (welcome-banner)
;;  (input-line "enter a line")

(use-package "SCREEN")

(defun wait-for-space (line col)
  (set-window-cursor-position *window* line col)
  (highlight-on *window*)
  (write-string "hit space when done" *window*)
  (highlight-off *window*)
  (print (with-keyboard (read-char *keyboard-input*))))

;;;
;;; simple demo: say hi and wait for keypress
;;;

(defun welcome-simple ()
  (with-window
    (clear-window *window*)
    (set-window-cursor-position *window* 2 2)
    (write-string "Welcome to " *window*)
    (highlight-on *window*)
    (write-string "CLISP" *window*)
    (highlight-off *window*)
    (wait-for-space 4 4)))

;;;
;;; show the clisp banner character-by-character in random order
;;;

(defun clisp-banner ()
  "get the clisp welcome message"
  (let* ((argv (ext:argv))
         (lispinit (aref argv (1+ (position "-m" argv :test #'string=)))))
    (with-open-stream (s (ext:make-pipe-input-stream
                          (format nil "'~a' -norc -b '~a' -m '~a' < /dev/null 2>/dev/null"
                                  (aref argv 0) (namestring *lib-directory*)
                                  (namestring (merge-pathnames
                                               lispinit *lib-directory*)))))
      (loop :for line = (read-line s nil nil) :while line :collect line))))

(defun lines-to-vector (lines)
  "convert list of lines to a vector of (char lineno column)"
  (loop :with vec = (make-array 10 :adjustable t :fill-pointer 0)
    :for line :in lines :and lineno :upfrom 0 :do
    (loop :for ch :across line :and colno :upfrom 0
      :unless (char= #\space ch)
      :do (vector-push-extend (list ch lineno colno) vec))
    :finally (return vec)))

(defparameter *delay* 0.03)
(defparameter *start-line* 5)
(defparameter *start-column* 5)

(defun show-char (list &key (delay *delay*) (start-line *start-line*)
                  (start-column *start-column*))
  "output a char at the given position, then sleep"
  (destructuring-bind (ch lineno colno) list
    (set-window-cursor-position *window* (+ lineno start-line)
                                (+ colno start-column))
    (write-char ch *window*)
    (sleep delay)))

(defun vector-shuffle (vec)     ; see clocc/cllib/math.lisp
  "generate a random permutation of the vector in place."
  (loop :for ii :downfrom (1- (length vec)) :to 1
    :for jj = (random (1+ ii))
    :unless (= jj ii)
    :do (rotatef (aref vec ii) (aref vec jj))))

(defun welcome-banner (&key (*delay* *delay*) (*start-line* *start-line*)
                       (*start-column* *start-column*))
  "show the clisp banner character-by-character in random order"
  (let* ((lines (clisp-banner))
         (banner (copy-seq (lines-to-vector lines))))
    (vector-shuffle banner)
    (with-window
      (clear-window *window*)
      (map nil #'show-char banner)
      (wait-for-space (+ *start-line* (length lines) *start-line*) 10))))

;;;
;;; ask for an input line
;;;

(defun input-line (prompt &key (*start-line* *start-line*)
                   (*start-column* *start-column*))
  (with-window
    (set-window-cursor-position *window* *start-line* *start-column*)
    (highlight-on *window*)
    (write-string prompt *window*)
    (highlight-off *window*)
    (write-string ": " *window*)
    (multiple-value-bind (row col) (window-cursor-position *window*)
      (set-window-cursor-position *window* (+ *start-line* 1) *start-column*)
      (write-string "hit <enter> when done")
      (set-window-cursor-position *window* row col)
      (let ((rite col) (pos col) (left col)
            (before nil) (after nil) (insert-modep t))
        ;; pos == (length before)
        (with-keyboard
          (loop :for ic = (read-char *keyboard-input*)
            :for ch = (system::input-character-char ic)
            :until (eql ch #\Return) :do ; not char=, ch may be nil!
            (cond ((eql ch #\Backspace)
                   (when (< left pos)
                     (pop before)
                     (decf pos)))
                  (ch
                   (write-char ch *window*)
                   (incf pos) (push ch before)
                   (when (or insert-modep (null after)) (incf rite))
                   (unless insert-modep (pop after)))
                  ((case (system::input-character-key ic)
                     (:LEFT (when (< left pos)
                              (push (pop before) after)
                              (decf pos)))
                     (:RIGHT (when (< pos rite)
                               (push (pop after) before)
                               (incf pos)))
                     (:HOME (setq pos left
                                  after (nreconc before after)
                                  before nil))
                     (:END (setq pos rite
                                 before (nreconc after before)
                                 after nil))
                     (:DELETE (pop after))
                     (:INSERT (setq insert-modep (not insert-modep))))))
            (set-window-cursor-position *window* row pos)
            (clear-window-to-eol *window*)
            (write-string (coerce after 'string) *window*)
            (set-window-cursor-position *window* row pos)))
        (coerce (nreverse (nreconc after before)) 'string)))))
