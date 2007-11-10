;; -*- Lisp -*- vim:filetype=lisp
;; some tests for clx/new-clx
;; clisp -K full -E 1:1 -q -norc -i ../tests/tests -x '(run-test "clx/new-clx/test")'

(defparameter *dpy* (show (xlib:open-default-display))) *dpy*

(xlib:closed-display-p *dpy*) NIL
(stringp (show (xlib:display-authorization-data *dpy*))) T
(stringp (show (xlib:display-authorization-name *dpy*))) T
(listp (show (multiple-value-list (xlib:display-authorization *dpy*)))) T
(integerp (show (xlib:display-protocol-major-version *dpy*))) T
(integerp (show (xlib:display-protocol-minor-version *dpy*))) T
(listp (show (multiple-value-list (xlib:display-protocol-version *dpy*)))) T
(format t "~29b" (xlib:display-resource-id-base *dpy*)) NIL
(format t "~29b" (xlib:display-resource-id-mask *dpy*)) NIL
(xlib:display-noop *dpy*) 1
(listp (show (xlib:display-roots *dpy*))) T
(listp (show (multiple-value-list (xlib:display-vendor *dpy*)))) T
(stringp (show (xlib:display-vendor-name *dpy*))) T
(integerp (show (xlib:display-release-number *dpy*))) T
(listp (show (xlib:display-plist *dpy*))) T
(stringp (show (xlib:display-host *dpy*))) T
(listp (show (multiple-value-list (xlib:pointer-control *dpy*)))) T
(listp (show (xlib:pointer-mapping *dpy*))) T
(listp (show (xlib:font-path *dpy*) :pretty t)) T
(defparameter *font-count*
  (let ((font-names (xlib:list-font-names *dpy* "*")))
    (ext:times (map-into font-names (lambda (n) (xlib:open-font *dpy* n))
                         font-names))
    (mapc #'xlib:close-font font-names)
    (show (length font-names)))) *FONT-COUNT*
(let ((fonts (ext:times (xlib:list-fonts *dpy* "*"))))
  (mapc #'xlib:close-font fonts)
  (= *font-count* (length fonts))) T

(dotimes (i 8) (show (xlib:cut-buffer *dpy* :buffer i))) NIL
(loop :for i :from 1 :to 1000
  :always (= i (xlib:find-atom *dpy* (xlib:atom-name *dpy* i))))
T
(block xlib:atom-name
  (handler-bind ((error (lambda (c)
                          (princ-error c)
                          (return-from xlib:atom-name 42))))
    (xlib:atom-name *dpy* 0)))
42                              ; no atom 0!

(multiple-value-bind (kc% b% bp bd lm gar arm) (xlib:keyboard-control *dpy*)
  (show (list kc% b% bp bd lm gar arm) :pretty t)
  (xlib:change-keyboard-control
   *dpy* :KEY-CLICK-PERCENT kc%
   :BELL-PERCENT b% :BELL-PITCH bp :BELL-DURATION bd
   :KEY 80 :AUTO-REPEAT-MODE (if (plusp (aref arm 80)) :on :off)))
NIL

(vectorp (show (xlib:query-keymap *dpy*))) T
(listp (show (multiple-value-list (xlib:display-keycode-range *dpy*)))) T
(integerp (show (xlib:display-max-request-length *dpy*))) T
(integerp (show (xlib::display-extended-max-request-length *dpy*))) T
(let ((r (show (xlib::display-resource-manager-string *dpy*))))
  (or (null r) (stringp r))) T
(integerp (show (xlib:display-motion-buffer-size *dpy*))) T
(listp (show (xlib:display-pixmap-formats *dpy*) :pretty t)) T
(xlib:bitmap-format-p (show (xlib:display-bitmap-format *dpy*))) T
(symbolp (show (xlib:display-byte-order *dpy*))) T
(listp (show (multiple-value-list (xlib:display-protocol-version *dpy*)))) T
(listp (show (multiple-value-list (xlib:display-vendor *dpy*)))) T
(listp (show (multiple-value-list (xlib:global-pointer-position *dpy*)))) T
(integerp (show (xlib:display-nscreens *dpy*))) T

(defparameter *screen* (show (xlib:display-default-screen *dpy*))) *SCREEN*

(let ((n (show (setf (xlib:display-default-screen *dpy*) *screen*))))
  (list (equalp (slot-value *screen* 'xlib::ptr)
                (slot-value (nth n (xlib:display-roots *dpy*)) 'xlib::ptr))
        (= (setf (xlib:display-default-screen *dpy*) n) n)
        (equalp (slot-value *screen* 'xlib::ptr)
                (slot-value (nth n (xlib:display-roots *dpy*)) 'xlib::ptr))))
(T T T)
(let ((r (show (xlib::screen-resource-string *screen*))))
  (or (null r) (stringp r))) T
(integerp (show (xlib:screen-black-pixel *screen*))) T
(integerp (show (xlib:screen-white-pixel *screen*))) T
(integerp (show (xlib:screen-event-mask-at-open *screen*))) T
(integerp (show (xlib:screen-height *screen*))) T
(integerp (show (xlib:screen-height-in-millimeters *screen*))) T
(integerp (show (xlib:screen-width *screen*))) T
(integerp (show (xlib:screen-width-in-millimeters *screen*))) T
(integerp (show (xlib:screen-max-installed-maps *screen*))) T
(integerp (show (xlib:screen-min-installed-maps *screen*))) T
(integerp (show (xlib:screen-root-depth *screen*))) T
(xlib:visual-info-p (show (xlib:screen-root-visual-info *screen*) :pretty t)) T
(typep (show (xlib:screen-save-unders-p *screen*)) 'boolean) T
(symbolp (show (xlib:screen-backing-stores *screen*))) T
(listp (show (xlib:screen-depths *screen*) :pretty t)) T

(defparameter *visual* (show (xlib:screen-root-visual *screen*))) *VISUAL*

(listp (show (xlib:screen-plist *screen*))) T
(xlib:visual-info-p (show (xlib:visual-info *dpy* *visual*) :pretty t)) T

(defparameter *root* (show (xlib:screen-root *screen*))) *ROOT*
(listp (show (xlib:list-properties *root*) :pretty t)) T

(defparameter *colormap* (show (xlib:screen-default-colormap *screen*)))
*COLORMAP*
(defparameter *color*
  (show (multiple-value-list (xlib:lookup-color *colormap* "red"))))
*COLOR*
(multiple-value-bind (pixel screen-color exact-color)
    (xlib:alloc-color *colormap* (first *color*))
  (show (list pixel screen-color exact-color))
  (xlib:free-colors *colormap* (list pixel)))
NIL

(defparameter *font* (show (xlib:open-font *dpy* "fixed"))) *FONT*
(listp (show (multiple-value-list (xlib:text-extents *font* "abcd")))) T
(listp (show (xlib:font-properties *font*) :pretty t)) T
(xlib:font-name *font*) "fixed"
(xlib:font-direction *font*) :LEFT-TO-RIGHT
(xlib:font-all-chars-exist-p *font*) NIL
(integerp (show (xlib:min-char-width *font*))) T
(integerp (show (xlib:max-char-width *font*))) T

(defparameter *window*
  (multiple-value-bind (window revert) (xlib:input-focus *dpy*)
    (show (list :window window :revert revert) :pretty t)
    window))
*WINDOW*
(listp (show (xlib:list-properties *window*) :pretty t)) T
(listp (show (xlib:window-plist *window*) :pretty t)) T
(xlib:window-equal *window* *window*) T
(xlib:window-equal *window* *root*) NIL
(typep (show (xlib:window-bit-gravity *window*)) 'xlib:bit-gravity) T
(typep (show (xlib:window-gravity *window*)) 'xlib:win-gravity) T
(integerp (show (xlib:window-id *window*))) T
(xlib:window-p (show (xlib:drawable-root *window*))) T
(listp (show (multiple-value-list (xlib:query-tree *window*)) :pretty t)) T
(length (show (multiple-value-list (xlib:query-pointer *window*)) :pretty t)) 8
(listp (show (xlib:motion-events *window*))) T
(defparameter *window-position*
  (show (multiple-value-list (xlib:pointer-position *window*))))
*WINDOW-POSITION*
(xlib:warp-pointer *window* 10 10) NIL
(xlib:warp-pointer-relative *dpy* 10 10) NIL
(xlib:warp-pointer *window* (first *window-position*)
                   (second *window-position*))
NIL
(equal *window-position* (multiple-value-list (xlib:pointer-position *window*)))
T
(dolist (selection '("PRIMARY" "SECONDARY" "CLIPBOARD"))
  (let ((w (xlib:selection-owner *dpy* selection)))
    (or (null w) (xlib:window-p w) (error "~S is not a window" w))))
NIL

(defparameter *gcontext*
  (xlib:create-gcontext :drawable *window* :font *font*))
*GCONTEXT*
(integerp (show (xlib:text-width *gcontext* "abazonk"))) T

(xlib:free-gcontext *gcontext*) NIL
(xlib:close-font *font*) NIL

(let ((modifiers (multiple-value-list (xlib:modifier-mapping *dpy*))))
  (apply #'xlib:set-modifier-mapping *dpy*
         (show (mapcan #'list '(:SHIFT :LOCK :CONTROL
                                :MOD1 :MOD2 :MOD3 :MOD4 :MOD5)
                       modifiers)
               :pretty t)))
:SUCCESS

(let ((map (show (xlib:keyboard-mapping *dpy*) :pretty t)))
  (show (array-dimensions map))
  (list (eq map (xlib:keyboard-mapping *dpy* :data map))
        (xlib:change-keyboard-mapping
         *dpy* map :first-keycode (xlib:display-min-keycode *dpy*))
        (equalp map (xlib:keyboard-mapping *dpy*))))
(T NIL T)

(multiple-value-list (xlib:keysym->keycodes *dpy* 65)) (38)
(multiple-value-list (xlib:keysym->keycodes *dpy* #xFF52)) (98) ; Up
(xlib:keysym "Up") #xFF52

(xlib:keysym->character *dpy* 97)    #\a
(xlib:keysym->character *dpy* 97 4)  #\a ; 4 is <ctrl>
(xlib:keysym->character *dpy* 97 8)  #\a ; 8 is <meta>
(xlib:keysym->character *dpy* 65)    #\A
(xlib:keysym->character *dpy* 65 4)  #\A
(xlib:keysym->character *dpy* 65 8)  #\A
(xlib:keysym->character *dpy* #xFF52) ; #xFF52 is <up>
#+unicode #\FULLWIDTH_LATIN_SMALL_LETTER_R #-unicode NIL

(listp (show (loop :for i :from 0 :to 255
               :collect (xlib:keycode->character *dpy* i 0))
             :pretty t))
T

(defun c2s (index)
  (loop :for keycode :from 0 :to 255
    :collect (xlib:keycode->keysym *dpy* keycode index)))
C2S

(let ((l-255 (show (c2s 255) :pretty t)))
  (loop :for index :from 0 :to 254 :for l = (c2s index)
    :unless (equal l-255 l)
    :do (show (list index (diff-seq l-255 l)) :pretty t)))
NIL

(xlib:keysym-name (show (xlib:keysym "Down"))) "Down"

(let ((access (show (xlib:access-control *dpy*))))
  (assert (eq access (setf (xlib:access-control *dpy*) access)))
  t) T

(defparameter *access-hosts* (show (xlib:access-hosts *dpy*) :pretty t))
*ACCESS-HOSTS*
(xlib:add-access-host *dpy* "localhost") NIL
(every (lambda (x)
         (or (posix:hostent-p x)
             (and (listp x) (eq (car x) :SERVER-INTERPRETED))))
       (show (xlib:access-hosts *dpy*) :pretty t)) T
(xlib:remove-access-host *dpy* "localhost") NIL
(equalp *access-hosts* (show (xlib:access-hosts *dpy*) :pretty t)) T

(xlib:activate-screen-saver *dpy*) NIL
(xlib:reset-screen-saver *dpy*) NIL
(listp (show (multiple-value-list (xlib:screen-saver *dpy*)))) T
(dolist (ext (xlib:list-extensions *dpy*))
  (show (cons ext (multiple-value-list (xlib:query-extension *dpy* ext)))))
NIL

(xlib:bell *dpy* 50) NIL        ; signal that we are almost done

(xlib:display-force-output *dpy*) NIL
(xlib:display-finish-output *dpy*) NIL
(xlib:display-p (show (xlib:close-display *dpy*))) T

;; cleanup
(flet ((del (s) (makunbound s) (fmakunbound s) (unintern s)))
  (del '*dpy*)
  (del '*font-count*)
  (del '*screen*)
  (del '*visual*)
  (del '*root*)
  (del '*colormap*)
  (del '*color*)
  (del '*font*)
  (del '*window*)
  (del '*window-position*)
  (del '*gcontext*)
  (del 'c2s)
  (del '*access-hosts*))
T
