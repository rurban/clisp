;;;; Open a little window and draw lines
(in-package "AFFI-DEMOS")

(use-package "AFFI")

(export 'make-window-demo)

(defun test-pointer (p &optional error)
  (if (nzero-pointer-p p) p
      (when error (error "Null pointer assertion failed!"))))

;;; declare used libraries
;; care is taken that the compiled file will not load any fd files

(eval-when (compile eval load)
  (declare-library-base :SysBase "exec.library")
  (declare-library-base :IntuitionBase "intuition.library"))
(eval-when (eval compile)
  (require-library-functions "exec.library"
    :import '("AllocMem" "FreeMem"))
  (require-library-functions "intuition.library"
    :import '("OpenWindowTagList" "CloseWindow"
              "DrawBorder")))

(eval-when (compile eval load)
  (declare-library-base :UtilityBase "utility.library"))
(eval-when (eval compile)
  (require-library-functions "utility.library"
    :import '("AllocateTagItems" "FreeTagItems")))


;;; taglist management
;; I wrote no parser for C include files so I have to define
;; everything myself.

(defconstant MEMF_ANY 1)
(defconstant MEMF_CLEAR (ash 1 16))
(defconstant TAG_DONE 0)
(defconstant TAG_USER (ash 1 31))
(defconstant WA_Dummy   (+ TAG_USER 99))
(defconstant WA_Left    (+ WA_Dummy #x1))
(defconstant WA_Width   (+ WA_Dummy #x3))
(defconstant WA_Height  (+ WA_Dummy #x4))
(defconstant WA_IDCMP   (+ WA_Dummy #x7))
(defconstant WA_DragBar (+ WA_Dummy #x1f))
(defconstant WA_DepthGadget (+ WA_Dummy #x20))
(defconstant WA_CloseGadget (+ WA_Dummy #x21))

;; caller must unwind-protect
;; taglist is simple because it cannot accept Lisp strings (for WA_Title)
(defun make-simple-taglist (&rest args)
  ;; always terminate taglist with TAG_DONE
  (let ((length (length args)))
    (unless (evenp length)
      (error "TagList of uneven length: ~S" args))
    (with-open-library ("utility.library")
      (let ((mem (mlibcall AllocateTagItems (1+ (/ length 2)))))
        ;; mem-write does test-pointer
        (do ((i 0 (1+ i))
             (args args (rest args)))
            ((null args)
             (mem-write mem 4 TAG_DONE (* 4 i)))
          (mem-write mem (if (typep (first args) '(unsigned-byte 32)) 4 -4)
                     (first args) (* 4 i)))
        mem))))

;;; open window

(defun make-window-taglist (taglist)
  (with-open-library ("utility.library")
  (with-open-library ("intuition.library")
    (let ((win-tags (apply #'make-simple-taglist taglist)))
      (test-pointer win-tags :error)
      (unwind-protect
           (mlibcall OpenWindowTagList 0 win-tags)
        (mlibcall FreeTagItems win-tags)))
  )))

#|
(defmacro with-simple-taglist ((var . args) &body body)
  `(let ((,var (make-simple-taglist ,@args)))
     (unless (nzero-pointer-p ,var)
       (error "Couldn't allocate Taglist"))
     (unwind-protect (progn ,@body)
       (mlibcall FreeTagItems ,var))))
|#

;; unwind-protect is very useful for all software development
(defun make-window-demo
    (&key width (height 150)
          (taglist
           (list* WA_Left 20
                  WA_Height height
                  ;;WA_CloseGadget 1
                  WA_DragBar 1
                  WA_DepthGadget 1
                  WA_IDCMP 0
                  (if width (list WA_Width width)))))
  (with-open-library ("intuition.library")
    (let (win)
      (unwind-protect
           (progn
             (setq win (make-window-taglist taglist))
             ;; avoid ~X as window may be FOREIGN-POINTER in future
             (let ((*print-base* 16)) (format t "~&Window ~S~%" win))
             (window-fun1 win))
        (when (test-pointer win nil) (mlibcall CloseWindow win))))))

;;; memory primitives

(defmacro with-mem ((var size flags) &body body)
  (let ((sym (gensym)))
    `(LET* ((,sym ,size)
            (,var (MLIBCALL AllocMem ,sym ,flags)))
       (WHEN (NZERO-POINTER-P ,var)
         (UNWIND-PROTECT (PROGN ,@body)
           (MLIBCALL FreeMem ,var ,sym))))))


;;; draw lines

;; Instead of all these mem-read/write with offset and type, it would
;; be better to define a simple equivalent of ffi:def-c-struct
;; (with-struct (LeftBorder) 'Window &body)

(defun draw-one-rectangle (window border x y width height)
  (let (;;(rastport (mem-read window '* 50))
        (xy (mem-read border '* 8)))
    (mem-write xy -2 x 0)
    (mem-write xy -2 y 2)
    (mem-write xy -2 (+ x width) 4)
    (mem-write xy -2 y 6)
    (mem-write xy -2 (+ x width) 8)
    (mem-write xy -2 (+ y height) 10)
    (mem-write xy -2 x 12)
    (mem-write xy -2 (+ y height) 14)
    (mem-write xy -2 x 16)
    (mem-write xy -2 y 18)
    (mlibcall DrawBorder (mem-read window '* 50) border 0 0)))

(defun draw-rectangles (window border x y width height)
  (do ((x x (+ x 2))
       (y y (+ y 2))
       (width width (- width 4))
       (height height (- height 4)))
      ((or (>= 0 width) (>= 0 height)))
    (draw-one-rectangle window border x y width height)))

(defconstant JAM1 0)

(defun window-fun1 (window)
  (with-open-library ("exec.library")
    (with-mem (border 16 (logior MEMF_ANY MEMF_CLEAR))
      (with-mem (xy (* 2 2 5) MEMF_ANY)
        (mem-write border '-2 (mem-read window -1 54) 0) ;LeftEdge:=BorderLeft
        (mem-write border '-2 (mem-read window -1 55) 2) ;TopEdge :=BorderTop
        (mem-write border '1 1 4)       ;FrontPen, possibly use window->RPort->FgPen
        (mem-write border '1 2 5)       ;BackPen
        (mem-write border '1 JAM1 6)    ;DrawMode
        (mem-write border '-1 5 7)      ;Count
        (mem-write border '* xy 8)
        (mem-write border '* 0 12)
        (draw-rectangles
         window border
         0 0
         (- (mem-read window -2  8) (mem-read window -1 56) (mem-read window -1 54) 1) ;Width-BorderRight-BorderLeft-1
         (- (mem-read window -2 10) (mem-read window -1 57) (mem-read window -1 55) 1)) ;Height-BorderBottom-BorderTop-1
        (break "Have fun with window ~S" window)
        (let ((sec 2))
          (format t "~&Waiting ~R second~P.~%" sec sec)
          (sleep sec))))))

