(in-package "AFFI")

(export 'make-partial-affi-file)

(defun read-fd-directive (stream macro-char)
  (declare (ignore macro-char)
           (special *fd-readtable*))
  (unless (eq *readtable* *fd-readtable*)
    (error "Not the FD-readtable: ~S" *readtable*))
  (let ((*package* (find-package "KEYWORD"))
        (case (readtable-case *readtable*)))
    (unwind-protect
         (progn
           (setf (readtable-case *readtable*) :upcase)
           (read stream t nil t))
      (setf (readtable-case *readtable*) case))))

;;Problem: AmigaGuide uses * in OpenAmigaGuideA()
;; Replace it with attrs, see AutoDocs, which gives
;; APTR OpenAmigaGuideA( struct NewAmigaGuide *nag, struct TagItem *attrs )(a0/a1);

(defun make-fd-readtable (&optional (readtable-case :upcase))
  (let ((readtable (copy-readtable nil)))
    ;; , and / separate registers
    (set-syntax-from-char #\, #\  readtable)
    (set-syntax-from-char #\/ #\  readtable)
    ;; * serves as a comment
    (set-syntax-from-char #\* #\; readtable)
    ;; ## introduces specials
    (set-macro-character #\# #'read-fd-directive t readtable)
    ;; we choose to preserve case for all function names
    (setf (readtable-case readtable) readtable-case)
    readtable))

;; TODO maybe better read strings by preserving case, because output will look nicer?

(defvar *fd-readtable* (make-fd-readtable)) ;or :preserve

(defun read-from-fd (stream &optional (eof-error-p t))
  (let* ((unique "EoF")
         (read
          ;; switch readtables very temporarily only
          (let ((*readtable* *fd-readtable*))
            (read stream nil unique))))
    (if (eq read unique)
        (if eof-error-p
            (error "FD file ~S ended" stream)
            :end)
        read)))

;; Bignum Maske: wb/AddAppIconA
(defun calc-reg-mask (regs mask)
  (if (null regs) mask
      (calc-reg-mask
       (rest regs)
       (logior
        (ash mask 4)
        (1+ (position
             (symbol-name (first regs))
             '("D0" "D1" "D2" "D3" "D4" "D5" "D6" "D7"
               "A0" "A1" "A2" "A3" "A4" "A5" "A6")
             :test #'string-equal))))))

;; funinfo ist {(function . (offset . mask))}*
(defun read-fd-functions (stream skip offset funinfo)
  (let ((tag (read-from-fd stream nil)))
    (etypecase tag
      ;;(null (return-from read-fd-functions funinfo))
      (keyword
       (ecase tag
         (:base (error "##base only allowed once: ~S"))
         (:bias (setq offset (- (read-from-fd stream))))
         (:public  (setq skip nil))
         (:private (setq skip t))
         (:end (return-from read-fd-functions funinfo)))
       (read-fd-functions stream skip offset funinfo))
      (symbol                           ;tag is function name
       (let ((vars (read-from-fd stream))
             (regs (read-from-fd stream)))
         (unless (listp vars)
           (error "No FFI variable names read from ~S: ~S" stream vars))
         (unless (listp regs)
           (error "No FFI register specification read from ~S: ~S" stream regs))
         (read-fd-functions
          stream skip (- offset 6)
          (if skip funinfo
              ;;TODO hashtable instead of alist
              (cons (list* tag offset (calc-reg-mask (reverse regs) 0))
                funinfo))))))))

;;Problem: cia_lib.fd contains no library base
;; libinfo ist (basename . {(function offset . mask)}*)
(defun parse-fd (name)
  (with-open-file (file name :direction :input)
    (let ((*package* (load-time-value *package*)))
      (unless (eq (read-from-fd file) :base)
        (error "FD file does not start with ##base: ~S" file))
      (let ((library (read-from-fd file)))
        (unless (symbolp library)
          (error "Not a library base name: ~S in ~S" library file))
        (cons (if (char= (schar (symbol-name library) 0) #\_) ;strip leading underscore
                  (intern (subseq (symbol-name library) 1))
                  library)
              (read-fd-functions file nil -30 ()))))))

;; Problem: current AFFI.D doesn't handle more than 8 args (uint32)
(defun mention-large-masks (libinfos)
  (dolist (libinfo libinfos)
    ;;TODO hashtable instead of alist
    (dolist (funinfo (rest libinfo))
      (unless (typep (cddr funinfo) '(unsigned-byte 32))
        (format t "~&;;Too big mask for ~A in ~A~%" (car funinfo) (car libinfo))))))
;;Bignum mask for AddAppIconA in _WorkbenchBase
;;Bignum mask for CreateBehindHookLayer in _LayersBase
;;Bignum mask for CreateUpfrontHookLayer in _LayersBase
;;Bignum mask for CreateBehindLayer in _LayersBase
;;Bignum mask for CreateUpfrontLayer in _LayersBase
;;Bignum mask for ScrollWindowRaster in _IntuitionBase
;;Bignum mask for NewModifyProp in _IntuitionBase
;;Bignum mask for AutoRequest in _IntuitionBase
;;Bignum mask for ModifyProp in _IntuitionBase
;;Bignum mask for WriteChunkyPixels in _GfxBase
;;Bignum mask for ScrollRasterBF in _GfxBase
;;Bignum mask for WritePixelArray8 in _GfxBase
;;Bignum mask for ReadPixelArray8 in _GfxBase
;;Bignum mask for TextFit in _GfxBase
;;Bignum mask for BltMaskBitMapRastPort in _GfxBase
;;Bignum mask for BltBitMapRastPort in _GfxBase
;;Bignum mask for ClipBlit in _GfxBase
;;Bignum mask for ScrollRaster in _GfxBase
;;Bignum mask for BltPattern in _GfxBase
;;Bignum mask for BltTemplate in _GfxBase
;;Bignum mask for BltBitMap in _GfxBase
;;Bignum mask for DoPkt in _DOSBase

(defun make-partial-affi-file (name)
  (let ((fdlibinfo (parse-fd (format () "FD:~A_lib.fd" (pathname-name name))))
        (*package* (load-time-value *package*)))
    (with-open-file (stream
                     (namestring (make-pathname :type "affi" :defaults name))
                     :direction :output
                     :if-exists :error) ;at least for now
      ;;(format stream "(in-package ~S)" (package-name (load-time-value *package*)))
      (princ "(in-package \"AFFI\")" stream)(terpri stream)
      (format stream "(declare-library-base :~A ~S)~%" (car fdlibinfo) name)
      (format stream "(format *error-output* \"~~&;;; Warning: Please adapt the prototypes for ~~S manually!~~%\" ~S)~%~%" name)
      (dolist (ffinfo (nreverse (rest fdlibinfo)))
        ;; the current implementation of AFFI.D is limited:
        (unless (typep (cddr ffinfo) '(unsigned-byte 32))
          (princ ";; The following mask is too large for AFFI:" stream)
          (terpri stream))
        (format stream "(defflibfun '~S '~S ~D #x~X '*"
                (car ffinfo)            ;Function
                (car fdlibinfo)         ;Library
                (cadr ffinfo)           ;Offset
                (cddr ffinfo))          ;Mask
        ;; Here we abuse the knowledge that AFFI.D:reg_coding is 4:
        (dotimes (i (ceiling (integer-length (cddr ffinfo)) 4))
          (princ " '*" stream))
        (princ ")" stream)(terpri stream))
      (format stream "~%(provide ~S)~%" name)
      (pathname stream))))
