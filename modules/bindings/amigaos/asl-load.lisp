;;;; Use ASL filerequester to load a file

;;; Show how library functions can be defined at compile-time only
;;; when using mlibcall and proper eval-when, all references are compiled in

(defpackage "AFFI-DEMOS" (:use "AFFI"))
(in-package "AFFI-DEMOS")

(eval-when (compile eval load)
  (declare-library-base :AslBase "asl.library"))

;;importing some function definitions
(eval-when (eval compile)
  (require-library-functions "asl.library"
    :import '("AllocAslRequest" "FreeAslRequest" "AslRequest")) )

(defun AddPart2 (dir file)
  (declare (type string dir file))
  ;; pretend we don't know dos.library/AddPart()
  (concatenate
   'string
   dir
   (unless (zerop (length dir))
           (unless (find (char dir (1- (length dir))) ":/") "/"))
   file))

(defun asl-load (&rest keys &key &allow-other-keys)
  (let ((file
   (with-open-library ("asl.library")
     (let ((filerequest (mlibcall AllocAslRequest 0 0))) ; type, tags
       (when (nzero-pointer-p filerequest)
         (unwind-protect
              (when (mlibcall AslRequest filerequest 0)
                (let ((file (mem-read (mem-read filerequest '* 4) 'string))
                      (dir  (mem-read (mem-read filerequest '* 8) 'string)))
                  (unless (zerop (length file))
                    (addpart2 dir file))))
           (mlibcall FreeAslRequest filerequest)))))))
    (when file                          ;didn't cancel
      (apply #'load file keys))))

