(with-open-file (infile "COPYRIGHT" :direction :input :if-does-not-exist :error)
  (with-open-file (outfile "COPYRIGHT.rtf" :direction :output
                           :if-exists :overwrite :if-does-not-exist :create)
    (write-string
     (do ((output (format nil "{\\rtf1\\ansi\\ansicpg1252\\deff0{\\fonttbl{\\f0\\fswiss\\fprq2\\fcharset0 Arial;}{\\f1\\fswiss\\fcharset0 Arial;}}~%\\viewkind4\\uc1\\pard\\lang1033\\f0\\fs20                                  %NAME% %VERSION%")
                  (if (and line
                           (not (regexp:match "-\*- coding: utf-8 -\*-" line)))
                      (format nil "~A~A\\par~%" output line)
                      output))
          (line "" (read-line infile nil)))
         ((not line) (format nil "~A~%}~%" output))
       (do ((match-oe (if line (regexp:match "ö" line))
		      (if line (regexp:match "ö" line)))
	    (match-ue (if line (regexp:match "ü" line))
		      (if line (regexp:match "ü" line))))
	   ((not (or match-oe match-ue)))
	 (setf line (cond
		      (match-oe
		       (format nil "~A~A~A"
                               (subseq line 0 (regexp:match-start match-oe))
                               "\\f1\\'f6\\f0 "
                               (subseq line (regexp:match-end match-oe)
                                       (length line))))
		      (match-ue
		       (format nil "~A~A~A"
                               (subseq line 0 (regexp:match-start match-ue))
                               "\\f1\\'f6\\f0 "
                               (subseq line (regexp:match-end match-ue)
				       (length line))))
		      (t line)))))
     outfile)))
    
