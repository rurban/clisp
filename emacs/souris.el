;; Mouse handling like X11 does it.
;; In particular:
;;   Click middle must insert at the cursor, without repositioning the cursor.
;;
;; I got this file from Bruno Berstel <berstel@ilog.fr>.

(global-set-key '(button1) 'mouse-track)
(global-set-key '(button2) 'my-x-insert-selection)
(global-set-key '(button3) 'mouse-track-adjust)

(global-set-key '(shift button1) 'mouse-scroll-down-full)
(global-set-key '(shift button2) 'my-mouse-what-line)
(global-set-key '(shift button3) 'mouse-scroll-up-full)

(global-set-key '(control button1) 'my-mouse-del-char)
(global-set-key '(control button2) 'my-x-mouse-kill)
(global-set-key '(control button3) 'my-x-mouse-kill-line)

(global-set-key '(meta button1) 'my-x-mouse-mark-sexp)
(global-set-key '(meta button2) 'my-x-mouse-copy-sexp)
(global-set-key '(meta button3) 'my-x-mouse-cut-sexp)

(global-set-key '(shift control button1) 'mouse-keep-one-window)
(global-set-key '(shift control button2) 'mouse-select-and-split)
(global-set-key '(shift control button3) 'mouse-delete-window)

(global-set-key '(shift meta button1) 'my-mouse-at-top)
(global-set-key '(shift meta button2) 'my-mouse-at-center)
(global-set-key '(shift meta button3) 'my-mouse-at-bottom)

(global-set-key '(control meta button1) 'my-mouse-select-window-and-bury-buffer)
(global-set-key '(control meta button2) 'my-mouse-switch-to-buffer)
(global-set-key '(control meta button3) 'my-mouse-bury-buffer)

;; Worse than mouse-track-adjust. But maybe you like it better than
;; mouse-track-adjust if you have set zmacs-regions = nil.
;(defun my-mouse-track-adjust (event)
;  (interactive "e")
;  (x-disown-selection)
;  (setq mouse-track-previous-point (point))
;  (save-excursion (mouse-track-adjust event)))

(defun my-x-insert-selection (event)
  (interactive "e")
  (x-insert-selection t nil))

(defun my-mouse-del-char (event)
  (interactive "@e")
  (mouse-set-point event)
  (delete-char 1 nil))

(defun my-x-mouse-kill (event)
  (interactive "@e")
  (let ((old-point (point)))
    (mouse-set-point event)
    (let ((s (buffer-substring old-point (point))))
      (x-own-clipboard s)
      (x-store-cutbuffer s))
    ;; Je rajoute le t, parce que sinon des fois il dit "Bad arg range".
    (kill-region old-point (point) t)))

(defun my-x-mouse-kill-line (event)
  (interactive "@e")
  (mouse-set-point event)
  (let* ((beg (point))
         (end (save-excursion
                (if (eobp)
                    (signal 'end-of-buffer nil))
                (if (or (looking-at "[ \t]*$") (and kill-whole-line (bolp)))
                    (forward-line 1)
                  (end-of-line))
                (point)))
         (s (buffer-substring beg end)))
    (x-own-clipboard s)
    (x-store-cutbuffer s)
    (kill-region beg end t)))

(defun my-mouse-what-line (event)
  (interactive "@e")
  (save-excursion
    (mouse-set-point event)
    (what-line)
    (sit-for 1)))

(defun my-mouse-at-top (event)
  (interactive "@e")
  (mouse-set-point event)
  (recenter 0))

(defun my-mouse-at-center (event)
  (interactive "@e")
  (mouse-set-point event)
  (recenter))

(defun my-mouse-at-bottom (event)
  (interactive "@e")
  (mouse-set-point event)
  (recenter -1))

(defun my-x-mouse-mark-sexp (event)
  (interactive "e")
  (let ((window (event-window event)))
    (or window (error "not in a window"))
    (save-window-excursion
      (select-window window)
      (save-excursion
        (mouse-set-point event)
        (let* ((end (progn (condition-case ed
                               (forward-sexp 1)
                             (error (forward-char 1)))
                           (point)))
               (beg (progn (forward-sexp -1) (point))))
          (mouse-track-maybe-own-selection (cons beg end) 'PRIMARY))))))

(defun my-x-mouse-copy-sexp (event)
  (interactive "e")
  (let ((window (event-window event)) s)
    (or window (error "not in a window"))
    (save-window-excursion
      (select-window window)
      (save-excursion
        (mouse-set-point event)
        (let* ((end (progn (condition-case ed
                               (forward-sexp 1)
                             (error (forward-char 1)))
                           (point)))
               (beg (progn (forward-sexp -1) (point))))
          (setq s (buffer-substring beg end)))))
    (insert s)))

;(defun my-x-mouse-cut-sexp (event)
;  (interactive "e")
;  (let ((window (event-window event)))
;    (or window (error "not in a window"))
;    (save-window-excursion
;      (select-window window)
;      (save-excursion
;       (mouse-set-point event)
;       (let* ((end (progn (condition-case ed
;                              (forward-sexp 1)
;                            (error (forward-char 1)))
;                          (point)))
;              (beg (progn (forward-sexp -1) (point)))
;              (s (buffer-substring beg end)))
;         (x-own-clipboard s)
;         (x-store-cutbuffer s)
;         (kill-region beg end t))))))

(defun my-x-mouse-cut-sexp (event)
  (interactive "e")
  (mouse-set-point event)
  (let* ((end (progn (condition-case ed
                         (forward-sexp 1)
                       (error (forward-char 1)))
                     (point)))
         (beg (progn (forward-sexp -1) (point)))
         (s (buffer-substring beg end)))
    (x-own-clipboard s)
    (x-store-cutbuffer s)
    (kill-region beg end t)))

(defun my-mouse-select-window-and-bury-buffer (event)
  (interactive "@e")
  (bury-buffer))

(defun my-mouse-switch-to-buffer (event)
  (interactive "@e")
  (switch-to-buffer (other-buffer (current-buffer)) ()))

(defun my-mouse-bury-buffer (event)
  (interactive "e")
  (let ((click-window (or (event-window event) (error "not in a window")))
        (initial-window (selected-window)))
    (select-window click-window)
    (bury-buffer)
    (select-window initial-window)))
