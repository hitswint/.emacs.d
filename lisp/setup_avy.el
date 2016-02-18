;; =======================avy===========================
(use-package ace-pinyin
  ;; Enabled at commands.
  :defer t
  :bind (("C-h" . ace-pinyin-jump-char))
  :init
  (use-package avy
    ;; Enabled at commands.
    :defer t
    :bind (("C-x C-h" . avy-goto-word-0)
           ("C-c C-h" . avy-goto-word-1))
    :init
    (bind-key "C-h"  'avy-isearch isearch-mode-map)
    :config
    ;; at-full会造成ace-pinyin显示不正确。
    (setq avy-keys (number-sequence ?a ?z))
    (setq avy-styles-alist '((avy-goto-char . at)
                             (avy-goto-char-2 . at)
                             (avy-goto-word-0 . at)))
    (setq avy-dispatch-alist '((23 . avy-action-kill)
                               (67108923 . avy-action-mark)
                               (134217847 . avy-action-copy))))
  (use-package avy-zap
    ;; Enabled at commands.
    :bind (("M-z" . avy-zap-to-char-dwim)
           ("M-Z" . avy-zap-up-to-char-dwim)))
  :config
  (setq ace-pinyin-use-avy t)
  (ace-pinyin-global-mode +1)
  (bind-key "C-h" 'avy-goto-char)
  (defun ace-pinyin-jump-char (query-char)
    "AceJump with pinyin by QUERY-CHAR."
    (interactive (list (if ace-pinyin-use-avy
                           (read-char "char: ")
                         (read-char "Query Char:"))))
    (cond
     ((= query-char 8)
      (call-interactively 'avy-goto-char-2))
     ((= query-char 12)
      (call-interactively 'avy-goto-line))
     ((= query-char 19)
      (call-interactively 'swint-avy-goto-char-timer))
     ((= query-char 134217847)
      (call-interactively 'avy-copy-line))
     ((= query-char 23)
      (call-interactively 'avy-move-line))
     ((= query-char 134217751)
      (call-interactively 'avy-copy-region))
     (t (cond (ace-pinyin-mode
               (ace-pinyin--jump-impl query-char))
              (ace-pinyin-use-avy
               (funcall ace-pinyin--original-avy query-char))
              (t
               (funcall ace-pinyin--original-ace query-char))))))
  (defun swint-avy--read-candidates ()
    "Read as many chars as possible and return their occurences.
At least one char must be read, and then repeatedly one next char
may be read if it is entered before `avy-timeout-seconds'.  `DEL'
deletes the last char entered, and `RET' exits with the currently
read string immediately instead of waiting for another char for
`avy-timeout-seconds'.
The format of the result is the same as that of `avy--regex-candidates'.
This function obeys `avy-all-windows' setting."
    (let ((str "") char break overlays regex)
      (unwind-protect
          (progn
            (while (and (not break)
                        (setq char
                              (read-char (format "char%s: "
                                                 (if (string= str "")
                                                     str
                                                   (format " (%s)" str)))
                                         t
                                         (and (not (string= str ""))
                                              avy-timeout-seconds))))
              ;; Unhighlight
              (dolist (ov overlays)
                (delete-overlay ov))
              (setq overlays nil)
              (cond
               ;; Handle RET
               ((= char 13)
                (setq break t))
               ;; Handle DEL
               ((= char 127)
                (let ((l (length str)))
                  (when (>= l 1)
                    (setq str (substring str 0 (1- l))))))
               (t
                (setq str (concat str (list char)))))
              ;; Highlight
              (when (>= (length str) 1)
                (let ((case-fold-search
                       (or avy-case-fold-search (string= str (downcase str))))
                      found)
                  (avy-dowindows current-prefix-arg
                    (dolist (pair (avy--find-visible-regions
                                   (window-start)
                                   (window-end (selected-window) t)))
                      (save-excursion
                        (goto-char (car pair))
                        (if (or (string-match "[iuv]" str) ;当字符串中有iuv时，不转换string
                                (string-empty-p (pinyin-search--pinyin-to-regexp str))) ;当搜索中文或符号时，不转换string
                            (setq regex (regexp-quote str))
                          (setq regex (concat (regexp-quote str) "\\|" (pinyin-search--pinyin-to-regexp str))))
                        (while (re-search-forward regex (cdr pair) t)
                          (unless (get-char-property (1- (point)) 'invisible)
                            (let ((ov (make-overlay
                                       (match-beginning 0)
                                       (match-end 0))))
                              (setq found t)
                              (push ov overlays)
                              (overlay-put ov 'window (selected-window))
                              (overlay-put ov 'face 'avy-goto-char-timer-face)))))))
                  ;; No matches at all, so there's surely a typo in the input.
                  (unless found (beep)))))
            (nreverse (mapcar (lambda (ov)
                                (cons (cons (overlay-start ov)
                                            (overlay-end ov))
                                      (overlay-get ov 'window)))
                              overlays)))
        (dolist (ov overlays)
          (delete-overlay ov)))))
  (defun swint-avy-goto-char-timer (&optional arg)
    "Read one or many consecutive chars and jump to the first one.
  The window scope is determined by `avy-all-windows' (ARG negates it)."
    (interactive "P")
    (let ((avy-all-windows (if arg
                               (not avy-all-windows)
                             avy-all-windows)))
      (avy-with swint-avy-goto-char-timer
        (avy--process
         (swint-avy--read-candidates)
         (avy--style-fn avy-style))))))
;; =======================avy===========================
(provide 'setup_avy)
