;;; avy
;; =====================avy=====================
(use-package avy
  :after ace-pinyin
  :config
  (bind-key "C-h" 'avy-isearch isearch-mode-map)
  (setq avy-keys (append (number-sequence ?a ?z) (number-sequence ?0 ?9) '(?, ?. ?/ ?' ?\; ? )))
  (setq avy-dispatch-alist '((?\C-\M-w . avy-action-kill-move)
                             (?\C-w . avy-action-kill-stay)
                             (?\C-\; . avy-action-mark)
                             (?\M-w . avy-action-copy)
                             (?\C-y . avy-action-yank)
                             (?\M-f . avy-action-ispell)))
  ;; 默认当前窗口，加C-u时所有窗口。
  (setq avy-all-windows nil)
  (setq avy-all-windows-alt t)
  ;; 修复at-full造成ace-pinyin显示不正确的问题。
  (defun avy--overlay-at-full (path leaf)
    "Create an overlay with PATH at LEAF.
PATH is a list of keys from tree root to LEAF.
LEAF is normally ((BEG . END) . WND)."
    (let* ((path (mapcar #'avy--key-to-char path))
           (str (propertize
                 (apply #'string (reverse path))
                 'face 'avy-lead-face))
           (len (length path))
           (beg (avy-candidate-beg leaf))
           (wnd (cdr leaf))
           end)
      (dotimes (i len)
        (set-text-properties (- len i 1) (- len i)
                             `(face ,(nth i avy-lead-faces))
                             str))
      (when (eq avy-style 'de-bruijn)
        (setq str (concat
                   (propertize avy-current-path
                               'face 'avy-lead-face-1)
                   str))
        (setq len (length str)))
      (with-selected-window wnd
        (save-excursion
          (goto-char beg)
          (let* ((lep (if (bound-and-true-p visual-line-mode)
                          (save-excursion
                            (end-of-visual-line)
                            (point))
                        (line-end-position)))
                 (len-and-str (avy--update-offset-and-str len str lep)))
            (setq len (car len-and-str))
            (setq str (cdr len-and-str))
            (setq end (if (= beg lep)
                          (1+ beg)
                        (min (+ beg
                                (cond
                                 ((or (eq (char-after) ?\t)) 1)
                                 ;; 当(point)和(+ (point) 1)位置处有中文字符时，确定正确的end位置。
                                 ((and (char-after (+ (point) 1))
                                       (> (+ (char-width (char-after)) (char-width (char-after (+ (point) 1)))) 2))
                                  (if (> len (char-width (char-after)))
                                      2 1))
                                 (t len)))
                             lep)))
            (when (and (bound-and-true-p visual-line-mode)
                       (> len (- end beg))
                       (not (eq lep beg)))
              (setq len (- end beg))
              (let ((old-str (apply #'string (reverse path))))
                (setq str
                      (substring
                       (propertize
                        old-str
                        'face
                        (if (= (length old-str) 1)
                            'avy-lead-face
                          'avy-lead-face-0))
                       0 len)))))))
      (avy--overlay
       str beg end wnd
       (lambda (str old-str)
         (cond ((string= old-str "\n")
                (concat str "\n"))
               ((string= old-str "\t")
                (concat str (make-string (max (- tab-width len) 0) ?\ )))
               (t
                ;; Add padding for wide-width character.
                ;; 确定是否添加padding。
                (save-excursion
                  (goto-char beg)
                  (if (and (char-after)
                           (/= (string-width str) (char-width (char-after)))
                           (char-after (+ (point) 1))
                           (/= (string-width str) (+ (char-width (char-after))
                                                     (char-width (char-after (+ (point) 1)))))
                           (char-after (+ (point) 2))
                           (/= (string-width str) (+ (char-width (char-after))
                                                     (char-width (char-after (+ (point) 1)))
                                                     (char-width (char-after (+ (point) 2))))))
                      (concat str " ")
                    str)))))))))
;; =====================avy=====================
;;; avy-zap
;; ===================avy-zap===================
(use-package avy-zap
  ;; avy-zap-up-to-char-dwim保留char，而avy-zap-to-char-dwim不保留。
  :commands (avy-zap-to-char-dwim avy-zap-up-to-char-dwim))
;; ===================avy-zap===================
;;; ace-pinyin
;; =================ace-pinyin==================
(use-package ace-pinyin
  :bind ("C-h" . ace-pinyin-jump-char)
  :config
  (setq ace-pinyin-use-avy t)
  (ace-pinyin-global-mode +1)
  (bind-key "C-h" 'avy-goto-char)
  (defun ace-pinyin-jump-char (query-char &optional arg)
    "AceJump with pinyin by QUERY-CHAR."
    (interactive (list (if ace-pinyin-use-avy
                           (read-char "char: ")
                         (read-char "Query Char:"))))
    (cond
     ((= query-char ?\C-u)
      (let ((current-prefix-arg '4))
        (call-interactively 'ace-pinyin-jump-char)))
     ((= query-char ?\C-h)
      (call-interactively 'avy-goto-char-2))
     ((= query-char ?\C-l)
      (call-interactively 'avy-goto-line))
     ((= query-char ?\C-s)
      (call-interactively 'swint-avy-goto-char-timer))
     ((= query-char ?\C-y)
      (call-interactively 'avy-copy-line))
     ((= query-char ?\C-\M-y)
      (call-interactively 'avy-copy-region))
     ((= query-char ?\M-y)
      (call-interactively 'avy-move-line))
     ((= query-char ?\M-Y)
      (call-interactively 'avy-move-region))
     ((= query-char ?\C-w)
      (call-interactively 'avy-kill-whole-line))
     ((= query-char ?\C-\M-w)
      (call-interactively 'avy-kill-region))
     ((= query-char ?\M-w)
      (call-interactively 'avy-kill-ring-save-whole-line))
     ((= query-char ?\M-W)
      (call-interactively 'avy-kill-ring-save-region))
     ((= query-char ?\C-x)
      (call-interactively 'avy-goto-word-0))
     ((= query-char ?\C-c)
      (call-interactively 'avy-goto-word-1))
     ((= query-char ?\C-d)
      (call-interactively 'avy-zap-to-char-dwim))
     ((= query-char ?\C-i)
      (call-interactively 'ace-link))
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
                                     (setq regex (pinyin-search--pinyin-to-regexp str))
                                     (while (re-search-forward regex (cdr pair) t)
                                       (unless (get-char-property (1- (point)) 'invisible)
                                         (let ((ov (make-overlay
                                                    (match-beginning 0)
                                                    (match-end 0))))
                                           (setq found t)
                                           (push ov overlays)
                                           (overlay-put
                                            ov 'window (selected-window))
                                           (overlay-put
                                            ov 'face 'avy-goto-char-timer-face)))))))
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
;; =================ace-pinyin==================
;;; ace-link
;; ==================ace-link===================
(use-package ace-link
  :commands ace-link)
;; ==================ace-link===================
(provide 'setup_avy)
