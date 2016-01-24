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
      (call-interactively 'avy-goto-char-timer))
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
               (funcall ace-pinyin--original-ace query-char)))))))
;; =======================avy===========================
(provide 'setup_avy)
