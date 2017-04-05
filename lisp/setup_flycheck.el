;;; flycheck
;; ===============flycheck====================
(use-package flycheck
  ;; Enabled at commands.
  :defer t
  :bind ("M-g c" . flycheck-mode)
  :config
  ;; Change the prefix.
  (define-key flycheck-mode-map flycheck-keymap-prefix nil)
  (setq flycheck-keymap-prefix (kbd "M-g M-c"))
  (define-key flycheck-mode-map flycheck-keymap-prefix flycheck-command-map)
  (smartrep-define-key flycheck-mode-map "M-g"
    '(("M-p" . flycheck-previous-error)
      ("M-n" . flycheck-next-error)
      ("M-c" . helm-flycheck))))
;; ===============flycheck====================
;;; helm-flycheck
;; ==============helm-flycheck================
(use-package helm-flycheck
  ;; Enabled at commands.
  :defer t
  :commands helm-flycheck)
;; ==============helm-flycheck================
(provide 'setup_flycheck)
