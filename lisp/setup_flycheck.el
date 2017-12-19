;;; flycheck
;; ===============flycheck====================
(use-package flycheck
  :diminish flycheck-mode
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
  :commands helm-flycheck)
;; ==============helm-flycheck================
;;; flycheck-pos-tip
;; =============flycheck-pos-tip==============
(use-package flycheck-pos-tip
  :commands flycheck-pos-tip-mode
  :init
  ;; flycheck-pos-tip-mode会导致pos-tip弹出后快速隐藏。
  (add-hook 'flycheck-mode-hook '(lambda () (flycheck-pos-tip-mode 'toggle))))
;; =============flycheck-pos-tip==============
;;; flymake
;; ================flymake====================
(use-package flymake
  :diminish flymake-mode
  :commands flymake-mode)
;; ================flymake====================
(provide 'setup_flycheck)
