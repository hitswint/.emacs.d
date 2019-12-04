;;; flycheck
;; ===============flycheck====================
(def-package! flycheck
  :bind ("M-g C" . flycheck-mode)
  :config
  ;; Change the prefix.
  (define-key flycheck-mode-map flycheck-keymap-prefix nil)
  (setq flycheck-keymap-prefix (kbd "M-g c"))
  (define-key flycheck-mode-map flycheck-keymap-prefix flycheck-command-map)
  (smartrep-define-key flycheck-mode-map "M-g"
    '(("p" . flycheck-previous-error)
      ("n" . flycheck-next-error)
      ("M-c" . helm-flycheck))))
;; ===============flycheck====================
;;; helm-flycheck
;; ==============helm-flycheck================
(def-package! helm-flycheck
  :commands helm-flycheck)
;; ==============helm-flycheck================
;;; flycheck-pos-tip
;; =============flycheck-pos-tip==============
(def-package! flycheck-pos-tip
  :commands flycheck-pos-tip-mode
  :config
  ;; flycheck-pos-tip-mode会导致pos-tip弹出后快速隐藏。
  (add-hook 'flycheck-mode-hook '(lambda () (flycheck-pos-tip-mode 'toggle))))
;; =============flycheck-pos-tip==============
(provide 'setup_flycheck)
