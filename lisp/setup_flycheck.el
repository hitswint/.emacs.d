;;; flycheck
;; ===============flycheck====================
(def-package! flycheck
  :bind ("M-g C" . swint-toggle-flycheck-mode)
  :config
  ;; https://github.com/emacs-grammarly/flycheck-grammarly 支持grammarly，但连网速度慢
  ;; 默认支持proselint，但在latex-mode中不开启
  (define-key flycheck-mode-map flycheck-keymap-prefix nil)
  (setq flycheck-keymap-prefix (kbd "M-g c"))
  (define-key flycheck-mode-map flycheck-keymap-prefix flycheck-command-map)
  (smartrep-define-key flycheck-mode-map "M-g"
    '(("p" . flycheck-previous-error)
      ("n" . flycheck-next-error)
      ("M-c" . helm-flycheck)))
  (defun swint-toggle-flycheck-mode ()
    (interactive)
    (dolist (buf (cl-remove-if-not (lambda (x)
                                     (equal (buffer-mode x) major-mode))
                                   (buffer-list)))
      (with-current-buffer buf
        (call-interactively 'flycheck-mode)))
    (if flycheck-mode
        (add-hook (intern (concat (symbol-name major-mode) "-hook")) 'flycheck-mode)
      (remove-hook (intern (concat (symbol-name major-mode) "-hook")) 'flycheck-mode))))
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
