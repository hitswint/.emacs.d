;;; yasnippet
;; ==================yasnippet===================
(use-package yasnippet
  ;; Enabled at commands.
  :defer t
  :bind ("M-I" . swint-yas-insert-snippet)
  :config
  (defun swint-yas-insert-snippet ()
    (interactive)
    (if (featurep 'company)
        (company-abort))
    (unless (auto-complete '(ac-source-yasnippet))
      (yas-insert-snippet)))
  ;; (yas-initialize)
  (unless yas-global-mode
    (yas-global-mode 1)
    (yas-minor-mode 1))
  (define-key yas-minor-mode-map (kbd "<tab>") nil)
  (define-key yas-minor-mode-map (kbd "TAB") nil)
  ;; yas-snippet-dirs 默认包括自带snippets和用户自定义~/.emacs.d/snippets。
  ;; (setq yas-snippet-dirs
  ;;       '("~/.emacs.d/snippets" ;; personal snippets
  ;;         ;;         "/path/to/some/collection/"           ;; foo-mode and bar-mode snippet collection
  ;;         ;;         "/path/to/yasnippet/yasmate/snippets" ;; the yasmate collection
  ;;         ;;         "/path/to/yasnippet/snippets"         ;; the default collection
  ;;         ))
  ;; 使用ac的popup代替yas/choose-value自带的弹出菜单。
  (defun yas-popup-isearch-prompt (prompt choices &optional display-fn)
    (when (featurep 'popup)
      (popup-menu*
       (mapcar
        (lambda (choice)
          (popup-make-item
           (or (and display-fn (funcall display-fn choice))
               choice)
           :value choice))
        choices)
       :prompt prompt
       ;; start isearch mode immediately
       :isearch t)))
  (setq yas-prompt-functions '(yas-popup-isearch-prompt yas-no-prompt)))
;; ==================yasnippet===================
(provide 'setup_yasnippet)
