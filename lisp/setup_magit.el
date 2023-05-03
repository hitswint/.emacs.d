;;; magit
;; ====================magit=======================
(def-package! transient
  :commands transient-define-prefix)
(def-package! magit
  :diminish magit-auto-revert-mode
  :bind (("C-x M-g" . magit-status)
         ("C-x C-M-g" . magit-dispatch))
  :init
  (setq magit-auto-revert-mode nil)
  :config
  (dolist (hook '(magit-diff-mode-hook magit-status-mode-hook))
    (add-hook hook #'(lambda ()
                       (highlight-parentheses-mode -1)
                       (auto-mark-mode -1))))
  ;; magit-status中去除headers/staged，使用magit-show-refs(y)/magit-diff-staged(ds)/magit-diff-unstaged(du)
  (dolist (hook '(magit-insert-status-headers magit-insert-staged-changes))
    (remove-hook 'magit-status-sections-hook hook))
  (define-key magit-mode-map (kbd "<C-tab>") nil)
;;;; 使用git管理doc文件
  ;; =============使用git管理doc文件===============
  (defun swint-magit-diff-doc ()
    (interactive)
    (with-temp-file ".gitattributes"
      (insert (concat "*.doc diff=word" "\n" "*.docx diff=wordx")))
    (shell-command "git config diff.word.textconv catdoc")
    (shell-command "git config diff.wordx.textconv pandoc\\ --to=plain")
    (with-temp-file ".gitignore"
      (insert (concat ".~*" "\n"))))
  (define-key magit-status-mode-map (kbd "C-c d") 'swint-magit-diff-doc)
  ;; =============使用git管理doc文件===============
  )
;; ====================magit=======================
(provide 'setup_magit)
