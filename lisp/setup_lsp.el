;;; nox
;; =====================nox=====================
(def-package! posframe
  :load-path "site-lisp/posframe/"
  :after nox)
(def-package! nox
  :load-path "site-lisp/nox/"
  :bind ("M-g l" . nox)
  :init
  (global-set-key (kbd "C-x C-,") 'xref-find-definitions)
  (global-set-key (kbd "C-x C-.") 'xref-pop-marker-stack)
  (global-set-key (kbd "C-x C-/") 'xref-find-references)
  (global-set-key (kbd "C-x C-?") 'xref-find-apropos)
  :config
  (dolist (hook (list
                 'js-mode-hook
                 'rust-mode-hook
                 'python-mode-hook
                 'ruby-mode-hook
                 'java-mode-hook
                 'sh-mode-hook
                 'php-mode-hook
                 'c-mode-common-hook
                 'c-mode-hook
                 'csharp-mode-hook
                 'c++-mode-hook
                 'haskell-mode-hook
                 ))
    (add-hook hook '(lambda () (nox-ensure))))
  (setq nox-python-path (expand-file-name "~/.virtualenvs/py3/bin/python3"))
  ;; (setq nox-mspyls-search-paths [])
  (bind-key "M-g l" nil)
  (bind-key "M-g L" 'nox-shutdown)
  (bind-key "M-g l c" 'nox-reconnect)
  (bind-key "M-g l b" 'nox-events-buffer)
  (bind-key "M-g l e" 'nox-stderr-buffer)
  (bind-key "M-g l r" 'nox-rename)
  (bind-key "M-g l f" 'nox-format)
  (bind-key "M-g l o" 'nox-show-doc))
;; =====================nox=====================
(provide 'setup_lsp)
