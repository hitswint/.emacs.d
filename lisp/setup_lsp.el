;;; nox
;; =====================nox=====================
(def-package! posframe
  :load-path "site-lisp/posframe/"
  :after nox)
(def-package! nox
  :load-path "site-lisp/nox/"
  :bind (("M-g l" . nox)
         ("M-g L" . nox-shutdown))
  :init
  (bind-key "C-x C-," 'xref-find-definitions)
  (bind-key "C-x C-." 'xref-pop-marker-stack)
  (bind-key "C-x C-/" 'xref-find-references)
  (bind-key "C-x C-?" 'xref-find-apropos)
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
                 'haskell-mode-hook))
    (add-hook hook #'(lambda () (nox-ensure)
                       ;; 需先打开相应文件，下列快捷键才会生效。
                       (local-set-key (kbd "C-c c") 'nox-reconnect)
                       (local-set-key (kbd "C-c b e") 'nox-events-buffer)
                       (local-set-key (kbd "C-c b s") 'nox-stderr-buffer)
                       (local-set-key (kbd "C-c r") 'nox-rename)
                       (local-set-key (kbd "C-c f") 'nox-format)
                       (local-set-key (kbd "C-c d") 'nox-show-doc))))
  (setq nox-python-path (expand-file-name "~/.virtualenvs/py3/bin/python3")))
;; =====================nox=====================
(provide 'setup_lsp)
