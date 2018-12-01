;;; lsp-mode
;; =====================lsp-mode=====================
(def-package! lsp-mode
  :diminish lsp-mode
  :after (:any cc-mode pyvenv)
  :init
  (global-set-key (kbd "C-x C-,") 'xref-find-definitions)
  (global-set-key (kbd "C-x C-.") 'xref-pop-marker-stack)
  (global-set-key (kbd "C-x C-/") 'xref-find-references)
  (global-set-key (kbd "C-x C-?") 'xref-find-apropos)
  (add-hook 'c-mode-common-hook (lambda ()
                                  (local-set-key (kbd "M-g l") 'lsp-cquery-enable)))
  (add-hook 'python-mode-hook (lambda ()
                                (local-set-key (kbd "M-g l") 'lsp-python-enable)))
  :config
  (def-package! lsp-ui
    :commands lsp-ui-mode
    :init
    (add-hook 'lsp-mode-hook 'lsp-ui-mode)
    :config
    (setq lsp-ui-sideline-ignore-duplicate t)
    (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
    (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references))
  (def-package! lsp-imenu
    :commands lsp-enable-imenu
    :init
    (add-hook 'lsp-after-open-hook 'lsp-enable-imenu))
  (def-package! company-lsp
    :config
    (push 'company-lsp company-backends))
  (def-package! lsp-python)
  (def-package! cquery
    :config
    (setq cquery-executable "~/git-repo/Emacs/cquery/build/release/bin/cquery"))
  (def-package! helm-xref
    :config
    (setq xref-show-xrefs-function 'helm-xref-show-xrefs)))
;; =====================lsp-mode=====================
(provide 'setup_lsp)
