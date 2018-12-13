;;; lsp-mode
;; =====================lsp-mode=====================
(def-package! lsp-mode
  :diminish lsp-mode
  :commands lsp
  :init
  (global-set-key (kbd "C-x C-,") 'xref-find-definitions)
  (global-set-key (kbd "C-x C-.") 'xref-pop-marker-stack)
  (global-set-key (kbd "C-x C-/") 'xref-find-references)
  (global-set-key (kbd "C-x C-?") 'xref-find-apropos)
  (add-hook 'c-mode-common-hook (lambda ()
                                  (local-set-key (kbd "M-g l") 'lsp)))
  (add-hook 'python-mode-hook (lambda ()
                                (local-set-key (kbd "M-g l") 'lsp)))
  :config
  ;; 自带客户端，默认支持的服务器为：C++(clangd)/Python(pyls)。
  (require 'lsp-clients)
  (setq lsp-auto-guess-root t)
  (def-package! lsp-ui
    :commands lsp-ui-mode
    :init
    (add-hook 'lsp-mode-hook 'lsp-ui-mode)
    :config
    (setq lsp-ui-sideline-ignore-duplicate t)
    (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
    (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references))
  (def-package! helm-xref
    :config
    (setq xref-show-xrefs-function 'helm-xref-show-xrefs))
  (def-package! company-lsp)
  ;; C++使用cquery服务器。
  (def-package! cquery
    :config
    (setq cquery-executable "~/git-repo/Emacs/cquery/build/release/bin/cquery")))
;; =====================lsp-mode=====================
(provide 'setup_lsp)
