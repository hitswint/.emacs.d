;; ==================paredit=========================
(use-package paredit
  ;; Enabled in emacs-lisp-mode.
  :defer t
  :commands enable-paredit-mode
  :init
  (add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)
  :config
  (defun paredit-wrap-round-from-behind ()
    (interactive)
    (forward-sexp -1)
    (paredit-wrap-round)
    (insert " ")
    (forward-char -1))
  (defun paredit-wrap-square-from-behind ()
    (interactive)
    (forward-sexp -1)
    (paredit-wrap-square))
  (defun paredit-wrap-curly-from-behind ()
    (interactive)
    (forward-sexp -1)
    (paredit-wrap-curly))
  (define-key paredit-mode-map (kbd "M-s") nil)
  (define-key paredit-mode-map (kbd "C-j") nil)
  (define-key paredit-mode-map (kbd "M-r") nil)
  (define-key paredit-mode-map (kbd "M-J") nil)
  (define-key paredit-mode-map (kbd "M-K") nil)
  (define-key paredit-mode-map (kbd "M-?") nil)
  (define-key paredit-mode-map (kbd "C-M-<left>") nil)
  (define-key paredit-mode-map (kbd "C-M-<right>") nil)
  (define-key paredit-mode-map (kbd "C-(") 'paredit-backward-slurp-sexp)
  (define-key paredit-mode-map (kbd "C-)") 'paredit-forward-slurp-sexp)
  (define-key paredit-mode-map (kbd "C-{") 'paredit-backward-barf-sexp)
  (define-key paredit-mode-map (kbd "C-}") 'paredit-forward-barf-sexp)
  (define-key paredit-mode-map (kbd "M-(") 'paredit-wrap-round)
  (define-key paredit-mode-map (kbd "M-)") 'paredit-wrap-round-from-behind)
  (define-key paredit-mode-map (kbd "M-[") 'paredit-wrap-square)
  (define-key paredit-mode-map (kbd "M-]") 'paredit-wrap-square-from-behind)
  (define-key paredit-mode-map (kbd "M-{") 'paredit-wrap-curly)
  (define-key paredit-mode-map (kbd "M-}") 'paredit-wrap-curly-from-behind)
  (define-key paredit-mode-map (kbd "M-S") 'paredit-split-sexp)
  (define-key paredit-mode-map (kbd "M-R") 'paredit-raise-sexp)
  (define-key paredit-mode-map (kbd "M-A") 'paredit-join-sexps)
  (define-key paredit-mode-map (kbd "M-D") 'paredit-splice-sexp))
;; (autoload 'enable-paredit-mode "paredit" t)
;; M-( M-) M-[ M-] M-{ M-} M-" M-S M-R M-A M-D C-) C-} C-( C-{ C-M-d C-M-u C-M-p C-M-n
;; 在其他mode中使用paredit。
(use-package paredit-everywhere
  ;; Enabled in modes.
  :defer t
  :commands paredit-everywhere-mode
  :init
  (dolist (hook '(LaTeX-mode-hook
                  org-mode-hook
                  octave-mode-hook
                  gnuplot-mode-hook
                  c-mode-hook
                  graphviz-dot-mode-hook
                  lisp-interaction-mode-hook))
    (add-hook hook 'paredit-everywhere-mode))
  :config
  (define-key paredit-everywhere-mode-map (kbd "M-s") nil)
  (define-key paredit-everywhere-mode-map (kbd "M-r") nil)
  (define-key paredit-everywhere-mode-map (kbd "M-d") nil)
  (define-key paredit-everywhere-mode-map (kbd "M-J") nil)
  (define-key paredit-everywhere-mode-map (kbd "M-K") nil)
  (define-key paredit-everywhere-mode-map (kbd "C-(") 'paredit-backward-slurp-sexp)
  (define-key paredit-everywhere-mode-map (kbd "C-)") 'paredit-forward-slurp-sexp)
  (define-key paredit-everywhere-mode-map (kbd "C-{") 'paredit-backward-barf-sexp)
  (define-key paredit-everywhere-mode-map (kbd "C-}") 'paredit-forward-barf-sexp)
  (define-key paredit-everywhere-mode-map (kbd "M-(") 'paredit-wrap-round)
  (define-key paredit-everywhere-mode-map (kbd "M-)") 'paredit-wrap-round-from-behind)
  (define-key paredit-everywhere-mode-map (kbd "M-[") 'paredit-wrap-square)
  (define-key paredit-everywhere-mode-map (kbd "M-]") 'paredit-wrap-square-from-behind)
  (define-key paredit-everywhere-mode-map (kbd "M-{") 'paredit-wrap-curly)
  (define-key paredit-everywhere-mode-map (kbd "M-}") 'paredit-wrap-curly-from-behind)
  (define-key paredit-everywhere-mode-map (kbd "M-S") 'paredit-split-sexp)
  (define-key paredit-everywhere-mode-map (kbd "M-R") 'paredit-raise-sexp)
  (define-key paredit-everywhere-mode-map (kbd "M-A") 'paredit-join-sexps)
  (define-key paredit-everywhere-mode-map (kbd "M-D") 'paredit-splice-sexp))
;; M-( M-) M-[ M-] M-{ M-} M-" M-S M-R M-A M-D C-) C-} C-( C-{ M-DEL M-d
;; ==================paredit=========================
(provide 'setup_paredit)
