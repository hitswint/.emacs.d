;;; paredit
;; ==================paredit=======================
(use-package paredit
  ;; Enabled in modes.
  :defer t
  :commands enable-paredit-mode
  :init
  (add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)
  :config
  (defun paredit-wrap-round-from-behind ()
    (interactive)
    (forward-sexp -1)
    (paredit-wrap-round))
  (defun paredit-wrap-square-from-behind ()
    (interactive)
    (forward-sexp -1)
    (paredit-wrap-square))
  (defun paredit-wrap-curly-from-behind ()
    (interactive)
    (forward-sexp -1)
    (paredit-wrap-curly))
  (defun paredit-wrap-angled-from-behind ()
    (interactive)
    (forward-sexp -1)
    (paredit-wrap-angled))
  (defun paredit-singlequote (&optional n)
    "Insert a pair of single-quotes."
    (interactive "P")
    (cond ((paredit-in-string-p)
           (if (eq (cdr (paredit-string-start+end-points))
                   (point))
               (forward-char)             ; We're on the closing quote.
             (insert ?\\ ?\' )))
          ((paredit-in-comment-p)
           (insert ?\' ))
          ((not (paredit-in-char-p))
           (paredit-insert-pair n ?\' ?\' 'paredit-forward-for-quote))))
  (defun paredit-meta-singlequote (&optional n)
    "Move to the end of the string."
    (interactive "P")
    (if (not (paredit-in-string-p))
        (paredit-singlequote (or n (and (not (paredit-region-active-p)) 1)))
      (goto-char (paredit-enclosing-string-end))))
  (defun swint-backward-kill-word ()
    (interactive)
    (if (and clean-aindent-mode
             (not (save-excursion (re-search-backward "[^[:space:]\n]" (point-at-bol) t))))
        (call-interactively 'clean-aindent--bsunindent)
      (call-interactively 'paredit-backward-kill-word)))
  (define-key paredit-mode-map (kbd "M-s") nil)
  (define-key paredit-mode-map (kbd "C-j") nil)
  (define-key paredit-mode-map (kbd "M-r") nil)
  (define-key paredit-mode-map (kbd "M-J") nil)
  (define-key paredit-mode-map (kbd "M-K") nil)
  (define-key paredit-mode-map (kbd "M-?") nil)
  (define-key paredit-mode-map (kbd "M-DEL") 'swint-backward-kill-word)
  (define-key paredit-mode-map (kbd "C-x (") 'paredit-backward-slurp-sexp)
  (define-key paredit-mode-map (kbd "C-x )") 'paredit-forward-slurp-sexp)
  (define-key paredit-mode-map (kbd "C-x {") 'paredit-backward-barf-sexp)
  (define-key paredit-mode-map (kbd "C-x }") 'paredit-forward-barf-sexp)
  (define-key paredit-mode-map (kbd "M-s (") 'paredit-wrap-round)
  (define-key paredit-mode-map (kbd "M-s )") 'paredit-wrap-round-from-behind)
  (define-key paredit-mode-map (kbd "M-s [") 'paredit-wrap-square)
  (define-key paredit-mode-map (kbd "M-s ]") 'paredit-wrap-square-from-behind)
  (define-key paredit-mode-map (kbd "M-s {") 'paredit-wrap-curly)
  (define-key paredit-mode-map (kbd "M-s }") 'paredit-wrap-curly-from-behind)
  (define-key paredit-mode-map (kbd "M-s <") 'paredit-wrap-angled)
  (define-key paredit-mode-map (kbd "M-s >") 'paredit-wrap-angled-from-behind)
  (define-key paredit-mode-map (kbd "M-s '") 'paredit-meta-singlequote)
  (define-key paredit-mode-map (kbd "M-s \"") 'paredit-meta-doublequote)
  (define-key paredit-mode-map (kbd "M-S") 'paredit-split-sexp)
  (define-key paredit-mode-map (kbd "M-R") 'paredit-raise-sexp)
  (define-key paredit-mode-map (kbd "M-A") 'paredit-join-sexps)
  (define-key paredit-mode-map (kbd "M-D") 'paredit-splice-sexp))
;; (autoload 'enable-paredit-mode "paredit" t)
;; ==================paredit=======================
;;; paredit-everywhere
;; ===============paredit-everything===============
;; 在其他mode中使用paredit。
(use-package paredit-everywhere
  ;; Enabled in modes.
  :defer t
  :commands paredit-everywhere-mode
  :init
  (dolist (hook '(LaTeX-mode-hook
                  arduino-mode-hook
                  c++-mode-hook
                  c-mode-hook
                  clojure-mode-hook
                  ess-mode-hook
                  octave-mode-hook
                  gnuplot-mode-hook
                  graphviz-dot-mode-hook
                  ledger-mode-hook
                  org-mode-hook
                  picolisp-mode-hook
                  python-mode-hook
                  lisp-interaction-mode-hook))
    (add-hook hook 'paredit-everywhere-mode))
  :config
  (define-key paredit-everywhere-mode-map (kbd "M-s") nil)
  (define-key paredit-everywhere-mode-map (kbd "M-r") nil)
  (define-key paredit-everywhere-mode-map (kbd "M-d") nil)
  (define-key paredit-everywhere-mode-map (kbd "M-J") nil)
  (define-key paredit-everywhere-mode-map (kbd "M-K") nil)
  (define-key paredit-everywhere-mode-map (kbd "M-DEL") 'swint-backward-kill-word)
  (define-key paredit-everywhere-mode-map (kbd "C-x (") 'paredit-backward-slurp-sexp)
  (define-key paredit-everywhere-mode-map (kbd "C-x )") 'paredit-forward-slurp-sexp)
  (define-key paredit-everywhere-mode-map (kbd "C-x {") 'paredit-backward-barf-sexp)
  (define-key paredit-everywhere-mode-map (kbd "C-x }") 'paredit-forward-barf-sexp)
  (define-key paredit-everywhere-mode-map (kbd "M-s (") 'paredit-wrap-round)
  (define-key paredit-everywhere-mode-map (kbd "M-s )") 'paredit-wrap-round-from-behind)
  (define-key paredit-everywhere-mode-map (kbd "M-s [") 'paredit-wrap-square)
  (define-key paredit-everywhere-mode-map (kbd "M-s ]") 'paredit-wrap-square-from-behind)
  (define-key paredit-everywhere-mode-map (kbd "M-s {") 'paredit-wrap-curly)
  (define-key paredit-everywhere-mode-map (kbd "M-s }") 'paredit-wrap-curly-from-behind)
  (define-key paredit-everywhere-mode-map (kbd "M-s <") 'paredit-wrap-angled)
  (define-key paredit-everywhere-mode-map (kbd "M-s >") 'paredit-wrap-angled-from-behind)
  (define-key paredit-everywhere-mode-map (kbd "M-s '") 'paredit-meta-singlequote)
  (define-key paredit-everywhere-mode-map (kbd "M-s \"") 'paredit-meta-doublequote)
  (define-key paredit-everywhere-mode-map (kbd "M-S") 'paredit-split-sexp)
  (define-key paredit-everywhere-mode-map (kbd "M-R") 'paredit-raise-sexp)
  (define-key paredit-everywhere-mode-map (kbd "M-A") 'paredit-join-sexps)
  (define-key paredit-everywhere-mode-map (kbd "M-D") 'paredit-splice-sexp))
;; ===============paredit-everything===============
(provide 'setup_paredit)
