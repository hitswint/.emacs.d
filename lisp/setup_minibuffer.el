;;; minibuffer
;; =================minibuffer==================
(defun switch-to-minibuffer ()
  "Switch to minibuffer window."
  (interactive)
  (if (active-minibuffer-window)
      (select-window (active-minibuffer-window))
    (error "Minibuffer is not active")))
(global-set-key (kbd "C-x C-<tab>") 'switch-to-minibuffer)
(define-key minibuffer-local-map (kbd "C-<tab>") 'nil)
;; =================minibuffer==================
;;; cycle-mini
;; ==================cycle-mini=================
(use-package cycle-mini
  ;; Enabled at commands.
  :load-path "site-lisp/cycle-mini/"
  :defer t
  :bind (:map minibuffer-local-completion-map
              ("C-p" . cycle-mini-previous-completion)
              ("C-n" . cycle-mini-next-completion)))
;; ==================cycle-mini=================
(provide 'setup_minibuffer)
