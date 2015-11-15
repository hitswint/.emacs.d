;; ========================minibuffer=========================
(defun switch-to-minibuffer ()
  "Switch to minibuffer window."
  (interactive)
  (if (active-minibuffer-window)
      (select-window (active-minibuffer-window))
    (error "Minibuffer is not active")))
(global-set-key (kbd "C-c C-<tab>") 'switch-to-minibuffer)
(define-key minibuffer-local-map (kbd "C-<tab>") 'nil)
;; ====================cycle-mini====================
(use-package cycle-mini
  :load-path "site-lisp/cycle-mini/")
;; ====================cycle-mini====================
;; ========================minibuffer=========================
(provide 'setup_minibuffer)
