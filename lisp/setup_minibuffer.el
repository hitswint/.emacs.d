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
  ;; Enabled at commands.
  :load-path "site-lisp/cycle-mini/"
  :defer t
  :commands (cycle-mini-previous-completion cycle-mini-next-completion)
  :init
  (bind-key "C-p" 'cycle-mini-previous-completion minibuffer-local-completion-map)
  (bind-key "C-n" 'cycle-mini-next-completion minibuffer-local-completion-map)
  (bind-key "C-p" 'cycle-mini-previous-completion minibuffer-local-must-match-map)
  (bind-key "C-n" 'cycle-mini-next-completion minibuffer-local-must-match-map))
;; ====================cycle-mini====================
;; ========================minibuffer=========================
(provide 'setup_minibuffer)
