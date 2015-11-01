;; ================================flyspell==================================
(setq ispell-program-name "aspell")
(setq ispell-personal-dictionary (expand-file-name "~/.ispell"))
(ispell-change-dictionary "american" t)
(when is-win
  (add-to-list 'exec-path "C:/Program Files (x86)/Aspell/bin/"))
(require 'ispell)
(require 'flyspell)
(require 'helm-flyspell)
(define-key flyspell-mode-map (kbd "C-,") nil)
(define-key flyspell-mode-map (kbd "C-.") nil)
(global-set-key (kbd "M-s M-f") 'helm-flyspell-correct)
(global-set-key (kbd "M-s f") '(lambda () (interactive) (if flyspell-mode
                                                            (flyspell-mode-off)
                                                          (progn (flyspell-buffer)
                                                                 (flyspell-mode)))))
;; ================================flyspell==================================
(provide 'setup_flyspell)
