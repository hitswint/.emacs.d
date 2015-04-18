;; ================================flyspell==================================
(setq ispell-program-name "aspell")
(setq ispell-personal-dictionary "~/.ispell")
(ispell-change-dictionary "american" t)
(require 'ispell)
(require 'flyspell)
(define-key flyspell-mode-map (kbd "C-,") nil)
(define-key flyspell-mode-map (kbd "C-.") nil)
(global-set-key (kbd "M-s M-f") 'ispell-word)
(global-set-key (kbd "M-s f") '(lambda () (interactive) (if flyspell-mode
                                                            (flyspell-mode-off)
                                                          (progn (flyspell-buffer)
                                                                 (flyspell-mode)))))
;; ================================flyspell==================================
(provide 'setup_flyspell)
