;; ================================flyspell==================================
(setq ispell-program-name "aspell")
(cond
 (is-lin
  (setq ispell-personal-dictionary "~/.ispell")
  (ispell-change-dictionary "american" t))
 (is-win
  (add-to-list 'exec-path "C:/Program Files (x86)/Aspell/bin/")
  (setq ispell-personal-dictionary "c:/Users/swint/.ispell")))
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
