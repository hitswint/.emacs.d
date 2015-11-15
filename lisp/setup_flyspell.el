;; ================================flyspell==================================
(use-package flyspell
  :defer t
  :bind ("M-s f" . swint-toggle-flyspell-mode)
  :init
  (defun swint-toggle-flyspell-mode ()
    (interactive)
    (if flyspell-mode
        (flyspell-mode-off)
      (progn (flyspell-buffer)
             (flyspell-mode))))
  (setq ispell-program-name "aspell")
  (setq ispell-personal-dictionary (expand-file-name "~/.ispell"))
  :config
  (use-package ispell)
  (ispell-change-dictionary "american" t)
  (when is-win
    (add-to-list 'exec-path "C:/Program Files (x86)/Aspell/bin/"))
  (if (not ispell-alternate-dictionary)
      (setq ispell-alternate-dictionary (file-truename "~/.english-words"))))
(use-package helm-flyspell
  :defer t
  :bind ("M-s M-f" . helm-flyspell-correct)
  :config
  (define-key flyspell-mode-map (kbd "C-,") nil)
  (define-key flyspell-mode-map (kbd "C-.") nil))
;; ================================flyspell==================================
(provide 'setup_flyspell)
