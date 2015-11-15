;; ================================eshell==================================
(use-package esh-mode
  :defer t
  :bind ("C-M-1" . eshell)
  :config
  (add-hook 'eshell-mode-hook (lambda()
                                (outline-minor-mode 1)
                                (setq outline-regexp "^[^#$\n]* [#>]+ "
                                      scroll-margin 0
                                      eshell-scroll-to-bottom-on-output t
                                      eshell-scroll-show-maximum-output t)
                                (add-to-list 'eshell-output-filter-functions
                                             'eshell-postoutput-scroll-to-bottom)
                                (define-key eshell-mode-map (kbd "C-M-r") 'helm-eshell-history)
                                (define-key eshell-mode-map (kbd "C-c C-i") nil)
                                (define-key eshell-mode-map (kbd "C-c C-o") nil)
                                (define-key eshell-mode-map (kbd "M-s") nil)))
  ;; 让shell命令在windows下默认启用cygwin bash
  ;; cmdproxy.exe则是windows自带命令行工具
  (cond
   (is-win
    (setq explicit-shell-file-name "bash.exe"))
   (is-lin
    (setq explicit-shell-file-name "bash")
    (setq explicit-bash-args '("-c" "export EMACS=; stty echo; bash")))))
;; ================================eshell==================================
(provide 'setup_eshell)
