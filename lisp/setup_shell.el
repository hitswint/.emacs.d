;; =====================shell======================
(use-package shell
  ;; Enabled at commands.
  ;; Enabled automatically actually.
  :defer t
  :bind ("C-M-!" . shell)
  :config
  (cond
   (is-win
    ;; 在windows下默认启用cygwin bash。
    ;; 若使用cmdproxy.exe，则是windows自带命令行工具。
    (setq explicit-shell-file-name "bash.exe"))
   (is-lin
    (setq explicit-shell-file-name "bash")
    (setq explicit-bash-args '("-c" "export EMACS=; stty echo; bash")))))
;; =====================shell======================
;; =====================eshell=====================
(use-package eshell
  ;; Enabled at commands.
  :defer t
  :bind ("C-M-1" . eshell)
  :config
  (message "eshell")
  (add-hook 'eshell-mode-hook (lambda()
                                (outline-minor-mode 1)
                                (setq outline-regexp "^[^#$\n]* [#>]+ "
                                      scroll-margin 0
                                      eshell-scroll-to-bottom-on-output t
                                      eshell-scroll-show-maximum-output t)
                                (add-to-list 'eshell-output-filter-functions
                                             'eshell-postoutput-scroll-to-bottom)
                                (define-key eshell-mode-map (kbd "C-M-r") 'helm-eshell-history)
                                (define-key eshell-mode-map (kbd "M-s") nil))))
;; =====================eshell=====================
(provide 'setup_shell)
