;;; shell
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
    (setq explicit-bash-args '("-c" "export EMACS=; stty echo; bash"))
    (add-hook 'shell-mode-hook '(lambda ()
				  ;; 若virtualenvs开启，启动相应虚拟环境，并使用auto-complete补全命令。
                                  (if (and (boundp 'pyvenv-virtual-env) pyvenv-virtual-env)
                                      (process-send-string (get-process "shell")
                                                           (concat "source " pyvenv-virtual-env "bin/activate\n")))))))
  (define-key shell-mode-map (kbd "C-q") 'comint-send-eof))
;; =====================shell======================
;;; eshell
;; =====================eshell=====================
(use-package eshell
  ;; Enabled at commands.
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
                                (define-key eshell-mode-map (kbd "M-s") nil))))
;; =====================eshell=====================
;;; eshell-prompt-extras
;; ==============eshell-prompt-extras==============
(use-package eshell-prompt-extras
  ;; Enabled after features.
  :defer t
  :after eshell
  :config
  (with-eval-after-load "esh-opt"
    (autoload 'epe-theme-lambda "eshell-prompt-extras")
    (setq eshell-highlight-prompt nil
          eshell-prompt-function 'epe-theme-lambda)))
;; ==============eshell-prompt-extras==============
(provide 'setup_shell)
