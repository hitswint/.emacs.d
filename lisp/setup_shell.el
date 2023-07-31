;;; shell
;; =====================shell======================
(use-package shell
  :bind (("M-o s" . shell)
         ("M-o S" . term))
  ;; :init
  ;; 使用zsh -ic无法加载alias，但bash可以，新建.bash_aliases
  ;; (setq shell-file-name "bash")
  ;; (setq shell-command-switch "-ic")
  :config
  (add-hook 'shell-mode-hook #'(lambda ()
                                 (if (not (file-exists-p "~/.zsh_history"))
                                     (setq comint-input-ring-file-name "~/.bash_history")
                                   (setq comint-input-ring-file-name "~/.zsh_history")
                                   (setq comint-input-ring-separator "\n: \\([0-9]+\\):\\([0-9]+\\);"))
                                 (comint-read-input-ring t)
                                 (add-hook 'shell-mode-hook 'kill-shell-buffer-after-exit t)
                                 ;; 若virtualenvs开启，启动相应虚拟环境，并使用auto-complete补全命令
                                 (if (bound-and-true-p pyvenv-virtual-env)
                                     (process-send-string (get-process "shell")
                                                          (concat "source " pyvenv-virtual-env "bin/activate\n")))))
  (define-key shell-mode-map (kbd "C-q") 'comint-send-eof))
;; =====================shell======================
;;; eshell
;; =====================eshell=====================
(use-package eshell
  :bind ("M-o M-s" . eshell)
  :config
  (add-hook 'eshell-mode-hook #'(lambda()
                                  (setq scroll-margin 0
                                        eshell-scroll-to-bottom-on-output t
                                        eshell-scroll-show-maximum-output t)
                                  (add-to-list 'eshell-output-filter-functions
                                               'eshell-postoutput-scroll-to-bottom)
                                  (define-key eshell-mode-map (kbd "M-s") nil))))
;; =====================eshell=====================
;;; eshell-prompt-extras
;; ==============eshell-prompt-extras==============
(use-package eshell-prompt-extras
  :after eshell
  :config
  (with-eval-after-load "esh-opt"
    (autoload 'epe-theme-lambda "eshell-prompt-extras")
    (setq eshell-highlight-prompt nil
          eshell-prompt-function 'epe-theme-lambda)))
;; ==============eshell-prompt-extras==============
(provide 'setup_shell)
