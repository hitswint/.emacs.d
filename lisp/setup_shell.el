;;; shell
;; =====================shell======================
(def-package! shell
  :bind (("C-M-!" . shell)
         ("C-x C-M-1" . term))
  :config
  (add-hook 'shell-mode-hook '(lambda ()
                                (if (not (file-exists-p "~/.zsh_history"))
                                    (setq comint-input-ring-file-name "~/.bash_history")
                                  (setq comint-input-ring-file-name "~/.zsh_history")
                                  (setq comint-input-ring-separator "\n: \\([0-9]+\\):\\([0-9]+\\);"))
                                (comint-read-input-ring t)
                                (add-hook 'shell-mode-hook 'kill-shell-buffer-after-exit t)
                                ;; 若virtualenvs开启，启动相应虚拟环境，并使用auto-complete补全命令。
                                (if (bound-and-true-p pyvenv-virtual-env)
                                    (process-send-string (get-process "shell")
                                                         (concat "source " pyvenv-virtual-env "bin/activate\n")))))
  (define-key shell-mode-map (kbd "C-q") 'comint-send-eof))
;; =====================shell======================
;;; eshell
;; =====================eshell=====================
(def-package! eshell
  :bind ("C-M-1" . eshell)
  :config
  (add-hook 'eshell-mode-hook '(lambda()
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
(def-package! eshell-prompt-extras
  :after eshell
  :config
  (with-eval-after-load "esh-opt"
    (autoload 'epe-theme-lambda "eshell-prompt-extras")
    (setq eshell-highlight-prompt nil
          eshell-prompt-function 'epe-theme-lambda)))
;; ==============eshell-prompt-extras==============
(provide 'setup_shell)
