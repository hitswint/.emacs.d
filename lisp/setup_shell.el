;;; shell
;; =====================shell======================
(use-package shell
  :bind ("M-o M-T" . shell)
  ;; :init
  ;; 使用zsh -ic无法加载alias，但bash可以，新建.bash_aliases
  ;; (setq shell-file-name "bash")
  ;; (setq shell-command-switch "-ic")
  :init
  (add-hook 'shell-mode-hook #'(lambda ()
                                 (if (not (file-exists-p "~/.zsh_history"))
                                     (setq comint-input-ring-file-name "~/.bash_history")
                                   (setq comint-input-ring-file-name "~/.zsh_history")
                                   (setq comint-input-ring-separator "\n: \\([0-9]+\\):\\([0-9]+\\);"))
                                 (comint-read-input-ring t)
                                 ;; 若virtualenvs开启，启动相应虚拟环境，并使用auto-complete补全命令
                                 (if (bound-and-true-p pyvenv-virtual-env)
                                     (process-send-string (get-process "shell")
                                                          (concat "source " pyvenv-virtual-env "bin/activate\n")))))
  (add-hook 'shell-mode-hook 'kill-shell-buffer-after-exit t))
;; =====================shell======================
;;; eshell
;; =====================eshell=====================
(use-package eshell
  :bind ("M-o M-t" . eshell)
  :init
  (add-hook 'eshell-mode-hook #'(lambda ()
                                  (define-key eshell-mode-map (kbd "M-s") nil)
                                  (add-to-list 'eshell-output-filter-functions
                                               'eshell-postoutput-scroll-to-bottom)))
  :config
  (setq eshell-scroll-to-bottom-on-output t
        eshell-scroll-show-maximum-output t))
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
;;; sh-script
;; ==============eshell-prompt-extras==============
(use-package sh-script
  :mode ("\\.sh\\'" . sh-mode)
  :init
  (add-hook 'sh-mode-hook (lambda () (when (getenv "WM_PROJECT")
                                       (require 'company)
                                       (make-local-variable 'company-backends)
                                       (add-to-list 'company-backends 'company-foamDictionary)
                                       (set (make-local-variable 'company-minimum-prefix-length) 0))))
  :config
  (defun company-foamDictionary (command &optional arg &rest ignored)
    (interactive (list 'interactive))
    (cl-case command
      (prefix
       (save-excursion
         (beginning-of-line)
         (when (looking-at "foamDictionary\\s-+\\([^[:space:]]+\\)\\s-+-entry\\s-+\\([^[:space:]]*\\)") "")))
      (candidates
       (let* ((entry (or (string-trim (buffer-substring-no-properties (match-beginning 2) (match-end 2))) ""))
              (dict-file (buffer-substring-no-properties (match-beginning 1) (match-end 1)))
              (setting-value (save-excursion (beginning-of-line) (looking-at "foamDictionary.*-set\\s-+$")))
              (params (cond ((string-empty-p entry)
                             "-keywords")
                            ((string-suffix-p "/" entry)
                             (concat "-entry " (substring entry 0 -1) " -keywords"))
                            (setting-value
                             (concat "-entry " entry " -value"))
                            (t nil)))
              (cmd (format "foamDictionary %s %s 2>/dev/null" dict-file (or params (concat "-entry " entry))))
              (cmd-output (shell-command-to-string cmd)))
         (if params
             (if setting-value
                 (list (concat "\"" (string-trim cmd-output) "\""))
               (split-string cmd-output "\n" t))
           (prog1 '()
             (message "%s" cmd-output)))))
      (meta
       (format "OpenFOAM key under '%s'" (or arg ""))))))
;; ==============eshell-prompt-extras==============
(provide 'setup_shell)
