;;; graphviz-dot-mode
;; ====================graphviz-dot-mode=========================
(use-package graphviz-dot-mode
  ;; Enabled in modes.
  :defer t
  :mode ("\\.dot\\'" . graphviz-dot-mode)
  :config
  (when is-win
    ;; 注意：路径/bin 后面一定要有那个分号;，不用将路径加到环境变量中。
    (setenv "PATH" (concat "c:/Program Files (x86)/Graphviz2.36/bin;" (getenv "PATH"))))
  (setq graphviz-dot-view-command "feh")
  (defun swint-graphviz-open-output-file ()
    "Start a viewer without confirmation.
The viewer is started either on region or master file,
depending on the last command issued."
    (interactive)
    (let ((output-file (concat (file-name-sans-extension (buffer-file-name)) ".png")))
      (if (file-exists-p output-file)
          (cond
           (is-lin (dired-async-shell-command output-file))
           (is-win (w32-browser output-file)))
        (message "Warning: No export file."))))
  (add-hook 'graphviz-dot-mode-hook
            '(lambda ()
               (define-key graphviz-dot-mode-map (kbd "C-c C-c") 'compile)
               (define-key graphviz-dot-mode-map (kbd "C-c C-v") 'swint-graphviz-open-output-file))))
;; ====================graphviz-dot-mode=========================
(provide 'setup_graphviz)
