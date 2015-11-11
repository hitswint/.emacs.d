;; ====================graphviz-dot-mode=========================
(when is-win
  ;; 注意：路径/bin 后面一定要有那个分号;，不用将路径加到环境变量中
  (setenv "PATH" (concat "c:/Program Files (x86)/Graphviz2.36/bin;" (getenv "PATH"))))
(load "graphviz-dot-mode.el" nil t t)
(add-hook 'find-file-hook (lambda()
                            (if (string= "dot" (file-name-extension
                                                buffer-file-name))
                                (progn
                                  (message "Enabling Setings for dot-mode")
                                  (setq fill-column 1000)
                                  ;; (base-auto-pair)
                                  (linum-mode)))))
(setq graphviz-dot-view-command "feh")
(add-hook 'graphviz-dot-mode-hook
          '(lambda ()
             (define-key graphviz-dot-mode-map (kbd "C-c C-c") 'compile)
             (define-key graphviz-dot-mode-map (kbd "C-c C-v") 'swint-graphviz-output)))
(defun swint-graphviz-output ()
  "Start a viewer without confirmation.
The viewer is started either on region or master file,
depending on the last command issued."
  (interactive)
  (let ((output-file (cond
                      (is-lin (concat (file-name-base (buffer-name)) ".png"))
                      (is-win (concat (file-name-directory buffer-file-name)
                                      (file-name-base (buffer-name)) ".png")))))
    (if (file-exists-p output-file)
        (cond
         (is-lin (async-shell-command-no-output-buffer-from-file output-file))
         (is-win (w32-browser output-file)))
      (message "Warning: No export png."))))
;; ====================graphviz-dot-mode=========================
(provide 'setup_graphviz)
