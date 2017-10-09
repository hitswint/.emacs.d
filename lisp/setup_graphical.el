;;; gnuplot
;; ========================gnuplot===============================
(use-package gnuplot-mode
  ;; Enabled in modes.
  :defer t
  :mode ("\\.\\(gp\\|gnuplot\\)$" . gnuplot-mode)
  :init
  (when is-win
    (setq gnuplot-program "c:/Program Files (x86)/gnuplot/bin/gnuplot.exe"))
  :config
  (defun swint-gnuplot-run-buffer ()
    "Swint gnuplot run buffer."
    (interactive)
    (cond
     (is-lin (gnuplot-run-buffer))
     (is-win (w32-browser (buffer-file-name)))))
  (add-hook 'gnuplot-mode-hook
            '(lambda ()
               (define-key gnuplot-mode-map (kbd "C-c C-c") 'swint-gnuplot-run-buffer)
               (define-key gnuplot-mode-map (kbd "C-c C-v") 'swint-open-output-file))))
;; ========================gnuplot===============================
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
  (add-hook 'graphviz-dot-mode-hook
            '(lambda ()
               (define-key graphviz-dot-mode-map (kbd "C-c C-c") 'compile)
               (define-key graphviz-dot-mode-map (kbd "C-c C-v") 'swint-open-output-file))))
;; ====================graphviz-dot-mode=========================
(provide 'setup_graphical)
