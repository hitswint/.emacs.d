;; ===========================gnuplot=================================
(use-package gnuplot-mode
  ;; Enabled in gnuplot-mode.
  :defer t
  :init
  (add-to-list 'auto-mode-alist '("\\.\\(gp\\|gnuplot\\)$" . gnuplot-mode))
  (when is-win
    (setq gnuplot-program "c:/Program Files (x86)/gnuplot/bin/gnuplot.exe"))
  :config
  (cond
   (is-lin
    ;; 打开gnuplot的输出图片。
    (defun open-gnuplot-output-file ()
      "Start a viewer without confirmation.
The viewer is started either on region or master file,
depending on the last command issued."
      (interactive)
      (let ((output-file (concat (file-name-base (buffer-name)) ".png")))
        (if (file-exists-p output-file)
            (async-shell-command-no-output-buffer-from-file output-file)
          (async-shell-command-no-output-buffer-from-file (concat (file-name-base (buffer-name)) ".eps")))))
    (add-hook 'gnuplot-mode-hook
              '(lambda ()
                 (define-key gnuplot-mode-map (kbd "C-c C-c") 'gnuplot-run-buffer)
                 (define-key gnuplot-mode-map (kbd "C-c C-v") 'open-gnuplot-output-file))))
   (is-win
    ;; use w32-brower to run gnuplot-buffer,because gnuplot-run-buffer is too slow
    (defun swint-gnuplot-run-buffer ()
      "Start a viewer without confirmation.
 The viewer is started either on region or master file,
 depending on the last command issued."
      (interactive)
      (w32-browser buffer-file-name))
    ;; open output image of gnuplot
    (defun swint-gnuplot-open-output-file ()
      "Start a viewer without confirmation.
  The viewer is started either on region or master file,
  depending on the last command issued."
      (interactive)
      (let ((output-file (concat (file-name-directory buffer-file-name) (file-name-base (buffer-name)) ".png")))
        (if (file-exists-p output-file)
            (w32-browser output-file)
          (w32-browser (concat (file-name-directory buffer-file-name) (file-name-base (buffer-name)) ".eps")))))
    (add-hook 'gnuplot-mode-hook
              '(lambda ()
                 (define-key gnuplot-mode-map (kbd "C-c C-c") 'swint-gnuplot-run-buffer)
                 (define-key gnuplot-mode-map (kbd "C-c C-v") 'swint-gnuplot-open-output-file))))))
;; ===========================gnuplot=================================
(provide 'setup_gnuplot)
