;;; gnuplot
;; ===========================gnuplot=================================
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
  (defun swint-gnuplot-open-output-file ()
    "Swint open gnuplot output file."
    (interactive)
    (let* ((output-png-file (concat (file-name-sans-extension (buffer-file-name)) ".png"))
           (output-eps-file (concat (file-name-sans-extension (buffer-file-name)) ".eps"))
           (output-file (cond ((file-exists-p output-png-file) output-png-file)
                              ((file-exists-p output-eps-file) output-eps-file))))
      (if output-file
          (cond
           (is-lin (async-shell-command-no-output-buffer-from-file output-file))
           (is-win (w32-browser output-file)))
        (message "Warning: No export file."))))
  (add-hook 'gnuplot-mode-hook
            '(lambda ()
               (define-key gnuplot-mode-map (kbd "C-c C-c") 'swint-gnuplot-run-buffer)
               (define-key gnuplot-mode-map (kbd "C-c C-v") 'swint-gnuplot-open-output-file))))
;; ===========================gnuplot=================================
(provide 'setup_gnuplot)
