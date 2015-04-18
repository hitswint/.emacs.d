;; ===========================gnuplot=================================
;; (add-to-list 'load-path "~/.emacs.d/gnuplot")
(require 'gnuplot-mode)
;; specify the gnuplot executable (if other than /usr/bin/gnuplot)
;; (setq gnuplot-program "c:/Octave/Octave3.6.4_gcc4.6.2/gnuplot/bin/gnuplot.exe")
;; automatically open files ending with .gp or .gnuplot in gnuplot mode
(setq auto-mode-alist
      (append '(("\\.\\(gp\\|gnuplot\\)$" . gnuplot-mode)) auto-mode-alist))
(add-hook 'gnuplot-mode-hook
          '(lambda ()
             (define-key gnuplot-mode-map (kbd "C-c C-c") 'gnuplot-run-buffer)
             (define-key gnuplot-mode-map (kbd "C-c C-v") 'open-gnuplot-output-file)))
;; 下面的函数获得不包括扩展名的文件名
(defun file-name-base (&optional filename)
  "Return the base name of the FILENAME: no directory, no extension.
FILENAME defaults to `buffer-file-name'."
  (file-name-sans-extension
   (file-name-nondirectory (or filename (buffer-file-name)))))
;; 打开gnuplot的输出图片
(defun open-gnuplot-output-file ()
  "Start a viewer without confirmation.
The viewer is started either on region or master file,
depending on the last command issued."
  (interactive)
  (let ((output-file (concat (file-name-base (buffer-name)) ".png")))
    (if (file-exists-p output-file)
        (async-shell-command-no-output-buffer-from-file output-file)
      (async-shell-command-no-output-buffer-from-file (concat (file-name-base (buffer-name)) ".eps")))))
;; ===========================gnuplot=================================
(provide 'setup_gnuplot)
