;;; 改变文件编码
;; ===================改变文件编码=================
(global-set-key (kbd "C-x RET RET") '(lambda ()
                                       (interactive)
                                       (revert-buffer-with-coding-system 'utf-8)))
;; ===================改变文件编码=================
;;; dos2unix
;; =====================dos2unix===================
(defun save-buffer-with-dos2unix ()
  (interactive)
  (let ((file-to-convert (file-name-nondirectory (buffer-file-name (current-buffer)))))
    (if (buffer-modified-p)
        (progn (save-buffer)
               (shell-command (concat "dos2unix " file-to-convert))
               (revert-buffer nil t))
      (save-buffer))))
(when is-win
  (define-key emacs-lisp-mode-map (kbd "C-x C-s") 'save-buffer-with-dos2unix))
(global-set-key (kbd "C-x M-s") 'save-buffer-with-dos2unix)
;; =====================dos2unix===================
;;; swint-open-output-file
;; =============swint-open-output-file=============
(defun swint-open-output-file ()
  "Swint open gnuplot output file."
  (interactive)
  (let* ((output-pdf-file (concat (file-name-sans-extension (buffer-file-name)) ".pdf"))
         (output-png-file (concat (file-name-sans-extension (buffer-file-name)) ".png"))
         (output-eps-file (concat (file-name-sans-extension (buffer-file-name)) ".eps"))
         (output-file (cond ((file-exists-p output-pdf-file) output-pdf-file)
                            ((file-exists-p output-png-file) output-png-file)
                            ((file-exists-p output-eps-file) output-eps-file))))
    (if output-file
        (cond
         (is-lin (dired-async-shell-command output-file))
         (is-win (w32-browser output-file)))
      (message "Warning: No export file."))))
;; =============swint-open-output-file=============
