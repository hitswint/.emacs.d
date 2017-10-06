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
