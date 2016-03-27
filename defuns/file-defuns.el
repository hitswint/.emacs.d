;; =============以root打开当前文件/文件夹==========
(defun sudo-edit (&optional arg)
  (interactive "p")
  (if (or arg (not buffer-file-name))
      (find-file (concat "/sudo:root@localhost:" (ido-read-file-name "File: ")))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))
(defun sudo-dired ()
  (interactive)
  (require 'tramp)
  (let ((dir (expand-file-name default-directory)))
    (if (string-match "^/sudo:" dir)
        (user-error "Already in sudo")
      (dired (concat "/sudo::" dir)))))
;; dired-mode：M(dired-do-chmod)改变权限；O(dired-do-chown)改变owner；G(dired-do-chgrp)改变group。
;; =============以root打开当前文件/文件夹==========
;; ===================改变文件编码=================
(fset 'swint-coding-system
      [?\C-x return ?r ?u ?t ?f ?- ?8 ?- ?u ?n ?i ?x ?\C-m ?y])
(global-set-key (kbd "M-s u") 'swint-coding-system)
;; ===================改变文件编码=================
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
