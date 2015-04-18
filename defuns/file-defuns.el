;; ==================以root打开当前文件/文件夹============
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
;; ==================以root打开当前文件/文件夹============
;; ===============改变文件编码==============
(fset 'swint-coding-system
      [?\C-x return ?r ?u ?t ?f ?- ?8 ?- ?u ?n ?i ?x ?\C-m ?y])
(global-set-key (kbd "C-c u") 'swint-coding-system)
;; ===============改变文件编码==============
