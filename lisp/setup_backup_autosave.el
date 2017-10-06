;;; BACKUP-AUTOSAVE
;; ============BACKUP-AUTOSAVE=============
;; 将backup和autosave文件都放在~/.emacs.d/.saves文件夹下。
(setq backup-by-copying t               ;don't clobber symlinks
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)                ;use versioned backups
(defconst temp-files-save-dir
  (format "%s%s/" (expand-file-name user-emacs-directory) ".saves"))
(setq backup-directory-alist `((".*" . ,temp-files-save-dir)))
(setq tramp-backup-directory-alist backup-directory-alist)
(setq auto-save-file-name-transforms `((".*" ,temp-files-save-dir t)))
(setq auto-save-list-file-prefix temp-files-save-dir)
(let ((day (* 60 60 24))
      (week (* 60 60 24 7))
      (month (* 60 60 24 30))
      (current (float-time (current-time))))
;;;; 每周清理backup/autosave文件。
  (message "Cleaning backup/autosave files...")
  (dolist (file (directory-files temp-files-save-dir t directory-files-no-dot-files-regexp))
    (when (> (- current (float-time (cl-sixth (file-attributes file)))) week)
      (message "%s" file)
      (delete-file file t)))
;;;; 每天清理trashcan。
  (when is-lin
    (message "Cleaning trashcan...")
    (dolist (file (directory-files trash-directory t directory-files-no-dot-files-regexp))
      (when (and (or (backup-file-name-p file) (auto-save-file-name-p file))
                 (> (- current (float-time (cl-sixth (file-attributes file)))) day))
        (message "%s" file)
        (if (file-directory-p file)
            (delete-directory file t)
          (delete-file file)))))
  (let* ((PC-dir (cond
                  (is-lin (replace-regexp-in-string
                           "\n" ""
                           (replace-regexp-in-string
                            "\\(/\\|\\.\\)" "_"
                            (shell-command-to-string
                             "$(cat /etc/machine-id > /tmp/machine.xid); mkpasswd $(</tmp/machine.xid) -s PC"))))
                  (is-win "PC_win")))
         (PC-path (expand-file-name PC-dir "~/org/backups/log/"))
         (PC-tree-file (expand-file-name "fs-tree.txt" PC-path))
         (PC-apt-history-file (expand-file-name "history.log" PC-path)))
    (unless (file-exists-p PC-path)
      (dired-create-directory PC-path)
      (message "Created %s" PC-path))
;;;; 每月备份apt history文件。
    (when (and is-lin
               (or (not (file-exists-p PC-apt-history-file))
                   (> (- current (float-time (cl-sixth (file-attributes PC-apt-history-file))))
                      month)))
      (message "Updating apt history: %s" PC-apt-history-file)
      (cl-loop for it in (directory-files "/var/log/apt/" t "history\\.log")
               do (copy-file it PC-path t)))
;;;; 每月更新file system tree文件。
    (when (or (not (file-exists-p PC-tree-file))
              (> (- current (float-time (cl-sixth (file-attributes PC-tree-file))))
                 month))
      (message "Updating file system tree: %s" PC-tree-file)
      (get-buffer-create "*Updating-file-system-tree*")
      (start-process-shell-command
       "Updating-file-system-tree" "*Updating-file-system-tree*"
       (concat "tree " (expand-file-name "~")
               (cond
                (is-lin " -o ")
                (is-win " /f /a > "))
               PC-tree-file)))))
;;;; 每次保存备份文件。
(defun force-backup-of-buffer ()
  (let ((buffer-backed-up nil))
    (unless (eq major-mode 'ein:notebook-multilang-mode)
      (backup-buffer))))
(add-hook 'before-save-hook 'force-backup-of-buffer)
;; ============BACKUP-AUTOSAVE=============
(provide 'setup_backup_autosave)
