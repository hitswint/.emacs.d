;; ============BACKUP-AUTOSAVE=============
;; 将backup和autosave文件都放在~/.emacs.d/.saves文件夹下。
(setq backup-by-copying t               ;don't clobber symlinks
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)                ;use versioned backups
(defconst temp-files-save-dir
  (format "%s%s/" (expand-file-name user-emacs-directory) ".saves"))
(setq backup-directory-alist
      `((".*" . ,temp-files-save-dir)))
(setq auto-save-file-name-transforms
      `((".*" ,temp-files-save-dir t)))
(setq tramp-backup-directory-alist backup-directory-alist)
(setq auto-save-list-file-prefix temp-files-save-dir)
;; Automatically purge backup files not accessed in a week.
(let ((day (* 60 60 24))
      (week (* 60 60 24 7))
      (current (float-time (current-time))))
  ;; 每周删除旧backup文件。
  (message "Deleting old backup files...")
  (dolist (file (directory-files temp-files-save-dir t))
    (when (and (backup-file-name-p file)
               (> (- current (float-time (fifth (file-attributes file))))
                  week))
      (message "%s" file)
      (delete-file file t)))
  ;; 每天清理trash中backup文件。
  (message "Cleaning trashcan...")
  (dolist (file (directory-files
                 (cond
                  (is-lin "~/.Trash")
                  (is-win "c:/TRASHCAN")) t))
    (when (and (backup-file-name-p file)
               (> (- current (float-time (fifth (file-attributes file))))
                  day))
      (message "%s" file)
      (delete-file file))))
;; Backup at each save.
(defun force-backup-of-buffer ()
  (let ((buffer-backed-up nil))
    (backup-buffer)))
(add-hook 'before-save-hook 'force-backup-of-buffer)
;; ============BACKUP-AUTOSAVE=============
(provide 'setup_backup_autosave)
