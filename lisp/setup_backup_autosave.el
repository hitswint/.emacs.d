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
      (delete-file file)))
  ;; 每周更新locate db文件，使用~/helm-locate-db.sh脚本更新~/.helm-locate.db文件。
  (message "Updating locate db file...")
  (let ((locat-db-file (expand-file-name "~/.helm-locate.db")))
    (when (or (not (file-exists-p locat-db-file))
              (> (- current (float-time (fifth (file-attributes locat-db-file))))
                 week))
      (message "%s" locat-db-file)
      (get-buffer-create "*Updating-locate-db-file*")
      (start-process-shell-command
       "Updating-locate-db-file" "*Updating-locate-db-file*"
       (concat "bash " (expand-file-name "~/helm-locate-db.sh")))))
  ;; 每周更新file system tree，win和lin都有各自的tree命令，格式不同。
  ;; win中shell-file-name为cmd，默认使用cmd执行命令，执行的是win自带的tree。
  ;; 若命令前面加bash，则使用cygwin的bash执行命令，执行的是cygwin的tree，输出文件存在编码问题。
  (message "Updating file system tree...")
  (let ((fs-tree-file (concat (expand-file-name "~/org/backups/fs-tree/")
                              (cond
                               (is-win "fs-tree-win.txt")
                               ((and is-lin is-X201)
                                "fs-tree-lin-X201.txt")
                               ((and is-lin is-T510)
                                "fs-tree-lin-T510.txt")))))
    (when (or (not (file-exists-p fs-tree-file))
              (> (- current (float-time (fifth (file-attributes fs-tree-file))))
                 week))
      (message "%s" fs-tree-file)
      (get-buffer-create "*Updating-file-system-tree*")
      (start-process-shell-command
       "Updating-file-system-tree" "*Updating-file-system-tree*"
       (concat "tree " (expand-file-name "~")
	       (cond
		(is-lin " -o ")
		(is-win " /f /a > ")) fs-tree-file)))))
;; Backup at each save.
(defun force-backup-of-buffer ()
  (let ((buffer-backed-up nil))
    (backup-buffer)))
(add-hook 'before-save-hook 'force-backup-of-buffer)
;; ============BACKUP-AUTOSAVE=============
(provide 'setup_backup_autosave)
