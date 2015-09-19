;; dired-sync-annotated.el --- Sync annotated files status.
(require 'dired-aux)
(require 'wdired)

(defvar files-to-be-deleted nil
  "Files to be deleted.")

(defun dired-copy-file (from to ok-flag)
  "Sync dired annotated status when copy files."
  (dired-handle-overwrite to)
  (dired-copy-file-recursive from to ok-flag dired-copy-preserve-time t
                             dired-recursive-copies)
  (let ((annotated-file-list (if (dired-k--parse-annotated-status)
                                 (hash-table-keys (dired-k--parse-annotated-status))))
        (annotation-storage-files
         (directory-files "~/org/annotated/" t
                          (concat "annotated-\\["
                                  (replace-regexp-in-string
                                   "/" "_" (substring-no-properties (abbreviate-file-name from) 1))))))
    (cond
     ((member (file-name-nondirectory from) annotated-file-list)
      (let ((annotation-refile-from (concat "~/org/annotated/annotated-["
                                            (replace-regexp-in-string
                                             "/" "_" (substring-no-properties (abbreviate-file-name (file-name-directory from)) 1 -1))
                                            "].org"))
            (annotation-refile-to (concat "~/org/annotated/annotated-["
                                          (replace-regexp-in-string
                                           "/" "_" (substring-no-properties (abbreviate-file-name (file-name-directory to)) 1 -1))
                                          "].org")))
        (with-current-buffer (find-file annotation-refile-from)
          (widen)
          (goto-char (point-min))
          (search-forward (org-make-link-string (concat "file:" (file-name-nondirectory from))) nil t)
          (if (org-at-heading-p)
              (my/refile annotation-refile-to (org-get-heading)))
          (kill-buffer))
        (with-current-buffer (find-file annotation-refile-to)
          (widen)
          (goto-char (point-min))
          (replace-string
           (org-make-link-string (concat "file:" (file-name-nondirectory from)))
           (org-make-link-string (concat "file:" (file-name-nondirectory to))))
          (if is-win
              (save-buffer-with-dos2unix)
            (save-buffer))
          (kill-buffer))))
     (annotation-storage-files
      (loop for annotation-storage-file in annotation-storage-files
            do (copy-file annotation-storage-file
                          (replace-regexp-in-string
                           (concat "annotated-\\["
                                   (replace-regexp-in-string
                                    "/" "_" (substring-no-properties (abbreviate-file-name from) 1)))
                           (concat "annotated-["
                                   (replace-regexp-in-string
                                    "/" "_" (substring-no-properties (abbreviate-file-name to) 1)))
                           annotation-storage-file)
                          ))))))

(defun my/refile (file headline)
  "Copy org entity to headline in file."
  (let ((pos (save-excursion
               (find-file file)
               (org-find-exact-headline-in-buffer headline)))
        (org-refile-keep t))
    (org-refile nil nil (list headline file nil pos) "Copy")))

(defun wdired-finish-edit ()
  "Actually rename files based on your editing in the Dired buffer."
  (interactive)
  (wdired-change-to-dired-mode)
  (let ((changes nil)
        (errors 0)
        files-deleted
        files-renamed
        some-file-names-unchanged
        file-old file-new tmp-value)
    (save-excursion
      (when (and wdired-allow-to-redirect-links
                 (fboundp 'make-symbolic-link))
        (setq tmp-value (wdired-do-symlink-changes))
        (setq errors (cdr tmp-value))
        (setq changes (car tmp-value)))
      (when (and wdired-allow-to-change-permissions
                 (boundp 'wdired-col-perm)) ; could have been changed
        (setq tmp-value (wdired-do-perm-changes))
        (setq errors (+ errors (cdr tmp-value)))
        (setq changes (or changes (car tmp-value))))
      (goto-char (point-max))
      (while (not (bobp))
        (setq file-old (wdired-get-filename nil t))
        (when file-old
          (setq file-new (wdired-get-filename))
          (if (equal file-new file-old)
              (setq some-file-names-unchanged t)
            (setq changes t)
            (if (not file-new)          ;empty filename!
                (push file-old files-deleted)
              (when wdired-keep-marker-rename
                (let ((mark (cond ((integerp wdired-keep-marker-rename)
                                   wdired-keep-marker-rename)
                                  (wdired-keep-marker-rename
                                   (cdr (assoc file-old wdired-old-marks)))
                                  (t nil))))
                  (when mark
                    (push (cons (substitute-in-file-name file-new) mark)
                          wdired-old-marks))))
              (push (cons file-old (substitute-in-file-name file-new))
                    files-renamed))))
        (forward-line -1)))
    (when files-renamed
      (setq errors (+ errors (wdired-do-renames files-renamed))))
    (if changes
        (progn
          ;; If we are displaying a single file (rather than the
          ;; contents of a directory), change dired-directory if that
          ;; file was renamed.  (This ought to be generalized to
          ;; handle the multiple files case, but that's less trivial).
          (when (and (stringp dired-directory)
                     (not (file-directory-p dired-directory))
                     (null some-file-names-unchanged)
                     (= (length files-renamed) 1))
            (setq dired-directory (cdr (car files-renamed))))
          ;; Re-sort the buffer.
          (dired-sync-annotated-status files-renamed)
          (revert-buffer)
          (let ((inhibit-read-only t))
            (dired-mark-remembered wdired-old-marks)))
      (let ((inhibit-read-only t))
        (remove-text-properties (point-min) (point-max)
                                '(old-name nil end-name nil old-link nil
                                           end-link nil end-perm nil
                                           old-perm nil perm-changed nil))
        (message "(No changes to be performed)")))
    (when files-deleted
      (wdired-flag-for-deletion files-deleted))
    (when (> errors 0)
      (dired-log-summary (format "%d rename actions failed" errors) nil)))
  (set-buffer-modified-p nil)
  (setq buffer-undo-list nil))

(defun dired-sync-annotated-status (files-renamed)
  "Sync dired annotated status in wdired-mode."
  (let ((annotated-file-list (if (dired-k--parse-annotated-status)
                                 (hash-table-keys (dired-k--parse-annotated-status)))))
    (loop for file-renamed in files-renamed
          do (let ((file-renamed-old (car file-renamed))
                   (file-renamed-new (cdr file-renamed))
                   (annotation-storage-files
                    (directory-files "~/org/annotated/" t
                                     (concat "annotated-\\["
                                             (replace-regexp-in-string
                                              "/" "_" (substring-no-properties (abbreviate-file-name (car file-renamed)) 1))))))

               (cond
                ((member (file-name-nondirectory file-renamed-old) annotated-file-list)
                 (with-current-buffer (find-file (swint-org-annotate-file-storage-file))
                   (widen)
                   (goto-char (point-min))
                   (replace-string
                    (org-make-link-string (concat "file:" (file-name-nondirectory file-renamed-old)))
                    (org-make-link-string (concat "file:" (file-name-nondirectory file-renamed-new))))
                   (if is-win
                       (save-buffer-with-dos2unix)
                     (save-buffer))
                   (kill-buffer)))
                (annotation-storage-files
                 (loop for annotation-storage-file in annotation-storage-files
                       do (rename-file annotation-storage-file
                                       (replace-regexp-in-string
                                        (concat "annotated-\\["
                                                (replace-regexp-in-string
                                                 "/" "_" (substring-no-properties (abbreviate-file-name file-renamed-old) 1)))
                                        (concat "annotated-["
                                                (replace-regexp-in-string
                                                 "/" "_" (substring-no-properties (abbreviate-file-name file-renamed-new) 1)))
                                        annotation-storage-file)
                                       ))))))))

(defun dired-do-flagged-delete (&optional nomessage)
  "In Dired, delete the files flagged for deletion.
If NOMESSAGE is non-nil, we don't display any message
if there are no flagged files.
`dired-recursive-deletes' controls whether deletion of
non-empty directories is allowed."
  (interactive)
  (let* ((dired-marker-char dired-del-marker)
         (regexp (dired-marker-regexp))
         case-fold-search)
    (if (save-excursion (goto-char (point-min))
                        (re-search-forward regexp nil t))
        (progn (dired-internal-do-deletions
                ;; this can't move point since ARG is nil
                (setq files-to-be-deleted (dired-map-over-marks (cons (dired-get-filename) (point))
                                                                nil))
                nil t)
               (let ((annotated-file-list (if (dired-k--parse-annotated-status)
                                              (hash-table-keys (dired-k--parse-annotated-status)))))
                 (loop for file-to-be-deleted in files-to-be-deleted
                       do (let ((file-being-deleted (car file-to-be-deleted))
                                (annotation-storage-files
                                 (directory-files "~/org/annotated/" t
                                                  (concat "annotated-\\["
                                                          (replace-regexp-in-string
                                                           "/" "_" (substring-no-properties (abbreviate-file-name (car file-to-be-deleted)) 1))))))

                            (cond
                             ((member (file-name-nondirectory file-being-deleted) annotated-file-list)
                              (progn (with-current-buffer (find-file (swint-org-annotate-file-storage-file))
                                       (widen)
                                       (goto-char (point-min))
                                       (search-forward (org-make-link-string (concat "file:" (file-name-nondirectory file-being-deleted))) nil t)
                                       (org-delete-entity-at-point)
                                       (if is-win
                                           (save-buffer-with-dos2unix)
                                         (save-buffer))
                                       (kill-buffer))
                                     (unless (hash-table-keys (dired-k--parse-annotated-status))
                                       (delete-file (swint-org-annotate-file-storage-file)))))
                             (annotation-storage-files
                              (mapcar 'delete-file annotation-storage-files)))))
                 files-to-be-deleted))
      (or nomessage
          (message "(No deletions requested)")))))

(defun org-delete-entity-at-point ()
  "Org delete entity at point."
  (interactive)
  (let* ((el (org-element-at-point))
         (beg (org-element-property :begin el))
         (end (org-element-property :end el)))
    (delete-region beg end)))

(provide 'dired-sync-annotated)
;;; dired-sync-annotated.el ends here
