;;; dired-sync-highlight.el --- Sync highlighted files status
(require 'dired-aux)
(require 'wdired)

(defun my/refile (file headline)
  "Copy org entity to headline in file."
  (let ((pos (save-excursion
               (find-file file)
               (org-find-exact-headline-in-buffer headline)))
        (org-refile-keep t))
    (org-refile nil nil (list headline file nil pos) "Copy")))

(defun dired-copy-file/after (from to ok-flag)
  "Sync dired annotated status when copy files."
  (cl-flet ((annotated-path (x)
                            (concat "annotated-("
                                    (replace-regexp-in-string
                                     "/" "_"
                                     (substring-no-properties
                                      (abbreviate-file-name x) 1)))))

    (let* ((annotated-file-list (if (dired-k--parse-status t)
                                    (hash-table-keys (dired-k--parse-status t))))
           (dir-from-str (annotated-path (file-name-directory from)))
           (dir-to-str (annotated-path (file-name-directory to)))
           (tar-from-str (annotated-path from))
           (tar-to-str (annotated-path to))
           (annotation-storage-files (directory-files "~/org/annotated/" t tar-from-str)))
      (when (member (file-name-nondirectory from) annotated-file-list)
        (let* ((annotation-refile-from (concat "~/org/annotated/" dir-from-str ").org"))
               (annotation-refile-to (concat "~/org/annotated/" dir-to-str ").org"))
               (link-from (concat "file:" (file-name-nondirectory from)))
               (link-to (concat "file:" (file-name-nondirectory to)))
               (string-from (org-make-link-string link-from link-from))
               (string-to (org-make-link-string link-to link-to)))
          (with-current-buffer (find-file annotation-refile-from)
            (widen)
            (goto-char (point-min))
            (search-forward string-from nil t)
            (if (org-at-heading-p)
                (my/refile annotation-refile-to (org-get-heading t)))
            (kill-buffer))
          (with-current-buffer (find-file annotation-refile-to)
            (widen)
            (goto-char (point-min))
            (while (search-forward string-from nil t)
              (replace-match string-to))
            (save-buffer)
            (kill-buffer))))
      (when annotation-storage-files
        (loop for annotation-storage-file in annotation-storage-files
              do (copy-file annotation-storage-file
                            (replace-regexp-in-string
                             tar-from-str tar-to-str annotation-storage-file)))))))

(advice-add 'dired-copy-file :after #'dired-copy-file/after)

(defun wdired-do-renames/before (files-renamed)
  "Sync dired annotated status in wdired-mode."
  (cl-flet ((annotated-path (x)
                            (concat "annotated-("
                                    (replace-regexp-in-string
                                     "/" "_"
                                     (substring-no-properties
                                      (abbreviate-file-name x) 1)))))
    (let ((annotated-file-list (if (dired-k--parse-status t)
                                   (hash-table-keys (dired-k--parse-status t)))))
      (loop for file-renamed in files-renamed
            do (let* ((file-renamed-old (car file-renamed))
                      (file-renamed-new (cdr file-renamed))
                      (annotation-storage-files (directory-files "~/org/annotated/" t
                                                                 (annotated-path file-renamed-old))))
                 (when (member (file-name-nondirectory file-renamed-old) annotated-file-list)
                   (let* ((link-old (concat "file:" (file-name-nondirectory file-renamed-old)))
                          (link-new (concat "file:" (file-name-nondirectory file-renamed-new)))
                          (string-old (org-make-link-string link-old link-old))
                          (string-new (org-make-link-string link-new link-new)))
                     (with-current-buffer (find-file (swint-org-annotation-storage-file))
                       (widen)
                       (goto-char (point-min))
                       (while (search-forward string-old nil t)
                         (replace-match string-new t t))
                       (save-buffer)
                       (kill-buffer))))
                 (when annotation-storage-files
                   (loop for annotation-storage-file in annotation-storage-files
                         do (rename-file annotation-storage-file
                                         (replace-regexp-in-string
                                          (annotated-path file-renamed-old)
                                          (annotated-path file-renamed-new)
                                          annotation-storage-file)))))))))

(advice-add 'wdired-do-renames :before #'wdired-do-renames/before)

(defun org-delete-entity-at-point ()
  "Org delete entity at point."
  (interactive)
  (let* ((el (org-element-at-point))
         (beg (org-element-property :begin el))
         (end (org-element-property :end el)))
    (delete-region beg end)))

(defun dired-internal-do-deletions/before (l arg &optional trash)
  "Sync dired annotated status when delete files."
  (let ((files-deleted (mapcar (function car) l))
        (annotated-file-list (if (dired-k--parse-status t)
                                 (hash-table-keys (dired-k--parse-status t)))))
    (loop for file-deleted in files-deleted
          do (let ((annotation-storage-files
                    (directory-files "~/org/annotated/" t
                                     (concat "annotated-("
                                             (replace-regexp-in-string
                                              "/" "_"
                                              (substring-no-properties (abbreviate-file-name file-deleted) 1))))))
               (when (member (file-name-nondirectory file-deleted) annotated-file-list)
                 (let ((link-deleted (concat "file:" (file-name-nondirectory file-deleted))))
                   (with-current-buffer (find-file (swint-org-annotation-storage-file))
                     (widen)
                     (goto-char (point-min))
                     (search-forward (org-make-link-string link-deleted link-deleted) nil t)
                     (org-delete-entity-at-point)
                     (save-buffer)
                     (kill-buffer)))
                 (unless (dired-k--parse-status t)
                   (delete-file (swint-org-annotation-storage-file))))
               (when annotation-storage-files
                 (mapc 'delete-file annotation-storage-files))))))

(advice-add 'dired-internal-do-deletions :before #'dired-internal-do-deletions/before)

(provide 'dired-sync-highlight)
;;; dired-sync-highlight.el ends here
