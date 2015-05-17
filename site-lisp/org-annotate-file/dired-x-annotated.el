;; dired-x-annotated.el --- Display annotated files with mark
(defface dired-k-annotated
  '((t (:foreground "red" :weight bold)))
  "Face of modified file in git repository"
  :group 'dired-k)

(defface dired-k-not-annotated nil
  "Face of modified file in git repository"
  :group 'dired-k)

(defgroup dired-k nil
  "k.sh in dired"
  :group 'dired)

(defun dired-k--highlight ()
  (save-excursion
    (if (and (file-exists-p swint-org-annotate-file-storage-file)
             (not (equal (buffer-name) "org"))) ;org文件夹下不显示annotated文件
        (let ((curbuf (current-buffer))
              (stats (dired-k--parse-annotated-status)))
          (funcall 'dired-k--highlight-annotated-file stats curbuf)))))

(defun dired-k--highlight-annotated-file (stats buf)
  (with-current-buffer buf
    (save-excursion
      (goto-char (point-min))
      (dired-next-line 4)
      (while (not (eobp))
        (let ((filename (file-name-nondirectory (dired-get-filename nil t))))
          (when filename
            (dired-k--highlight-line filename stats))
          (dired-next-line 1))
        ))))

(defun dired-k--highlight-line (file stats)
  (let ((stat (gethash file stats 'not-annotated)))
    (dired-k--highlight-line-normal stat)
    ))

(defun dired-k--highlight-line-normal (stat)
  (let ((ov (make-overlay (point) (1+ (point))))
        (stat-face (dired-k--annotated-status-color stat))
        (marker-symbol (substring-no-properties (thing-at-point 'symbol) 0 1))
        )
    (if (eq stat-face 'dired-k-annotated)
        (overlay-put ov 'display (propertize marker-symbol 'face stat-face)))
    ))

(defsubst dired-k--annotated-status-color (stat)
  (cl-case stat
    (annotated 'dired-k-annotated)
    (not-annotated 'dired-k-not-annotated)
    ))

(defun dired-k--parse-annotated-status ()
  (if (file-exists-p swint-org-annotate-file-storage-file)
      (with-temp-buffer
        (insert-file swint-org-annotate-file-storage-file)
        (goto-char (point-min))
        (let ((files-status (make-hash-table :test 'equal))
              (status 'annotated))
          (while (search-forward-regexp
                  (concat "^* " "\\[\\[file:") nil t)
            (puthash (car (last (split-string (buffer-substring (line-beginning-position) (- (line-end-position) 2)) "\\[file:") 1)) status files-status))
          files-status))))

(provide 'dired-x-annotated)
;;; dired-x-annotated.el ends here
