;;; org-annotate-file/dired-x-highlight.el --- Display annotated files with mark
(defface dired-k-annotated-locally
  '((t (:foreground "red" :weight bold)))
  "Face of modified file in git repository"
  :group 'dired-k)

(defface dired-k-annotated-globally
  '((t (:foreground "magenta" :weight bold)))
  "Face of modified file in git repository"
  :group 'dired-k)

(defface dired-k-interleaved
  '((t (:foreground "orange" :weight bold)))
  "Face of modified file in git repository"
  :group 'dired-k)

(defface dired-k-none nil
  "Face of modified file in git repository"
  :group 'dired-k)

(defgroup dired-k nil
  "k.sh in dired"
  :group 'dired)

(defun swint-interleaved-files ()
  (cl-remove-if-not (lambda (x) (member (file-name-base x)
                                        (mapcar 'file-name-base (directory-files "~/org/interleave_notes" nil "\\.[oO][rR][gG]\\'"))))
                    (directory-files default-directory nil "\\.[pP][dD][fF]\\'")))

(defun dired-k--highlight-buffer ()
  (save-excursion
    (let ((files-status (dired-k--parse-status))
          (curbuf (current-buffer)))
      (if files-status
          (funcall 'dired-k--highlighted-file files-status curbuf)))))

(defun dired-k--highlighted-file (stats buf)
  "Load definition of hash-table-keys."
  (require 'subr-x)
  (with-current-buffer buf
    (let ((highlighted-files-list (hash-table-keys stats)))
      (loop for highlighted-file in highlighted-files-list
            do (if (file-exists-p highlighted-file)
                   (progn (dired-goto-file (concat (expand-file-name default-directory) highlighted-file))
                          (dired-k--highlight-line highlighted-file stats)))))))

(defun dired-k--highlight-line (file stats)
  (let* ((stat (gethash file stats 'none))
         (ov (make-overlay (point) (1+ (point))))
         (stat-face (dired-k--status-color stat))
         (marker-symbol (substring-no-properties file 0 1)))
    (unless (eq stat-face 'dired-k-none)
      (overlay-put ov 'display (propertize marker-symbol 'face stat-face)))))

;; (defun dired-k--highlight-line-normal (stat)
;;   (let ((ov (make-overlay (point) (1+ (point))))
;;         (stat-face (dired-k--status-color stat))
;;         (marker-symbol (substring-no-properties (thing-at-point 'symbol) 0 1))
;;         )
;;     (if (eq stat-face 'dired-k-annotated)
;;         (overlay-put ov 'display (propertize marker-symbol 'face stat-face)))
;;     ))

(defsubst dired-k--status-color (stat)
  (cl-case stat
    (annotated-locally 'dired-k-annotated-locally)
    (annotated-globally 'dired-k-annotated-globally)
    (interleaved 'dired-k-interleaved)
    (none 'dired-k-none)))

(defun dired-k--parse-status (&optional only-annotated-locally)
  "Parse status of files."
  (let ((files-status (make-hash-table :test 'equal)))
    ;; Parse status of files annotated locally.
    (when (file-exists-p (swint-org-annotation-storage-file))
      (with-temp-buffer
        (insert-file-contents (swint-org-annotation-storage-file))
        (goto-char (point-min))
        (let ((status 'annotated-locally))
          (while (search-forward-regexp
                  (concat "^* " "\\[\\[file:") nil t)
            (puthash (car (last (split-string (buffer-substring (line-beginning-position) (- (line-end-position) 2)) "\\[file:") 1))
                     status files-status)))))
    (unless only-annotated-locally
      ;; Parse status of files annotated globally.
      (with-temp-buffer
        (insert-file-contents "~/org/annotated/annotated.org")
        (goto-char (point-min))
        (let ((status 'annotated-globally))
          (while (search-forward-regexp
                  (concat "^* " "\\[\\[file:.+\\]\\[file:"
                          (regexp-quote (abbreviate-file-name default-directory))
                          "\\([^/]+\\)\\]\\]") nil t)
            (puthash (match-string-no-properties 1) status files-status))))
      ;; Parse status of files interleaved.
      (let ((pdf-files-interleaved (swint-interleaved-files))
            (status 'interleaved))
        (when pdf-files-interleaved
          (mapc #'(lambda (x)
                    (puthash x status files-status))
                pdf-files-interleaved))))
    (when (eq (hash-table-count files-status) 0)
      (setq files-status nil))
    files-status))

;; (defsubst hash-table-keys (hash-table)
;;   "Return a list of keys in HASH-TABLE."
;;   (let ((keys '()))
;;     (maphash (lambda (k _v) (push k keys)) hash-table)
;;     keys))

(defun dired-k--previous-highlighted-file ()
  "Goto previous highlighted file."
  (interactive)
  (let* ((files-status (dired-k--parse-status))
         (highlighted-files-list (if files-status
                                     (hash-table-keys files-status)))
         result)
    (when highlighted-files-list
      (goto-char (- (line-beginning-position) 1))
      (setq result (concat " " (car highlighted-files-list)))
      (dotimes (i (- (length highlighted-files-list) 1))
        (setq result (concat result "\\| " (nth (+ i 1) highlighted-files-list))))
      (if (not (re-search-backward (concat "\\\(" result "\\\)") nil t))
          (progn (goto-char (point-max))
                 (dired-k--previous-highlighted-file))
        (smart-beginning-of-line)))))

(defun dired-k--next-highlighted-file ()
  "Goto next highlighted file."
  (interactive)
  (let* ((files-status (dired-k--parse-status))
         (highlighted-files-list (if files-status
                                     (hash-table-keys files-status)))
         result)
    (when highlighted-files-list
      (goto-char (+ (line-end-position) 1))
      (setq result (concat " " (car highlighted-files-list)))
      (dotimes (i (- (length highlighted-files-list) 1))
        (setq result (concat result "\\| " (nth (+ i 1) highlighted-files-list))))
      (if (not (re-search-forward (concat "\\\(" result "\\\)") nil t))
          (progn (goto-char (point-min))
                 (dired-k--next-highlighted-file))
        (smart-beginning-of-line)))))

(provide 'dired-x-highlight)
;;; dired-x-highlight.el ends here
