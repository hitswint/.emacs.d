;; dired-x-annotated.el --- Display annotated files with mark.
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
    (if (and (file-exists-p (swint-org-annotate-file-storage-file))
             (not (equal (buffer-name) "org"))) ;org文件夹下不显示annotated文件
        (let ((curbuf (current-buffer))
              (stats (dired-k--parse-annotated-status)))
          (funcall 'dired-k--highlight-annotated-file stats curbuf)))))

(defun dired-k--highlight-annotated-file (stats buf)
  (require 'subr-x)                     ;Load definition of hash-table-keys
  (with-current-buffer buf
    (let ((annotated-file-list (hash-table-keys stats)))
      (loop for annotated-file in annotated-file-list
            do (if (file-exists-p annotated-file)
                   (progn (dired-goto-file (concat (expand-file-name default-directory) annotated-file))
                          (dired-k--highlight-line annotated-file stats)))))))

(defun dired-k--highlight-line (file stats)
  (let* ((stat (gethash file stats 'not-annotated))
         (ov (make-overlay (point) (1+ (point))))
         (stat-face (dired-k--annotated-status-color stat))
         (marker-symbol (substring-no-properties file 0 1)))
    (if (eq stat-face 'dired-k-annotated)
        (overlay-put ov 'display (propertize marker-symbol 'face stat-face)))))

;; (defun dired-k--highlight-line-normal (stat)
;;   (let ((ov (make-overlay (point) (1+ (point))))
;;         (stat-face (dired-k--annotated-status-color stat))
;;         (marker-symbol (substring-no-properties (thing-at-point 'symbol) 0 1))
;;         )
;;     (if (eq stat-face 'dired-k-annotated)
;;         (overlay-put ov 'display (propertize marker-symbol 'face stat-face)))
;;     ))

(defsubst dired-k--annotated-status-color (stat)
  (cl-case stat
    (annotated 'dired-k-annotated)
    (not-annotated 'dired-k-not-annotated)
    ))

(defun dired-k--parse-annotated-status ()
  (if (file-exists-p (swint-org-annotate-file-storage-file))
      (with-temp-buffer
        (insert-file-contents (swint-org-annotate-file-storage-file))
        (goto-char (point-min))
        (let ((files-status (make-hash-table :test 'equal))
              (status 'annotated))
          (while (search-forward-regexp
                  (concat "^* " "\\[\\[file:") nil t)
            (puthash (car (last (split-string (buffer-substring (line-beginning-position) (- (line-end-position) 2)) "\\[file:") 1)) status files-status))
          files-status))))

;; (defsubst hash-table-keys (hash-table)
;;   "Return a list of keys in HASH-TABLE."
;;   (let ((keys '()))
;;     (maphash (lambda (k _v) (push k keys)) hash-table)
;;     keys))

(defun dired-k--previous-annotated-file ()
  "Goto previous annotated file"
  (interactive)
  (goto-char (- (line-beginning-position) 1))
  (let ((file-name-annotated (hash-table-keys (dired-k--parse-annotated-status))))
    (setq result (concat " " (car file-name-annotated)))
    (dotimes (i (- (length file-name-annotated) 1))
      (setq result (concat result "\\| " (nth (+ i 1) file-name-annotated))))
    (if (not (re-search-backward (concat "\\\(" result "\\\)") nil t))
        (progn (goto-char (point-max))
               (dired-k--previous-annotated-file))
      (smart-beginning-of-line))))

(defun dired-k--next-annotated-file ()
  "Goto next annotated file"
  (interactive)
  (goto-char (+ (line-end-position) 1))
  (let ((file-name-annotated (hash-table-keys (dired-k--parse-annotated-status))))
    (setq result (concat " " (car file-name-annotated)))
    (dotimes (i (- (length file-name-annotated) 1))
      (setq result (concat result "\\| " (nth (+ i 1) file-name-annotated))))
    (if (not (re-search-forward (concat "\\\(" result "\\\)") nil t))
        (progn (goto-char (point-min))
               (dired-k--next-annotated-file))
      (smart-beginning-of-line))))

(provide 'dired-x-annotated)
;;; dired-x-annotated.el ends here
