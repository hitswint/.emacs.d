;;; swint-org-open-at-point
;; ===========swint-org-open-at-point=========
(defun org-at-top-heading-p ()
  "Go back to top heading and return that point. If already on top heading, return nil."
  (let ((headline (org-element-at-point)))
    (and (org-at-heading-p)
         (equal (org-element-type headline) 'headline)
         (equal (org-element-property :level headline) 1))))
(defun swint-get-annotated-file ()
  (when (and (ignore-errors (org-at-top-heading-p))
             (string-prefix-p "annotated-(" (file-name-nondirectory (buffer-file-name))))
    (concat "~"
            (replace-regexp-in-string
             "_" "/" (substring-no-properties (file-name-nondirectory (buffer-file-name)) 11 -5))
            (car (last (split-string (substring-no-properties (org-get-heading t) nil -2) "\\[file:") 1)))))
;;;###autoload
(defun swint-org-open-at-point (&optional in-emacs)
  "Open annotated file if annotation storage file exists."
  (interactive)
  (when (and (org-in-regexp org-link-bracket-re 1)
             (string-match "\\`\\(pdf\\|pdfview\\):" (match-string-no-properties 1))
             (not (org-link-get-parameter "pdfview" :follow)))
    (org-pdftools-setup-link))
  (cl-flet* ((find-page () (or (org-entry-get nil "NOTER_PAGE")
                               (org-entry-get nil "interleave_page_note")
                               (org-entry-get nil "annotated_page")))
             (get-annotate-page () (save-excursion
                                     (org-back-to-heading)
                                     (let ((page (find-page)))
                                       (while (and (org-up-heading-safe) (null page))
                                         (setq page (find-page)))
                                       page)))
             (get-annotate-file (global-p) (save-excursion
                                             (when global-p
                                               (while (org-up-heading-safe)))
                                             (or (swint-get-annotated-file)
                                                 (car (bibtex-completion-find-pdf (org-entry-get nil "Custom_ID")))
                                                 (interleave--find-pdf-path (current-buffer))
                                                 (org-entry-get nil "interleave_url")))))
    (let ((annotate-file (when (or (ignore-errors (org-at-top-heading-p)) (org-at-keyword-p)) (get-annotate-file nil)))
          (annotate-page (unless (org-in-regexp org-link-any-re) (get-annotate-page))))
      (cond ((and annotate-file (file-exists-p annotate-file))
             (org-open-file annotate-file in-emacs))
            (annotate-page
             (start-process "Shell" nil shell-file-name shell-command-switch
                            (concat "qpdfview --unique \"" (expand-file-name (get-annotate-file t))
                                    "\"#" annotate-page)))
            (t
             (org-open-at-point in-emacs))))))
;;;###autoload
(defun swint-org-open-dired-at-point ()
  "Open Dired for directory of file link at point."
  (interactive)
  (let ((el (org-element-lineage (org-element-context) '(link) t)))
    (unless (and el (equal (org-element-property :type el) "file"))
      (user-error "Not on file link"))
    (dired-jump 'other-window
                (expand-file-name (org-element-property :path el)))))
;; ===========swint-org-open-at-point=========
;;; mobileorg
;; =================mobileorg=================
(setq org-directory "~/org")
(setq org-mobile-inbox-for-pull "~/org/task-from-mobile.org")
(setq org-mobile-directory "~/webdav-sync/mobileorg")
(setq org-mobile-encryption-tempfile "~/org/orgtmpcrypt")
(setq org-mobile-files (list "~/webdav-sync/orgzly/task.org"))
;; =================mobileorg=================
;;; swint-qpdfview-annotated-new
;; ========swint-qpdfview-annotated-new=======
;;;###autoload
(defun swint-qpdfview-annotated-new ()
  "Open annotated file if annotation storage file exists."
  (interactive)
  (let* ((annotated-file (save-excursion
                           (while (org-up-heading-safe))
                           (or (swint-get-annotated-file)
                               (car (bibtex-completion-find-pdf (org-entry-get nil "Custom_ID")))
                               (interleave--find-pdf-path (current-buffer)))))
         (qpdfview-database (expand-file-name "~/.local/share/qpdfview/qpdfview/database"))
         (qpdfview-page (when (and (not (string-empty-p (shell-command-to-string "pgrep -x qpdfview"))) annotated-file)
                          ;; 需在qpdfview设定中将Save database interval设置为0min，否则数据库无法及时更新
                          (shell-command-to-string
                           (format "sqlite3 %s \"select currentPage from tabs_v5 where filePath=\\\"%s\\\"\"" "~/.local/share/qpdfview/qpdfview/database" (expand-file-name annotated-file))))))
    (if (or (string-empty-p qpdfview-page) (null qpdfview-page))
        (message "No file annotated or not opened in qpdfview.")
      (org-entry-put nil (cond ((file-in-directory-p (buffer-file-name) "~/org/annotated") "annotated_page")
                               ((file-equal-p (buffer-file-name) bibtex-completion-notes-path) "NOTER_PAGE")
                               ((file-in-directory-p (buffer-file-name) "~/org/interleave_notes") "interleave_page_note"))
                     (string-trim qpdfview-page)))))
;; ========swint-qpdfview-annotated-new=======
;;; swint-cursor-localtion
;; ===========swint-cursor-localtion==========
;;;###autoload
(defun swint-cursor-localtion ()
  (interactive)
  (let ((command-output (shell-command-to-string "cursor_loc.sh")))
    (car (last (split-string command-output "\n" t "[\t\n]+")))))
;; ===========swint-cursor-localtion==========
;;; swint-org-redisplay-inline-images
;; =====swint-org-redisplay-inline-images=====
;;;###autoload
(defun swint-org-redisplay-inline-images ()
  (if (and (display-graphic-p) (not (featurep 'perspective)))
      (dolist (buf (cl-remove-if-not (lambda (x)
                                       (equal (buffer-mode x) 'org-mode))
                                     (buffer-list)))
        (with-current-buffer buf
          (org-redisplay-inline-images)))))
;; =====swint-org-redisplay-inline-images=====
