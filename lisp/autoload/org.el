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
(defun org-get-annotate-page (&optional operator)
  (cl-flet ((find-page () (or (org-entry-get nil "NOTER_PAGE")
                              (org-entry-get nil "interleave_page_note")
                              (org-entry-get nil "annotated_page"))))
    (let ((page (find-page))
          point)
      (save-excursion
        (org-back-to-heading)
        (while (and (null page) (org-up-heading-safe))
          (setq page (find-page)))
        (setq point (point)))
      (cond ((eq operator 'prev)
             (goto-char point)
             (org-backward-heading-same-level nil)
             (find-page))
            ((eq operator 'next)
             (goto-char point)
             (org-forward-heading-same-level nil)
             (find-page))
            (t page)))))
(defun org-get-annotate-file ()
  (save-excursion
    (while (org-up-heading-safe))
    (or (swint-get-annotated-file)
        (car (bibtex-completion-find-pdf (org-entry-get nil "Custom_ID")))
        (org-entry-get nil "interleave_url"))))
;;;###autoload
(defun eaf-find-file-other-window (filename)
  (if (and (buffer-live-p (get-buffer "*eaf*"))
           (member (ignore-errors (downcase (file-name-extension filename)))
                   (cl-loop for extensions-pair in eaf-app-extensions-alist
                            append (symbol-value (cdr extensions-pair)))))
      (progn
        (switch-to-buffer-other-window (current-buffer))
        (find-file filename))
    (find-file-other-window filename)))
;;;###autoload
(defun swint-org-open-at-point (&optional in-emacs)
  "Open annotated file if annotation storage file exists."
  (interactive)
  (if (and (org-in-regexp org-link-bracket-re 1)
           (string-match "\\`\\(pdf\\|pdfview\\):" (match-string-no-properties 1)))
      (progn (unless (org-link-get-parameter "pdfview" :follow)
               (org-pdftools-setup-link))
             (org-open-at-point in-emacs))
    (let ((annotate-file (when (or (ignore-errors (org-at-top-heading-p)) (org-at-keyword-p)) (org-get-annotate-file)))
          (annotate-page (unless (org-in-regexp org-link-any-re) (org-get-annotate-page)))
          (org-link-frame-setup (cl-acons 'file 'eaf-find-file-other-window org-link-frame-setup)))
      (cond (annotate-page
             (let ((file (expand-file-name (org-get-annotate-file))))
               (if (not in-emacs)
                   (start-process "Shell" nil shell-file-name shell-command-switch
                                  (concat "qpdfview --unique \"" file "\"#" annotate-page))
                 (if-let ((eaf-pdf-win (eaf-find-pdf-window)))
                     (select-window eaf-pdf-win)
                   (switch-to-buffer-other-window (current-buffer)))
                 (eaf-open-pdf-with-page file annotate-page))))
            ((and annotate-file (file-exists-p annotate-file))
             (org-open-file annotate-file in-emacs))
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
;;; org-eaf-pdf-sync
;; ==============org-eaf-pdf-sync=============
;;;###autoload
(defun org-eaf-pdf-sync (&optional operator)
  (interactive)
  (let* ((annotate-page (org-get-annotate-page operator))
         (annotate-file (expand-file-name (org-get-annotate-file)))
         (note-win (selected-window)))
    (if-let ((eaf-pdf-win (eaf-find-pdf-window)))
        (select-window eaf-pdf-win)
      (delete-other-windows)
      (setq note-win (split-window-right)))
    (eaf-open-pdf-with-page annotate-file annotate-page)
    (select-window note-win)))
;;;###autoload
(defun org-eaf-pdf-sync-prev ()
  (interactive)
  (org-eaf-pdf-sync 'prev))
;;;###autoload
(defun org-eaf-pdf-sync-next ()
  (interactive)
  (org-eaf-pdf-sync 'next))
;;;###autoload
(defun org-eaf-noter-sync (&optional operator new-note)
  (interactive)
  (let ((current-page (string-to-number (eaf-call-sync "execute_function" eaf--buffer-id "current_page")))
        (key-for-pdf (when (file-in-directory-p eaf--buffer-url "~/Zotero")
                       (bibtex-completion-get-value "=key=" (bibtex-completion-get-entry-for-pdf eaf--buffer-url))))
        (compare-operator (cond ((eq operator 'prev) #'<)
                                ((eq operator 'next) #'>)
                                (t #'=))))
    (if (not key-for-pdf)
        (message "Only pdf files in Zotero are supported now")
      (find-file-other-window bibtex-completion-notes-path)
      (widen)
      (let ((orig-point (point))
            (point (if (equal key-for-pdf (org-entry-get-with-inheritance "Custom_ID"))
                       (prog1 (point)
                         (while (org-up-heading-safe)))
                     (goto-char (point-min))
                     (when (re-search-forward (format bibtex-completion-notes-key-pattern (regexp-quote key-for-pdf)) nil t)
                       (org-back-to-heading t)))))
        (cond (point (let (note-page)
                       (org-narrow-to-subtree)
                       (if (eq operator 'prev)
                           (goto-char (point-max))
                         (goto-char (point-min)))
                       (while (and (funcall (if (eq operator 'prev) #'re-search-backward #'re-search-forward)
                                            (format "^\[ \t\r\]*\:%s\: \\([0-9]+\\)$" "NOTER_PAGE") nil t)
                                   (not (funcall compare-operator
                                                 (string-to-number (buffer-substring (match-beginning 1) (match-end 1)))
                                                 current-page)))
                         (org-cycle-hide-drawers t))
                       (widen)
                       (org-back-to-heading t)
                       (org-cycle-hide-drawers t)
                       (setq note-page (org-entry-get-with-inheritance "NOTER_PAGE"))
                       (if (and note-page (funcall compare-operator
                                                   (string-to-number note-page)
                                                   current-page))
                           (unless new-note
                             (org-eaf-pdf-sync))
                         (message "No %s page found" (or operator 'current))
                         (goto-char point))))
              (new-note (bibtex-completion-edit-notes (list key-for-pdf))
                        (bibtex-completion-notes-mode -1)
                        (widen)
                        (setq-local header-line-format nil))
              (t (goto-char orig-point)
                 (message "Cannot find any note entry for current pdf")))))))
;;;###autoload
(defun org-eaf-noter-sync-prev ()
  (interactive)
  (org-eaf-noter-sync 'prev))
;;;###autoload
(defun org-eaf-noter-sync-next ()
  (interactive)
  (org-eaf-noter-sync 'next))
;;;###autoload
(defun org-eaf-noter-new ()
  (interactive)
  (org-eaf-noter-sync 'prev t)
  (swint-annotate-generic-new))
;; ==============org-eaf-pdf-sync=============
;;; mobileorg
;; =================mobileorg=================

;; =================mobileorg=================
;;; swint-annotate-generic-new
;; ==========swint-annotate-generic-new=======
;;;###autoload
(defun swint-annotate-generic-new ()
  "Add annotate for current file using generic way."
  (interactive)
  (let* ((annotate-file (org-get-annotate-file))
         (qpdfview-database (expand-file-name "~/.local/share/qpdfview/qpdfview/database"))
         (qpdfview-page (when (and (not (string-empty-p (shell-command-to-string "pgrep -x qpdfview"))) annotate-file)
                          ;; 需在qpdfview设定中将Save database interval设置为0min，否则数据库无法及时更新
                          (shell-command-to-string
                           (format "sqlite3 %s \"select currentPage from tabs_v5 where filePath=\\\"%s\\\"\"" "~/.local/share/qpdfview/qpdfview/database" (expand-file-name annotate-file)))))
         (eaf-pdf-page (when-let ((annotate-buffer (eaf-interleave--find-buffer annotate-file)))
                         (with-current-buffer annotate-buffer
                           (eaf-call-sync "execute_function" eaf--buffer-id "current_page"))))
         (current-page (or (s-presence eaf-pdf-page) (s-presence qpdfview-page))))
    (if (or (string-empty-p current-page) (null current-page))
        (message "No file annotated or not opened in qpdfview.")
      (let ((page (string-trim current-page)))
        (cond ((file-in-directory-p (buffer-file-name) "~/org/annotated")
               (org-insert-heading-after-current)
               (org-entry-put nil "annotated_page" page))
              ((file-equal-p (buffer-file-name) bibtex-completion-notes-path)
               (while (and (> (org-current-level) 2) (org-up-heading-safe)))
               (if (= (org-current-level) 2)
                   (org-insert-heading-after-current)
                 (org-insert-heading nil nil 2))
               (org-entry-put nil "NOTER_PAGE" page))
              ((file-in-directory-p (buffer-file-name) "~/org/interleave_notes")
               (while (and (> (org-current-level) 1) (org-up-heading-safe)))
               (org-insert-heading-after-current)
               (org-entry-put nil "interleave_url" annotate-file)
               (org-entry-put nil "interleave_page_note" page)))))))
;; ==========swint-annotate-generic-new=======
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
