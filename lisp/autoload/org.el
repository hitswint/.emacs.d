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
  (let* ((annotated-file (swint-get-annotated-file)) ;org-annotate
         (noter-key (and (org-at-heading-p) (org-entry-get nil "Custom_ID")))
         (noter-file (car (bibtex-completion-find-pdf noter-key))) ;org-noter
         (file-at-point (or annotated-file noter-file)))
    (if (and file-at-point (file-exists-p file-at-point))
        (org-open-file file-at-point in-emacs)
      (org-open-at-point in-emacs))))
;; ===========swint-org-open-at-point=========
;;; mobileorg
;; =================mobileorg=================
;; Set to the location of your Org files on your local system.
(setq org-directory "~/org")
;; Set to the name of the file where new notes will be stored.
(setq org-mobile-inbox-for-pull "~/org/task-from-mobile.org")
;; Set to <your Dropbox root directory>/MobileOrg.
(setq org-mobile-directory "~/Nutstore-mobileorg")
;; 加密。
(setq org-mobile-encryption-tempfile "~/org/orgtmpcrypt")
(unless (file-exists-p org-mobile-encryption-tempfile)
  (shell-command (concat "touch " (expand-file-name org-mobile-encryption-tempfile))))
(unless (file-exists-p org-mobile-inbox-for-pull)
  (shell-command (concat "touch " (expand-file-name org-mobile-inbox-for-pull))))
;; 设置需要同步的文件。
(setq org-mobile-files (list "~/Nutstore-sync/orgzly/task.org"))
;;;###autoload
(defun swint-org-mobile-sync (arg)
  "Synchronization of org mobile."
  (interactive)
  (cond ((equal arg "down")
         ;; Webdav会造成文件conflict，在pull之前先删除本地mobileorg文件。
         (mapc #'delete-file (directory-files org-mobile-directory t ".+\\.\\(org\\|dat\\)")))
        ((equal arg "up")
         (with-current-buffer "task.org" (org-mobile-push))))
  (let* ((user (replace-regexp-in-string "@" "%40" (get-auth-user "Nutstore")))
         (pass (get-auth-pass "Nutstore"))
         (process
          (start-process-shell-command
           "webdav_sync" "*webdav_sync*"
           (concat "java -Dderby.system.home="  (expand-file-name "~/.webdav_sync/")
                   " -Dbe.re.http.no-compress -jar " (expand-file-name "~/.webdav_sync/webdav_sync1_1_6.jar")
                   " -r -" arg " -u https://" user ":" pass "@dav.jianguoyun.com/dav/Nutstore-mobileorg/ -d "
                   (expand-file-name "~/Nutstore-mobileorg/")))))
    (lexical-let ((pos (memq 'mode-line-modes mode-line-format))
                  (sync (cond ((equal arg "down") "pull")
                              ((equal arg "up") "push"))))
      (setcdr pos (cons (concat "Org-mobile " sync " ") (cdr pos)))
      (set-process-sentinel
       process
       (lambda (process signal)
         (when (memq (process-status process) '(exit signal))
           (let ((webdav_sync-process-output (with-current-buffer "*webdav_sync*"
                                               (buffer-substring-no-properties (- (point-max) 6) (point-max))))
                 (pos (memq 'mode-line-modes mode-line-format)))
             (if (string-equal webdav_sync-process-output "Done.\n")
                 (with-current-buffer "task.org"
                   (when (equal sync "pull")
                     (insert-file-contents "~/Nutstore-mobileorg/task.org" nil nil nil t)
                     (org-mobile-pull)
                     (org-mobile-push))
                   (message "Org-mobile %s done." sync))
               (message "Org-mobile %s failed." sync))
             (setcdr pos (remove (concat "Org-mobile " sync " ") (cdr pos))))))))))
;; =================mobileorg=================
;;; swint-qpdfview-annotated
;; ==========swint-qpdfview-annotated=========
;;;###autoload
(defun swint-qpdfview-annotated-open ()
  "Open annotated file if annotation storage file exists."
  (interactive)
  (cl-flet ((find-page () (or (org-entry-get nil "NOTER_PAGE")
                              (org-entry-get nil "interleave_page_note")
                              (org-entry-get nil "annotated_page"))))
    (let ((page (save-excursion (org-back-to-heading)
                                (find-page)))
          current-file)
      (save-excursion
        (while (not (or (ignore-errors (org-at-top-heading-p)) page))
          (org-up-heading-safe)
          (setq page (find-page))))
      (save-excursion
        (while (not (ignore-errors (org-at-top-heading-p)))
          (org-up-heading-safe))
        (setq current-file (or (swint-get-annotated-file)
                               (car (bibtex-completion-find-pdf (org-entry-get nil "Custom_ID")))
                               (interleave--find-pdf-path (current-buffer)))))
      (if (and current-file page)
          (start-process "Shell" nil shell-file-name shell-command-switch
                         (concat "qpdfview --unique \"" (expand-file-name current-file)
                                 "\"#" page))
        (message "Cannot find file or page.")))))
;;;###autoload
(defun swint-qpdfview-annotated-new ()
  "Open annotated file if annotation storage file exists."
  (interactive)
  (let* ((annotated-file (save-excursion
                           (while (not (ignore-errors (org-at-top-heading-p)))
                             (org-up-heading-safe))
                           (or (swint-get-annotated-file)
                               (car (bibtex-completion-find-pdf (org-entry-get nil "Custom_ID"))))))
         (qpdfview-database (expand-file-name "~/.local/share/qpdfview/qpdfview/database"))
         (qpdfview-page (when (and (not (string-empty-p (shell-command-to-string "pgrep -x qpdfview"))) annotated-file)
                          ;; 需在qpdfview设定中将Save database interval设置为0min，否则数据库无法及时更新
                          (shell-command-to-string
                           (format "sqlite3 %s \"select currentPage from tabs_v5 where filePath=\\\"%s\\\"\"" "~/.local/share/qpdfview/qpdfview/database" (expand-file-name annotated-file))))))
    (if (string-empty-p qpdfview-page)
        (message "No file annotated or not opened in qpdfview.")
      (if (file-equal-p (buffer-file-name) bibtex-completion-notes-path)
          (org-entry-put nil "NOTER_PAGE" (string-trim qpdfview-page))
        (org-entry-put nil "annotated_page" (string-trim qpdfview-page))))))
;; ==========swint-qpdfview-annotated=========
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
