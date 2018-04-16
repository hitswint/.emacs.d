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
            (car (last (split-string (substring-no-properties (org-get-heading) nil -2) "\\[file:") 1)))))
;;;###autoload
(defun swint-org-open-at-point (&optional in-emacs)
  "Open annotated file if annotation storage file exists."
  (interactive)
  (let* ((annotated-file (swint-get-annotated-file))
         (cite-results (ignore-errors (save-excursion (org-ref-get-bibtex-key-and-file))))
         (cite-file (and (cdr cite-results) (car (bibtex-completion-find-pdf (car cite-results)))))
         (file-at-point (or annotated-file cite-file)))
    (if (and file-at-point (file-exists-p file-at-point))
        (org-open-file file-at-point in-emacs)
      (org-open-at-point in-emacs))))
;;;###autoload
(defun swint-org-open-at-point-with-apps ()
  (interactive)
  (let ((org-file-apps '(("\\.pdf\\'" . "llpp %s")
                         ("\\.djvu\\'" . "llpp %s")
                         ("\\.png\\'" . "feh.sh %s")
                         ("\\.jpg\\'" . "feh.sh %s")
                         ("\\.bmp\\'" . "feh.sh %s")
                         ("\\.jpeg\\'" . "feh.sh %s")
                         ("\\.eps\\'" . "gv %s")
                         ("\\.ps\\'" . "gv %s")
                         ("\\.rmvb\\'" . "mplayer %s")
                         ("\\.rm\\'" . "mplayer %s")
                         ("\\.mp4\\'" . "mplayer %s")
                         ("\\.avi\\'" . "mplayer %s")
                         ("\\.flv\\'" . "mplayer %s")
                         ("\\.f4v\\'" . "mplayer %s")
                         ("\\.mpg\\'" . "mplayer %s")
                         ("\\.mkv\\'" . "mplayer %s")
                         ("\\.3gp\\'" . "mplayer %s")
                         ("\\.wmv\\'" . "mplayer %s")
                         ("\\.mov\\'" . "mplayer %s")
                         ("\\.dat\\'" . "mplayer %s")
                         ("\\.asf\\'" . "mplayer %s")
                         ("\\.mpeg\\'" . "mplayer %s")
                         ("\\.wma\\'" . "mplayer %s")
                         ("\\.gif\\'" . "mplayer %s")
                         ("\\.doc\\'" . "libreoffice %s")
                         ("\\.ppt\\'" . "libreoffice %s")
                         ("\\.xls\\'" . "libreoffice %s")
                         ("\\.ods\\'" . "libreoffice %s")
                         ("\\.odt\\'" . "libreoffice %s")
                         ("\\.docx\\'" . "libreoffice %s")
                         ("\\.pptx\\'" . "libreoffice %s")
                         ("\\.xlsx\\'" . "libreoffice %s")
                         ("\\.dxf\\'" . "librecad %s")
                         ("\\.html\\'" . "firefox %s")
                         ("\\.htm\\'" . "firefox %s")
                         )))
    (swint-org-open-at-point)))
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
(setq org-mobile-files org-agenda-files)
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
