;;; Kill subdir
;; ===============Kill subdir================
;;;###autoload
(defun dired-kill-and-next-subdir ()
  (interactive)
  (let* ((subdir-name (dired-current-directory))
         (parent-dir  (file-name-directory (directory-file-name subdir-name)))
         (search-term (concat " " (file-basename subdir-name))))
    (dired-kill-subdir)
    (dired-goto-subdir parent-dir)
    (search-forward search-term)))
;; ===============Kill subdir================
;;; helm-dired-current-file
;; ==========helm-dired-current-file=========
;;;###autoload
(defun helm-dired-current-file ()
  (interactive)
  (let ((swint-dired-current-file (or (dired-get-filename nil t)
                                      (expand-file-name default-directory))))
    (if (file-directory-p swint-dired-current-file)
        (helm-find-files-1 (file-name-as-directory swint-dired-current-file))
      (helm-find-files-1 (expand-file-name default-directory)
                         (if helm-ff-transformer-show-only-basename
                             (helm-basename swint-dired-current-file) swint-dired-current-file)))))
;; ==========helm-dired-current-file=========
;;; 跳转至dired顶部和尾部
;; ========跳转至dired顶部和尾部=============
;;;###autoload
(defun dired-beginning-of-buffer ()
  (interactive)
  (let ((move 1)
        (oldpos (point)))
    (goto-char (point-min))
    (unless dired-hide-details-mode
      (setq move (+ move 1)))
    (unless dired-omit-mode
      (setq move (+ move 2)))
    (dired-next-line move)
    (if (= oldpos (point))
        (goto-char (point-min)))))
;;;###autoload
(defun dired-end-of-buffer ()
  (interactive)
  (goto-char (point-max))
  (dired-next-line -1))
;; ========跳转至dired顶部和尾部=============
;;; webdav_sync同步文件
;; =========webdav_sync同步文件==============
;;;###autoload
(defun swint-nutstore-sync (arg)
  "Synchronization of Nutstore-sync."
  (interactive)
  (let* ((user (replace-regexp-in-string "@" "%40" (get-auth-user "Nutstore")))
         (pass (get-auth-pass "Nutstore"))
         (process
          (start-process-shell-command
           "webdav_sync" "*webdav_sync*"
           (concat "java -Dderby.system.home=" (expand-file-name "~/.webdav_sync/")
                   " -Dbe.re.http.no-compress -jar " (expand-file-name "~/.webdav_sync/webdav_sync1_1_6.jar")
                   " -r -" arg " -u https://" user ":" pass "@dav.jianguoyun.com/dav/Nutstore-sync/ -d "
                   (expand-file-name "~/Nutstore-sync/")))))
    (lexical-let ((pos (memq 'mode-line-modes mode-line-format))
                  (arg arg))
      (setcdr pos (cons (concat "Nutstore-sync " arg " ") (cdr pos)))
      (set-process-sentinel
       process
       (lambda (process signal)
         (when (memq (process-status process) '(exit signal))
           (let ((webdav_sync-process-output (with-current-buffer "*webdav_sync*"
                                               (buffer-substring-no-properties (- (point-max) 6) (point-max)))))
             (if (string-equal webdav_sync-process-output "Done.\n")
                 (message "Nutstore-sync %s done." arg)
               (message "Nutstore-sync %s failed." arg))
             (setcdr pos (remove (concat "Nutstore-sync " arg " ") (cdr pos))))))))))
;; =========webdav_sync同步文件==============
;;; unison
;; ================unison====================
;;;###autoload
(defun swint-unison-sync-backups ()
  "Sync files in ~/org/backups."
  (interactive)
  (let ((process (start-process-shell-command
                  "unison" "*unison*"
                  (concat "unison" " org-backups")))
        (pos (memq 'mode-line-modes mode-line-format)))
    (setcdr pos (cons "unison-sync-backups " (cdr pos)))
    (set-process-sentinel
     process
     (lambda (process signal)
       (when (memq (process-status process) '(exit signal))
         (let ((pos (memq 'mode-line-modes mode-line-format)))
           (message "unison-sync-backups done.")
           (setcdr pos (remove "unison-sync-backups " (cdr pos)))))))))
;; ================unison====================
;;; bypy
;; =================bypy=====================
;;;###autoload
(defun swint-bypy-sync (&optional arg)
  "Synchronization of bypy-sync."
  (interactive)
  (unless (equal (bound-and-true-p pyvenv-virtual-env-name) "bypy")
    (pyvenv-activate (format "%s/%s" (pyvenv-workon-home) "bypy")))
  (let* ((localdir (expand-file-name "~/Bypy"))
         (process
          (start-process-shell-command
           "bypy_sync" "*bypy_sync*"
           (concat "bypy sync" (if arg (concat "up " localdir " /")
                                 (concat "down " "/ " localdir))
                   " true")))
         (pos (memq 'mode-line-modes mode-line-format)))
    (setcdr pos (cons "bypy-sync " (cdr pos)))
    (set-process-sentinel
     process
     (lambda (process signal)
       (when (memq (process-status process) '(exit signal))
         (let ((pos (memq 'mode-line-modes mode-line-format)))
           (message "bypy-sync done.")
           (setcdr pos (remove "bypy-sync " (cdr pos)))))))))
;; =================bypy=====================
;;; OneDrive
;; ===============OneDrive===================
;;;###autoload
(defun swint-onedrive-sync (&optional arg)
  "Synchronization of OneDrive-sync."
  (interactive)
  (let* ((localdir (expand-file-name "~/OneDrive/rclone"))
         (remotedir "OneDrive:/rclone")
         (process
          (start-process-shell-command
           "OneDrive_sync" "*OneDrive_sync*"
           (cond ((equal arg "down") (concat "rclone sync " remotedir " " localdir))
                 ((equal arg "up") (concat "rclone sync " localdir " " remotedir))
                 ((equal arg "bi") "onedrive"))))
         (pos (memq 'mode-line-modes mode-line-format)))
    (setcdr pos (cons "OneDrive-sync " (cdr pos)))
    (set-process-sentinel
     process
     (lambda (process signal)
       (when (memq (process-status process) '(exit signal))
         (let ((pos (memq 'mode-line-modes mode-line-format)))
           (message "OneDrive-sync done.")
           (setcdr pos (remove "OneDrive-sync " (cdr pos)))))))))
;; ===============OneDrive===================
;;; totalcmd
;; ===============totalcmd===================
;;;###autoload
(defun tc-open-default-directory ()
  (interactive)
  (start-process-shell-command
   "tc" "*tc*"
   (concat "wine "
           "~/.wine/drive_c/totalcmd/TOTALCMD.EXE /O /T z:"
           (replace-regexp-in-string " " "\\\\ " (expand-file-name default-directory))))
  (let ((default-directory
          "~/.wine/drive_c/Program Files/viatc/"))
    (start-process-shell-command
     "viatc" "*viatc*"
     "wine viatc.exe")))
;;;###autoload
(defun tc-lister-open-file ()
  (interactive)
  (let ((string-to-escape "\\( \\|(\\|)\\|\\[\\|\\]\\|{\\|}\\)"))
    (cl-flet ((escape-local (x)
                            (replace-regexp-in-string string-to-escape
                                                      "\\\\\\1" x)))
      (start-process-shell-command
       "tc" "*tc*"
       (concat "wine "
               "~/.wine/drive_c/totalcmd/TOTALCMD.EXE /O /T /S=L z:"
               (escape-local (expand-file-name (dired-get-filename))))))))
;; ===============totalcmd===================
;;; 默认程序打开文件
;; =============默认程序打开文件=============
;;;###autoload
(defun dired-async-shell-command-on-files ()
  (interactive)
  (let ((marked-files (dired-get-marked-files)))
    (let ((inhibit-message t))
      (dired-unmark-all-files ?*))
    (mapc #'dired-async-shell-command marked-files)))
;;;###autoload
(defun dired-async-shell-command (file)
  (interactive)
  (let ((file-exten (downcase (or (file-name-extension file) "None"))))
    (let* ((wine-p (member file-exten '("doc" "docx" "xls" "xlsx" "ppt" "pptx" "dwg" "dxf" "caj" "nh" "kdh")))
           (command (cdr (assoc file-exten file-extension-app-alist)))
           (default-directory (or (if wine-p (ignore-errors (expand-file-name (file-name-directory file))))
                                  default-directory)))
      (if command
          (start-process "Shell" nil shell-file-name shell-command-switch
                         (concat command " " "\""
                                 (if wine-p
                                     (file-name-nondirectory file)
                                   file)
                                 "\""))
        (message "No command for \"%s\"" (file-name-nondirectory file))))))
;; =============默认程序打开文件=============
;;; 在当前目录下打开urxvt
;; =========在当前目录下打开urxvt============
;;;###autoload
(defun urxvt-default-directory (&optional arg)
  (interactive "P")
  (start-process "Urxvt" nil shell-file-name shell-command-switch
                 (concat "$(tabbed -c -d > /tmp/tabbed.xid);urxvt -pe default,-tabbed -embed $(</tmp/tabbed.xid) -cd "
                         "\"" (expand-file-name default-directory) "\""
                         ;; 启动bash终端同时开启virtualenv。
                         (when arg (require 'pyvenv)
                               (let ((pyvenv-virtual-env-for-urxvt
                                      (or pyvenv-virtual-env
                                          (file-name-as-directory
                                           (format "%s/%s" (pyvenv-workon-home)
                                                   (completing-read "Work on: " (pyvenv-virtualenv-list)
                                                                    nil t nil 'pyvenv-workon-history nil nil))))))
                                 (concat " -e zsh" " -is eval \"source "
                                         pyvenv-virtual-env-for-urxvt "bin/activate;\""))))))
;; =========在当前目录下打开urxvt============
;;; swint-dired-converter
;; ============swint-dired-converter=========
;;;###autoload
(defun swint-dired-converter ()
  (interactive)
  (let ((file-list (if (eq major-mode 'dired-mode)
                       (dired-get-marked-files)
                     (list (buffer-file-name))))
        (engine (helm-comp-read "Engine: " (list "pandoc" "libreoffice" "pdftk" "ODAFileConverter")
                                :buffer "*helm dired converter-swint*")))

    (cond ((string= engine "pandoc")
           (let ((output-format (read-string "Output format: ")))
             (cl-loop for x in file-list
                      do (shell-command (concat "pandoc -o " (file-name-base x)
                                                "." output-format " " (file-name-nondirectory x))))))
          ((string= engine "libreoffice")
           (let ((output-format (read-string "Output format: ")))
             (cl-loop for x in file-list
                      do (shell-command (concat "libreoffice --headless --convert-to "
                                                output-format
                                                (when (equal output-format "csv")
                                                  " --infilter=CSV:44,34,76,1")
                                                " " (file-name-nondirectory x))))))
          ((string= engine "pdftk")
           (let ((output-args (read-string "Pdftk args(1-2west 4 5-end): ")))
             (cl-loop for x in file-list
                      do (shell-command (concat "pdftk " (file-name-nondirectory x) " cat "
                                                output-args " output " (concat (file-name-base x) "-new.pdf"))))))
          ((string= engine "ODAFileConverter")
           (let ((output-version (helm-comp-read "Output version: "
                                                 (list "ACAD9" "ACAD10" "ACAD12" "ACAD13" "ACAD14" "ACAD2000" "ACAD2004" "ACAD2007" "ACAD2010")
                                                 :buffer "*helm dired converter-swint*"))
                 (output-type (helm-comp-read "Output type: "
                                              (list "DWG" "DXF" "DXB")
                                              :buffer "*helm dired converter-swint*"))
                 (all-files (y-or-n-p "All files?")))
             (cl-loop for x in file-list
                      do (shell-command (concat (format "ODAFileConverter ./ ./%s-%s %s %s 0 1 " output-type output-version output-version output-type)
                                                (unless all-files
                                                  (file-name-nondirectory x))))))))))
;; ============swint-dired-converter=========
;;; dired-view-file-or-dir
;; ==========dired-view-file-or-dir==========
(defconst +kilobyte+ 1024.0)
(defconst +megabyte+ (* 1024 1024.0))
(defconst +gigabyte+ (* 1024 1024 1024.0))
(defconst +terabyte+ (* 1024 1024 1024.0 1024.0))
(defun txm-format-file-size (size)
  "Return string with formatted file size."
  (cl-flet ((float-to-string (x)
                             (format "%.2f" x)))
    (cond ((< size +kilobyte+)
           (concat (number-to-string size) " bytes"))
          ((< size +megabyte+)
           (concat (float-to-string (/ size +kilobyte+)) " Kb"))
          ((< size +gigabyte+)
           (concat (float-to-string (/ size +megabyte+)) " Mb"))
          ((< size +terabyte+)
           (concat (float-to-string (/ size +gigabyte+)) " Gb"))
          (t "Unknown size"))))
(defun txm-file-or-dir-size (path)
  "Calculate size of the directory or file using Unix 'wc' tool."
  (message (concat "Processing " path "..."))
  (let ((du-command
         (if (eq system-type 'darwin)
             "/opt/local/bin/gdu"
           "du")))
    (with-temp-buffer
      (if (zerop (apply 'call-process
                        du-command
                        (list nil t nil "-s" "-b" path)))
          ;; Possibly more complicated processing here.
          (string-to-number (car (split-string (buffer-string))))
        -1))))
;;;###autoload
(defun txm-dired-view-file-or-dir ()
  "Replacement for dired-view-file-or-dir.
If called on file - view it, on directory - calculate its size
Assuming .. and . is a current directory (like in FAR)"
  (interactive)
  (let ((file (dired-get-file-for-visit)))
    (if (file-directory-p file)
        (let ((filename (car (last (split-string file "/")))))
          (when (or (string= filename "..")
                    (string= filename "."))
            (setq file (dired-current-directory)))
          (let ((size (txm-file-or-dir-size file)))
            (if (/= size -1 )
                (message (concat (txm-format-file-size size)
                                 " in "
                                 filename
                                 " ("
                                 (number-to-string size)
                                 " bytes)"))
              (message (concat "Cannot determine size of " filename)))))
      (view-file file))))
;; ==========dired-view-file-or-dir==========
;;; swint-dired-rsync/unison
;; =========swint-dired-rsync/unison=========
;;;###autoload
(defun swint-dired-rsync/unison (action)
  (interactive)
  (let ((remote (completing-read "Remote repo: "
                                 (split-string
                                  (shell-command-to-string
                                   "cat ~/.ssh/config | grep \"^Host \" | awk '{print $2}'"))))
        (path (abbreviate-file-name default-directory))
        (is-sync (equal action "sync"))
        (is-push (equal action "push"))
        (is-pull (equal action "pull"))
        (string-to-escape "\\( \\|(\\|)\\|\\[\\|\\]\\|{\\|}\\)")
        rsync/unison-command)
    ;; 对于rsync，escape本地路径用\，远程路径用\\\。
    (cl-flet ((escape-local (x)
                            (replace-regexp-in-string string-to-escape
                                                      "\\\\\\1" x))
              (escape-remote (x)
                             (replace-regexp-in-string string-to-escape
                                                       "\\\\\\\\\\\\\\1" x)))
      (if is-sync
          (let ((unison-path (escape-local (file-truename path))))
            (setq rsync/unison-command (concat "unison -batch -confirmbigdel=false " unison-path
                                               " ssh://" remote "//" unison-path)))
        (let ((files (cond (is-push
                            (cl-loop for f in (dired-get-marked-files)
                                     collect (escape-local f)))
                           (is-pull
                            (let (remote-files)
                              (if current-prefix-arg
                                  (counsel-read-file-for-rsync 'remote-files (format "/ssh:%s:~/" remote))
                                (setq remote-files
                                      (helm-comp-read "Remote files: "
                                                      (split-string (shell-command-to-string
                                                                     ;; 连接remote列出path下文件绝对路径，并不显示错误信息。
                                                                     (format "ssh %s '(cd %s && ls -A | sed \"s:^:`pwd`/:\") 2>/dev/null'"
                                                                             remote (escape-local path))) "\n")
                                                      :marked-candidates t
                                                      :buffer (format "*helm rsync/unison %s*" remote))))
                              (cl-loop for f in remote-files
                                       collect (concat remote ":" (escape-remote (if (directory-name-p f)
                                                                                     (directory-file-name f)
                                                                                   f))))))))
              (dest (cond (is-pull (escape-local path))
                          (is-push
                           (let (remote-files)
                             (escape-remote (if current-prefix-arg
                                                (directory-file-name
                                                 (car (split-string (counsel-read-file-for-rsync
                                                                     'remote-files (format "/ssh:%s:~/" remote)) "/ssh:" t)))
                                              (concat remote ":" path))))))))
          (setq rsync/unison-command "rsync -arv --progress ")
          (dolist (file files)
            (setq rsync/unison-command
                  (concat rsync/unison-command file " ")))
          (setq rsync/unison-command (concat rsync/unison-command dest)))))
    (let ((process (start-process-shell-command "rsync/unison" "*rsync/unison*" rsync/unison-command)))
      (lexical-let ((pos (memq 'mode-line-modes mode-line-format))
                    (mode-string action))
        (setcdr pos (cons (concat "Rsync/Unison " mode-string " ") (cdr pos)))
        (set-process-sentinel
         process
         (lambda (process signal)
           (when (memq (process-status process) '(exit signal))
             (message "Rsync/Unison %s done." mode-string)
             (setcdr pos (remove (concat "Rsync/Unison " mode-string " ") (cdr pos))))))))))
;; =========swint-dired-rsync/unison=========
