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
      (helm-find-files-1 (file-name-directory swint-dired-current-file)
                         (if helm-ff-transformer-show-only-basename
                             (helm-basename swint-dired-current-file)
                           swint-dired-current-file)))))
;; ==========helm-dired-current-file=========
;;; 跳转至dired顶部和尾部
;; ========跳转至dired顶部和尾部=============
;;;###autoload
(defun dired-beginning-of-buffer ()
  (interactive)
  (let ((move 1)
        (oldpos (point)))
    (goto-char (point-min))
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
(defun swint-webdav-sync (arg)
  "Synchronization of webdav-sync."
  (interactive)
  (with-current-buffer (get-buffer-create "*webdav_sync*")
    (goto-char (point-max))
    (insert (format "=====%s=====\n" arg)))
  (let* (;; Nutstore用户名为邮箱，需替代@
         ;; (user (replace-regexp-in-string "@" "%40" (get-auth-user "Nutstore-sync")))
         (user_orig (split-string (get-auth-user "webdav-sync") "@"))
         (user (car user_orig))
         (host (cadr user_orig))
         (pass (get-auth-pass "webdav-sync"))
         (port (get-auth-port "webdav-sync"))
         (process
          (start-process-shell-command
           "webdav_sync" "*webdav_sync*"
           (concat "java -Dderby.system.home=" (expand-file-name "~/.webdav_sync/")
                   " -Dbe.re.http.no-compress -jar " (expand-file-name "~/.webdav_sync/webdav_sync1_1_6.jar")
                   " -r -" arg " -u https://" user ":" pass "@" host ":" port "/dav/webdav-sync/ -d "
                   (expand-file-name "~/webdav-sync/")))))
    (lexical-let ((pos (memq 'mode-line-modes mode-line-format))
                  (arg arg))
      (setcdr pos (cons (concat "webdav-sync " arg " ") (cdr pos)))
      (set-process-sentinel
       process
       (lambda (process signal)
         (when (memq (process-status process) '(exit signal))
           (let ((webdav_sync-process-output (with-current-buffer "*webdav_sync*"
                                               (goto-char (1- (point-max)))
                                               (save-excursion (re-search-backward (format "=====%s=====" arg) nil t 1)
                                                               (buffer-substring-no-properties (1+ (match-end 0)) (1- (point-max)))))))
             (message "%s" webdav_sync-process-output)
             (setcdr pos (remove (concat "webdav-sync " arg " ") (cdr pos))))))))))
;; =========webdav_sync同步文件==============
;;; unison
;; ================unison====================
;;;###autoload
(defun swint-unison-sync-backups ()
  "Sync files in ~/org/backups."
  (interactive)
  (let ((process (start-process-shell-command
                  "unison" "*unison*"
                  (concat "unison" " N5095_backup")))
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
  (pyvenv-activate-py3)
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
;;; thunar/totalcmd
;; ============thunar/totalcmd===============
;;;###autoload
(defun thunar-open-default-directory ()
  (interactive)
  (let ((curr-dir (if-let ((curr-line (dired-get-filename nil t)))
                      (file-name-directory curr-line)
                    default-directory)))
    (start-process-shell-command
     "thunar" "*thunar*"
     (concat "thunar " curr-dir))))
;;;###autoload
(defun tc-open-default-directory ()
  (interactive)
  (start-process-shell-command
   "tc" "*tc*"
   (concat "wine "
           "~/.wine/drive_c/totalcmd/TOTALCMD.EXE /O /T z:"
           (replace-regexp-in-string " " "\\\\ " (expand-file-name default-directory))))
  (let ((default-directory "~/.wine/drive_c/Program Files/viatc/"))
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
;; ============thunar/totalcmd===============
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
  (let* ((file-exten (ignore-errors (downcase (file-name-extension file))))
         (wine-p (member file-exten '("doc" "docx" "xls" "xlsx" "ppt" "pptx" "dwg" "dxf" "caj" "nh" "kdh")))
         (command (if (file-directory-p file) "thunar"
                    (concat (assoc-default file-exten file-extension-app-alist)
                            (when (and (or (member file-exten file_video_exts)
                                           (member file-exten file_image_exts))
                                       (not (equal dired-listing-switches dired-actual-switches)))
                              (concat " -" (substring dired-actual-switches 32))))))
         (default-directory (or (if wine-p (ignore-errors (expand-file-name (file-name-directory file))))
                                (helm-current-directory))))
    (if (string-empty-p command)
        (message "No command for \"%s\"" (file-name-nondirectory file))
      (start-process "Shell" nil shell-file-name shell-command-switch
                     (concat command " " "\""
                             (if wine-p
                                 (file-name-nondirectory file)
                               file)
                             "\"")))))
;;;###autoload
(defun dired-xdg-open ()
  (interactive)
  (let ((marked-files (dired-get-marked-files)))
    (let ((inhibit-message t))
      (dired-unmark-all-files ?*))
    (mapc #'(lambda (file)
              (start-process "dired-xdg-open" nil "xdg-open"
                             (file-truename file)))
          marked-files)))
;; =============默认程序打开文件=============
;;; 在当前目录下打开urxvt
;; =========在当前目录下打开urxvt============
;;;###autoload
(defun urxvt-default-directory (&optional arg)
  (interactive "P")
  (let ((curr-dir (if-let ((curr-line (dired-get-filename nil t)))
                      (file-name-directory curr-line)
                    default-directory)))
    (start-process "Urxvt" nil shell-file-name shell-command-switch
                   (concat "$(tabbed -c -d > /tmp/tabbed.xid);urxvt -pe default,-tabbed -embed $(</tmp/tabbed.xid) -cd "
                           "\"" (expand-file-name curr-dir) "\""
                           ;; 启动bash终端同时开启virtualenv
                           (when arg
                             (require 'pyvenv)
                             (let ((pyvenv-virtual-env-for-urxvt
                                    (or pyvenv-virtual-env
                                        (file-name-as-directory
                                         (format "%s/%s" (pyvenv-workon-home)
                                                 (completing-read "Work on: " (pyvenv-virtualenv-list)
                                                                  nil t nil 'pyvenv-workon-history nil nil))))))
                               (concat " -e zsh" " -is eval \"source "
                                       pyvenv-virtual-env-for-urxvt "bin/activate;\"")))))))
;;;###autoload
(defun openincd-directory ()
  (with-selected-frame (selected-frame)
    (with-current-buffer (car (buffer-list))
      (let ((curr-dir (if-let ((curr-line (dired-get-filename nil t)))
                          (file-name-directory curr-line)
                        default-directory)))
        (expand-file-name curr-dir)))))
;;;###autoload
(defun openincd-file ()
  (with-selected-frame (selected-frame)
    (with-current-buffer (car (buffer-list))
      (expand-file-name (or (dired-get-filename nil t)
                            (buffer-file-name))))))
;; =========在当前目录下打开urxvt============
;;; swint-dired-converter
;; ============swint-dired-converter=========
;;;###autoload
(defun swint-dired-converter ()
  (interactive)
  (let ((file-list (if (eq major-mode 'dired-mode)
                       (dired-get-marked-files)
                     (list (buffer-file-name))))
        (engine (helm-comp-read "Engine: " (list "pandoc" "libreoffice" "pdftk" "convert" "pdftoppm" "ODAFileConverter" "xlsx2csv" "caj2pdf" "img2pdf" "pptx2md" "numpy-stl")
                                :buffer "*helm dired converter-swint*"))
        (string-to-escape "\\( \\|(\\|)\\|\\[\\|\\]\\|{\\|}\\)")
        (default-directory (helm-current-directory)))
    (cl-flet ((escape-local (x)
                (replace-regexp-in-string string-to-escape
                                          "\\\\\\1" x)))
      (cond ((string= engine "pandoc")
             (let ((output-format (ivy-read "Output format: " '(org md docx pdf))))
               (cl-loop for x in file-list
                        do (let* ((filename (escape-local (file-name-nondirectory x)))
                                  (options (concat (when (string= (file-name-extension filename) "docx")
                                                     "--extract-media ./pic ")
                                                   (cond
                                                    ((member output-format '("org" "md"))
                                                     "--wrap=preserve")
                                                    ((equal output-format "docx")
                                                     (format "--reference-doc=%s --toc --toc-depth=3 --citeproc --bibliography=%s --csl=%s --metadata reference-section-title:\"%s\" --metadata=figureTitle:图 --metadata=figPrefix:图 --metadata=tableTitle:表 --metadata=tblPrefix:表 --metadata=eqnPrefix:式 --metadata=autoEqnLabels:true --metadata=titleDelim: --metadata=chapters:false --metadata=chapDelim:- --filter=pandoc-crossref"
                                                             (expand-file-name "~/.pandoc/reference.docx")
                                                             (expand-file-name "~/.bib/Zotero.bib")
                                                             (expand-file-name "~/Zotero/styles/china-national-standard-gb-t-7714-2015-numeric.csl")
                                                             "参考文献"))
                                                    ((equal output-format "pdf")
                                                     "--pdf-engine=xelatex --template=eisvogel.latex --toc -V toc-title=\"目录\" --number-sections --include-in-header ~/.pandoc/templates/chapter_break.tex -V geometry:a4paper -V geometry:margin=2cm -V CJKmainfont=\"SimSun\" -V mainfont=\"Times New Roman\" -V monofont=\"SimSun\"")))))
                             (message "%s" options)
                             (shell-command (concat "pandoc " options " -o " (file-name-base filename) "." output-format " " filename))))))
            ((string= engine "libreoffice")
             (let ((output-format (read-string "Output format: ")))
               (cl-loop for x in file-list
                        do (let* ((filename (escape-local (file-name-nondirectory x)))
                                  (file-extension (ignore-errors (downcase (file-name-extension filename)))))
                             (shell-command (concat "libreoffice --headless --convert-to "
                                                    output-format
                                                    (when (or (equal output-format "csv")
                                                              (equal file-extension "csv"))
                                                      " --infilter=CSV:44,34,76,1")
                                                    " " filename))))))
            ((string= engine "pdftk")
             (let* ((file-list-ordered (if (> (length file-list) 1)
                                           (helm-comp-read "Select files with order: " file-list
                                                           :marked-candidates t
                                                           :buffer "*helm dired converter-swint*")
                                         file-list))
                    (filename-list (cl-loop for x in file-list-ordered
                                            collect (file-name-nondirectory x)))
                    (file-args (cl-loop for x in filename-list
                                        for y from ?A to ?Z
                                        collect (concat (char-to-string y) "=" (escape-local x))))
                    (page-args (cl-loop for x in filename-list
                                        for y from ?A to ?Z
                                        collect (mapconcat (lambda (arg) (concat (char-to-string y) arg))
                                                           (split-string (read-string (concat x " (1-2west 4 5-end): ") nil nil "1-end") " " t)
                                                           " ")))
                    (output-file (mapconcat (lambda (arg) (concat (file-name-base (car arg)) "_" (cdr arg)))
                                            (-zip-pair filename-list page-args) ".")))
               (shell-command (concat "pdftk " (mapconcat 'identity file-args " ") " cat " (mapconcat 'identity page-args " ") " output \""
                                      ;; 输出文件可能过长，Linux中文件名最长允许255个字符(Byte)，UTF-8编码中每个汉字为3个字符
                                      (if (> (string-bytes output-file) 250)
                                          (if (multibyte-string-p output-file)
                                              ;; 取前250个字符，末尾可能残存1/2个不完整字符
                                              (substring (string-as-multibyte (s-left 250 (string-as-unibyte output-file))) 0 -2)
                                            (s-left 250 output-file))
                                        output-file)
                                      ".pdf\""))))
            ((string= engine "convert")
             (let ((output-format (read-string "Output format: "))
                   (input-string (mapconcat (lambda (arg) (escape-local (file-name-nondirectory arg))) file-list " ")))
               (shell-command (read-string "Commands: " (concat "convert " input-string " " (make-temp-name (concat (escape-local (file-name-base (car file-list))) "_")) "." output-format)))))
            ((string= engine "pdftoppm")
             (let ((output-format (read-string "Output image format(png/jpeg/tiff/mono): ")))
               (cl-loop for x in file-list
                        do (let ((filename (escape-local (file-name-nondirectory x))))
                             (shell-command (read-string "Commands: " (concat "pdftoppm -" output-format " -r 500 " filename " " filename)))))))
            ((string= engine "ODAFileConverter")  ;https://www.opendesign.com/guestfiles/oda_file_converter
             (let ((output-version (helm-comp-read "Output version: "
                                                   (list "ACAD9" "ACAD10" "ACAD12" "ACAD13" "ACAD14" "ACAD2000" "ACAD2004" "ACAD2007" "ACAD2010")
                                                   :buffer "*helm dired converter-swint*"))
                   (output-type (helm-comp-read "Output type: "
                                                (list "DWG" "DXF" "DXB")
                                                :buffer "*helm dired converter-swint*"))
                   (all-files (y-or-n-p "All files?")))
               (cl-loop for x in file-list
                        do (let ((filename (escape-local (file-name-nondirectory x))))
                             (shell-command (concat (format "ODAFileConverter ./ ./%s-%s %s %s 0 1 " output-type output-version output-version output-type)
                                                    (unless all-files filename)))))))
            ((string= engine "xlsx2csv")  ;https://github.com/dilshod/xlsx2csv
             (cl-loop for x in file-list
                      do (let* ((filename (escape-local (file-name-nondirectory x)))
                                (file-extension (ignore-errors (downcase (file-name-extension filename)))))
                           (cond ((equal file-extension "xlsx")
                                  (shell-command (concat "xlsx2csv -i -a " filename " " (file-name-sans-extension filename))))
                                 ((equal file-extension "xls")
                                  ;; 存在中文乱码问题，需设置charset，自带charset位于/usr/share/catdoc/下，但无中文支持
                                  (shell-command (concat "xls2csv " filename " > " (file-name-sans-extension filename) ".csv")))))))
            ((string= engine "caj2pdf")  ;https://github.com/caj2pdf/caj2pdf
             (let ((cmd-path (expand-file-name "repos/caj2pdf/caj2pdf" user-emacs-directory))
                   (default-directory (expand-file-name "repos/caj2pdf" user-emacs-directory)))
               (cl-loop for x in file-list
                        do (let ((filename (escape-local x)))
                             (shell-command (concat cmd-path " convert " filename " -o " filename ".pdf"))))))
            ((string= engine "img2pdf")  ;https://gitlab.mister-muffin.de/josch/img2pdf
             (let ((input-string (mapconcat (lambda (arg) (escape-local (file-name-nondirectory arg))) file-list " ")))
               (shell-command (read-string "Commands: " (concat "img2pdf -o " (make-temp-name (concat (escape-local (file-name-base (car file-list))) "_")) ".pdf " input-string )))))
            ((string= engine "pptx2md")  ;https://github.com/ssine/pptx2md
             (let ((options (read-string "Options: " "--disable-color --disable-escaping --disable-image")))
               (cl-loop for x in file-list
                        do (let ((filename (escape-local (file-name-nondirectory x))))
                             (shell-command (concat "pptx2md -o " (file-name-sans-extension filename) ".md " options " " filename))))))
            ((string= engine "numpy-stl")
             (if (> (length file-list) 1)
                 (shell-command (concat "cat " (mapconcat (lambda (arg) (concat "\"" (file-name-nondirectory arg) "\"")) file-list " ")
                                        " >> combined.stl"))
               (shell-command (format "stl_split.sh %s" (car file-list)))))))))
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
  (let* ((file-list (dired-get-marked-files))
         (total-size (cl-loop for x in file-list
                              sum (txm-file-or-dir-size x))))

    (if (/= total-size -1)
        (message (concat (mapconcat 'file-name-nondirectory file-list "\n")
                         "\n" (number-to-string (length file-list)) " files, "
                         (txm-format-file-size total-size) " in total.\n"))
      (message "Cannot determine size."))))
;; ==========dired-view-file-or-dir==========
;;; swint-dired-rsync/unison
;; =========swint-dired-rsync/unison=========
;;;###autoload
(defun swint-dired-rsync/unison (action)
  (interactive)
  (let ((remote (helm-select-host))
        (path (abbreviate-file-name (helm-current-directory)))
        (is-sync (equal action "sync"))
        (is-push (equal action "push"))
        (is-pull (equal action "pull"))
        (string-to-escape "\\( \\|(\\|)\\|\\[\\|\\]\\|{\\|}\\)")
        rsync/unison-command)
    ;; 对于rsync，escape本地路径用\，远程路径用\\\
    (cl-flet ((escape-local (x)
                (replace-regexp-in-string string-to-escape
                                          "\\\\\\1" x))
              (escape-remote (x)
                (replace-regexp-in-string string-to-escape
                                          "\\\\\\\\\\\\\\1" x)))
      (if is-sync
          (let* ((path-abs (file-truename path))
                 (proj (projectile-project-root path-abs))
                 (unison-path (escape-local (or proj path-abs)))
                 (unison-cmd (if (and proj (file-exists-p (expand-file-name ".gitignore" proj)))
                                 "unison-gitignore"
                               "unison -batch -confirmbigdel=false -ignorearchives")))
            (setq rsync/unison-command (concat unison-cmd " " unison-path
                                               " ssh://" remote "//" unison-path)))
        (let ((files (cond (is-push
                            (cl-loop for f in (or (dired-get-marked-files) (list (buffer-file-name)))
                                     collect (escape-local f)))
                           (is-pull
                            (let (remote-files)
                              (if current-prefix-arg
                                  (counsel-read-file-for-rsync 'remote-files (format "/ssh:%s:~/" remote))
                                (setq remote-files
                                      (helm-comp-read "Remote files: "
                                                      (split-string (shell-command-to-string
                                                                     ;; 连接remote列出path下文件绝对路径，并不显示错误信息
                                                                     (format "ssh %s '(cd %s && ls -A | sed \"s:^:`pwd`/:\") 2>/dev/null'"
                                                                             remote (escape-local path)))
                                                                    "\n")
                                                      :marked-candidates t
                                                      :buffer (format "*helm rsync/unison %s*" remote))))
                              (cl-loop for f in remote-files
                                       collect (concat remote ":" (escape-local (if (directory-name-p f)
                                                                                    (directory-file-name f)
                                                                                  f))))))))
              (dest (cond (is-pull (escape-local path))
                          (is-push
                           (let (remote-files)
                             (escape-local (if current-prefix-arg
                                               (directory-file-name
                                                (car (split-string (counsel-read-file-for-rsync
                                                                    'remote-files (format "/ssh:%s:~/" remote))
                                                                   "/ssh:" t)))
                                             (concat remote ":" path))))))))
          (setq rsync/unison-command "rsync -arv --progress ")
          (dolist (file files)
            (setq rsync/unison-command
                  (concat rsync/unison-command file " ")))
          (setq rsync/unison-command (concat rsync/unison-command dest)))))
    (with-current-buffer (get-buffer-create "*rsync/unison*")
      (let (org-fontify-emphasized-text)
        (org-mode)
        (goto-char (point-max))
        (insert "* " rsync/unison-command "\n")))
    (let ((process (start-process-shell-command "rsync/unison" "*rsync/unison*" rsync/unison-command)))
      (lexical-let ((pos (memq 'mode-line-modes mode-line-format))
                    (mode-string action)
                    (rsync/unison-command rsync/unison-command)
                    (remote remote))
        (setcdr pos (cons (concat "Rsync/Unison " mode-string " ") (cdr pos)))
        (set-process-sentinel
         process
         (lambda (process signal)
           (when (memq (process-status process) '(exit signal))
             (message "Rsync/Unison %s %s done.\nCommand: %s"
                      (propertize remote 'face 'font-lock-function-name-face)
                      (propertize mode-string 'face 'font-lock-type-face)
                      rsync/unison-command)
             (setcdr pos (remove (concat "Rsync/Unison " mode-string " ") (cdr pos))))))))))
;; =========swint-dired-rsync/unison=========
