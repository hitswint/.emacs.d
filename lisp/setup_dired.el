;;; dired
;; ==================dired=====================
(use-package dired
  :config
  (use-package dired-x)
  (use-package dired-details
    :config
    (dired-details-install)
    (setq dired-omit-verbose nil)
    (setq dired-omit-size-limit nil)
    (add-hook 'dired-mode-hook (lambda () (dired-omit-mode)))
    (setq dired-details-hidden-string "")
    (advice-add 'dired-details-show :after #'(lambda () (dired-hide-details-mode 0)))
    (advice-add 'dired-details-hide :after #'(lambda () (dired-hide-details-mode 1))))
  (use-package diredful
    :config
    (diredful-mode 1))
  (setq dired-recursive-copies 'top)
  (setq dired-recursive-deletes 'always)
  (custom-set-faces '(diredp-compressed-file-suffix ((t (:foreground "#7b68ee"))) t)
                    '(diredp-ignored-file-name ((t (:foreground "#aaaaaa"))) t))
;;;; Auto-revert-mode
  ;; =============Auto-revert-mode=============
  ;; Auto refresh buffers.
  (global-auto-revert-mode 1)
  ;; Also auto refresh dired, but be quiet about it.
  (setq global-auto-revert-non-file-buffers t)
  (setq auto-revert-verbose nil)
  ;; (setq global-auto-revert-ignore-modes '(dired-mode))
  ;; 使用dired-mode自带的auto-revert。
  (setq dired-auto-revert-buffer t)
  ;; =============Auto-revert-mode=============
;;;; setup-and-keybindings
  ;; ==========setup-and-keybindings===========
  (put 'dired-find-alternate-file 'disabled nil)
  ;; 显示文件大小。
  (setq dired-listing-switches "-alh")
  ;; 文件夹间复制。
  (setq dired-dwim-target t)
  ;; Allow editing file permissions.
  (setq wdired-allow-to-change-permissions t)
  (add-hook 'wdired-mode-hook 'undo-tree-mode)
  ;; 将dired-k--highlight-buffer加hook放在前面，使其出现在hook列表最后，以便最后生效。
  (add-hook 'dired-after-readin-hook 'dired-k--highlight-buffer)
  ;; 不折行显示。
  (add-hook 'dired-after-readin-hook '(lambda () (setq truncate-lines t)))
  (add-hook 'dired-mode-hook
            '(lambda ()
               (define-key dired-mode-map (kbd "M-=") nil)
               (define-key dired-mode-map (kbd "r") (lambda ()
                                                      (interactive)
                                                      (let ((current-directory default-directory))
                                                        (find-alternate-file "..")
                                                        (dired-goto-file (expand-file-name current-directory)))))
               (define-key dired-mode-map (kbd "i") '(lambda ()
                                                       (interactive)
                                                       (call-interactively 'dired-maybe-insert-subdir)
                                                       (revert-buffer)))
               (define-key dired-mode-map (kbd "I") '(lambda ()
                                                       (interactive)
                                                       (dired-kill-and-next-subdir)
                                                       (revert-buffer)))
               (define-key dired-mode-map (kbd "l") 'swint-org-annotate-file-current)
               (define-key dired-mode-map (kbd "L") 'org-annotate-file-current)
               (define-key dired-mode-map (kbd "C-c l") 'swint-dired-interleave)
               (smartrep-define-key dired-mode-map "C-c"
                 '(("p" . dired-k--previous-highlighted-file)
                   ("n" . dired-k--next-highlighted-file)))
               (define-key dired-mode-map (kbd "v") 'txm-dired-view-file-or-dir)
               (define-key dired-mode-map (kbd "M-RET") 'helm-dired-current-file)
               (define-key dired-mode-map (kbd "C-M-j") 'tc-lister-open-file)
               ;; 在dired对mark的多个文件内容进行查找。
               (define-key dired-mode-map (kbd "C-c C-s") 'dired-do-isearch)
               (define-key dired-mode-map (kbd "C-c C-M-s") 'dired-do-isearch-regexp)))
  ;; ==========setup-and-keybindings===========
;;;; Kill subdir
  ;; ===============Kill subdir================
  (defun file-basename (file)
    (let ((file-no-ending-slash (replace-regexp-in-string "/$" "" file)))
      (car (reverse (split-string file-no-ending-slash "/")))))
  (defun dired-kill-and-next-subdir ()
    (interactive)
    (let* ((subdir-name (dired-current-directory))
           (parent-dir  (file-name-directory (directory-file-name subdir-name)))
           (search-term (concat " " (file-basename subdir-name))))
      (dired-kill-subdir)
      (dired-goto-subdir parent-dir)
      (search-forward search-term)))
  ;; ===============Kill subdir================
;;;; helm-dired-current-file
  ;; ==========helm-dired-current-file=========
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
;;;; 默认文件夹排在最前面
  ;; =========默认文件夹排在最前面=============
  (defun sof/dired-sort ()
    "Dired sort hook to list directories first."
    (save-excursion
      (let (buffer-read-only)
        (forward-line 2)
        (sort-regexp-fields t "^.*$" "[ ]*." (point) (point-max))))
    (and (featurep 'xemacs)
         (fboundp 'dired-insert-set-properties)
         (dired-insert-set-properties (point-min) (point-max)))
    (set-buffer-modified-p nil))
  (add-hook 'dired-after-readin-hook 'sof/dired-sort)
  ;; =========默认文件夹排在最前面=============
;;;; 文件夹排序
  ;; =============文件夹排序===================
  (add-hook 'dired-mode-hook (lambda ()
                               (interactive)
                               (make-local-variable 'dired-sort-map)
                               (setq dired-sort-map (make-sparse-keymap))
                               (define-key dired-mode-map "s" dired-sort-map)
                               (define-key dired-sort-map "s"
                                 '(lambda () ;"sort by Size"
                                    (interactive)
                                    (dired-sort-other (concat dired-listing-switches "S"))))
                               (define-key dired-sort-map "x"
                                 '(lambda () ;"sort by eXtension"
                                    (interactive)
                                    (dired-sort-other (concat dired-listing-switches "X"))))
                               (define-key dired-sort-map "t"
                                 '(lambda () ;"sort by Time"
                                    (interactive)
                                    (dired-sort-other (concat dired-listing-switches "t"))))
                               (define-key dired-sort-map "n"
                                 '(lambda () ;"sort by Name"
                                    (interactive)
                                    (dired-sort-other (concat dired-listing-switches ""))))))
  ;; =============文件夹排序===================
;;;; 跳转至dired顶部和尾部
  ;; ========跳转至dired顶部和尾部=============
  (defadvice dired-next-line (around dired-next-line+ activate)
    "Replace current buffer if file is a directory."
    ad-do-it
    (while (and  (not  (eobp)) (not ad-return-value))
      (forward-line)
      (setq ad-return-value(dired-move-to-filename)))
    (when (eobp)
      (forward-line -1)
      (setq ad-return-value(dired-move-to-filename))))
  (defadvice dired-previous-line (around dired-previous-line+ activate)
    "Replace current buffer if file is a directory."
    ad-do-it
    (while (and  (not  (bobp)) (not ad-return-value))
      (forward-line -1)
      (setq ad-return-value(dired-move-to-filename)))
    (when (bobp)
      (call-interactively 'dired-next-line)))
  (defun dired-beginning-of-buffer ()
    (interactive)
    (let ((move 1)
          (oldpos (point)))
      (beginning-of-buffer)
      (unless dired-hide-details-mode
        (setq move (+ move 1)))
      (unless dired-omit-mode
        (setq move (+ move 2)))
      (dired-next-line move)
      (if (= oldpos (point))
          (beginning-of-buffer))))
  (defun dired-end-of-buffer ()
    (interactive)
    (end-of-buffer)
    (dired-next-line -1))
  (define-key dired-mode-map (vector 'remap 'beginning-of-buffer) 'dired-beginning-of-buffer)
  (define-key dired-mode-map (vector 'remap 'end-of-buffer) 'dired-end-of-buffer)
  ;; ========跳转至dired顶部和尾部=============
;;;; webdav_sync同步文件
  ;; =========webdav_sync同步文件==============
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
  (global-set-key (kbd "C-x M-,") '(lambda () (interactive) (swint-nutstore-sync "down")))
  (global-set-key (kbd "C-x M-.") '(lambda () (interactive) (swint-nutstore-sync "up")))
  (global-set-key (kbd "C-x M-/") '(lambda () (interactive) (swint-nutstore-sync "bi")))
  ;; =========webdav_sync同步文件==============
;;;; unison
  ;; ================unison====================
  (defun swint-unison-sync-backups ()
    "Sync files in ~/org/backups with ~/Nutstore/backups."
    (interactive)
    (let ((process (start-process-shell-command
                    "unison" "*unison*"
                    (concat "unison" " Nutstore-backups")))
          (pos (memq 'mode-line-modes mode-line-format)))
      (setcdr pos (cons "unison-sync-backups " (cdr pos)))
      (set-process-sentinel
       process
       (lambda (process signal)
         (when (memq (process-status process) '(exit signal))
           (let ((pos (memq 'mode-line-modes mode-line-format)))
             (message "unison-sync-backups done.")
             (setcdr pos (remove "unison-sync-backups " (cdr pos)))))))))
  (global-set-key (kbd "M-s C-/") 'swint-unison-sync-backups)
  ;; ================unison====================
;;;; bypy
  ;; =================bypy=====================
  (defun swint-bypy-sync (&optional arg)
    "Synchronization of bypy-sync."
    (interactive)
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
  (global-set-key (kbd "M-s C-,") '(lambda () (interactive) (swint-bypy-sync)))
  (global-set-key (kbd "M-s C-.") '(lambda () (interactive) (swint-bypy-sync t)))
  ;; =================bypy=====================
;;;; totalcmd
  ;; ===============totalcmd===================
  (defun tc-open-default-directory ()
    (interactive)
    (cond
     (is-win (w32-shell-execute
              "open" "c:/totalcmd/TOTALCMD.EXE" (concat "/O /T \" " (expand-file-name default-directory))))
     (is-lin (progn (start-process-shell-command
                     "tc" "*tc*"
                     (concat "wine "
                             "~/.wine/drive_c/totalcmd/TOTALCMD.EXE /O /T z:"
                             (replace-regexp-in-string " " "\\\\ " (expand-file-name default-directory))))
                    (let ((default-directory
                            "~/.wine/drive_c/Program Files/viatc/"))
                      (start-process-shell-command
                       "viatc" "*viatc*"
                       "wine viatc.exe"))))))
  (defun tc-lister-open-file ()
    (interactive)
    (cond
     (is-win (w32-shell-execute
              "open" "c:/totalcmd/TOTALCMD.EXE" (concat "/O /T /S=L \" " (dired-get-filename))))
     (is-lin (start-process-shell-command
              "tc" "*tc*"
              (concat "wine "
                      "~/.wine/drive_c/totalcmd/TOTALCMD.EXE /O /T /S=L z:"
                      (replace-regexp-in-string " " "\\\\ "
                                                (expand-file-name (dired-get-filename))))))))
  (global-set-key (kbd "C-s-e") 'tc-open-default-directory)
  ;; ===============totalcmd===================
  (when is-lin
;;;; 默认程序打开文件
    ;; ============默认程序打开文件============
    (defun dired-async-shell-command-on-files ()
      (interactive)
      (mapcar #'(lambda (file) (dired-async-shell-command file))
              (dired-get-marked-files))
      (dired-unmark-all-files ?*))
    (defun dired-async-shell-command (file)
      (interactive)
      (if (not (boundp 'file-extension-app-alist))
          (setq file-extension-app-alist
                '(("pdf" . "llpp") ("djvu" . "llpp")
                  ("rmvb" . "mplayer") ("rm" . "mplayer") ("mp4" . "mplayer") ("avi" . "mplayer") ("flv" . "mplayer") ("f4v" . "mplayer") ("mpg" . "mplayer") ("mkv" . "mplayer") ("3gp" . "mplayer") ("wmv" . "mplayer") ("mov" . "mplayer") ("dat" . "mplayer") ("asf" . "mplayer") ("mpeg" . "mplayer") ("wma" . "mplayer") ("gif" . "mplayer")
                  ("mp3" . "mpg321") ("ape" . "mplayer")
                  ("xoj" . "xournal")
                  ("jpg" . "feh.sh") ("png" . "feh.sh") ("bmp" . "feh.sh") ("jpeg" . "feh.sh")
                  ("eps" . "gv") ("ps" . "gv")
                  ("html" . "firefox") ("htm" . "firefox")
                  ("doc" . "word.sh") ("docx" . "word.sh")
                  ("xls" . "excel.sh") ("xlsx" . "excel.sh")
                  ("ppt" . "ppt.sh") ("pptx" . "ppt.sh")
                  ("ods" . "libreoffice")("odt" . "libreoffice")
                  ("dwg" . "cad-2004.sh") ("dxf" . "cad-2004.sh")
                  ("caj" . "caj.sh") ("nh" . "caj.sh") ("kdh" . "caj.sh")
                  ("gp" . "gnuplot")
                  ("rar" . "unrar x -o+")
                  ("zip" . "unzip")
                  ("gz" . "tar zvxf") ("tgz" . "tar zvxf") ("bz2" . "tar jvxf") ("tar" . "tar xf")
                  ("tex" . "xelatex")
                  ("dot" . "dot -Tpng -o dot.png")
                  ("c" . "gcc -Wall"))))
      (let ((file-exten (downcase (file-name-extension file))))
        (let ((command (cdr (assoc file-exten file-extension-app-alist))))
          (start-process "Shell" nil shell-file-name shell-command-switch
                         (concat command " " "\""
                                 (if (member file-exten '("doc" "docx" "xls" "xlsx" "ppt" "pptx" "dwg" "dxf" "caj" "nh" "kdh"))
                                     (file-name-nondirectory file)
                                   file)
                                 "\"")))))
    ;; 设置文件的默认打开方式。
    (setq dired-guess-shell-alist-user
          (list
           (list "\\.pdf$" "adobe.sh * >/dev/null 2>&1 &")
           (list "\\.doc$" "wps * >/dev/null 2>&1 &")
           (list "\\.docx$" "wps * >/dev/null 2>&1 &")
           (list "\\.ppt$" "wpp * >/dev/null 2>&1 &")
           (list "\\.pptx$" "wpp * >/dev/null 2>&1 &")
           (list "\\.xls$" "et * >/dev/null 2>&1 &")
           (list "\\.xlsx$" "et * >/dev/null 2>&1 &")
           (list "\\.ps$" "display -flatten * >/dev/null 2>&1 &")
           (list "\\.eps$" "display -flatten * >/dev/null 2>&1 &")
           (list "\\.jpg$" "display -flatten * >/dev/null 2>&1 &")
           (list "\\.png$" "display -flatten * >/dev/null 2>&1 &")
           (list "\\.bmp$" "display -flatten * >/dev/null 2>&1 &")
           (list "\\.html$" "firefox * >/dev/null 2>&1 &")
           (list "\\.dwg$" "cad-2008.sh * >/dev/null 2>&1 &")
           (list "\\.dxf$" "cad-2008.sh * >/dev/null 2>&1 &")
           (list "\\.mp3$" "mpg321 * >/dev/null 2>&1 &")
           (list "\\.ape$" "mplayer * >/dev/null 2>&1 &")
           (list "\\.avi$" "mplayer * >/dev/null 2>&1 &")
           (list "\\.mkv$" "mplayer * >/dev/null 2>&1 &")
           (list "\\.rmvb$" "mplayer * >/dev/null 2>&1 &")
           (list "\\.mp4$" "mplayer * >/dev/null 2>&1 &")
           (list "\\.rm$" "mplayer * >/dev/null 2>&1 &")
           (list "\\.flv$" "mplayer * >/dev/null 2>&1 &")
           (list "\\.mov$" "mplayer * >/dev/null 2>&1 &")
           (list "\\.mpg$" "mplayer * >/dev/null 2>&1 &")
           (list "\\.ods$" "libreoffice * >/dev/null 2>&1 &")
           (list "\\.tex$" "xelatex * >/dev/null 2>&1 &")
           (list "\\.c$" "gcc -Wall")))
    (define-key dired-mode-map (kbd "C-j") 'dired-async-shell-command-on-files)
    ;; ============默认程序打开文件============
;;;; 在当前目录下打开urxvt
    ;; ========在当前目录下打开urxvt===========
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
    (global-set-key (kbd "C-s-<return>") 'urxvt-default-directory)
    ;; ========在当前目录下打开urxvt===========
;;;; cad文件版本转换
    ;; ===========cad文件版本转换==============
    (defun swint-dired-cad-converter (&optional arg)
      "Convert cad file version in dired-mode."
      (interactive "P")
      (let ((swint-dired-current-file (file-name-nondirectory (dired-get-filename))))
        (start-process-shell-command
         "TeighaFileConverter" "*TeighaFileConverter*"
         (concat "TeighaFileConverter ./ ./dwg ACAD2004 DWG 0 1 "
                 (unless arg
                   swint-dired-current-file)))))
    (define-key dired-mode-map (kbd "C-c c") 'swint-dired-cad-converter)
    (define-key dired-mode-map (kbd "C-c C") '(lambda () (interactive)
                                                (swint-dired-cad-converter t)))
    ;; ===========cad文件版本转换==============
    )
;;;; dired-view-file-or-dir
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
  )
;; ==================dired=====================
;;; w32-browser
;; ================w32-browser=================
(use-package w32-browser
  :if is-win
  :commands w32-browser
  :bind (:map dired-mode-map
              ("C-i" . w32explore)
              ("C-j" . dired-w32-browser-on-files))
  :config
  (defun dired-w32-browser-on-files ()
    "Fix problems of opening word."
    (interactive)
    (mapcar #'(lambda (file)
                (if (and (or (string-equal (file-name-extension file) "doc")
                             (string-equal (file-name-extension file) "docx"))
                         (not (string-match "WINWORD.EXE" (concat (prin1-to-string (proced-process-attributes))))))
                    (progn (w32-shell-execute "open" "word")
                           (sit-for 5)))
                (w32-browser (dired-replace-in-string "/" "\\" file)))
            (dired-get-marked-files))
    (dired-unmark-all-files ?*)))
;; ================w32-browser=================
;;; peep-dired
;; ================peep-dired==================
(use-package peep-dired
  ;; image-dired: Use C-t as prefix.
  :bind (:map dired-mode-map
              ("q" . peep-dired))
  :config
  (defun peep-dired-display-image-other-window (fn)
    "Use image-dired for peep images."
    (let ((image-entry-name (dired-file-name-at-point))
          (peep-dired-image-extensions '("png" "PNG" "JPG" "jpg" "bmp" "BMP" "jpeg" "JPEG")))
      (if (member (file-name-extension image-entry-name)
                  peep-dired-image-extensions)
          (add-to-list 'peep-dired-peeped-buffers
                       (window-buffer
                        (display-buffer
                         (progn
                           (require 'image-dired)
                           (image-dired-create-display-image-buffer)
                           (display-buffer image-dired-display-image-buffer)
                           (image-dired-display-image image-entry-name)
                           image-dired-display-image-buffer)
                         t)))
        (funcall fn))))
  (advice-add 'peep-dired-display-file-other-window :around #'peep-dired-display-image-other-window)
  (advice-add 'peep-dired-disable :after #'(lambda () (if (and (boundp 'image-dired-display-image-buffer)
                                                               (get-buffer image-dired-display-image-buffer))
                                                          (kill-buffer image-dired-display-image-buffer))))
  (setq peep-dired-enable-on-directories nil)
  (define-key peep-dired-mode-map (kbd "p") 'peep-dired-prev-file)
  (define-key peep-dired-mode-map (kbd "n") 'peep-dired-next-file)
  (define-key peep-dired-mode-map (kbd "C-p") nil)
  (define-key peep-dired-mode-map (kbd "C-n") nil))
;; ================peep-dired==================
;;; dired-async
;; ================dired-async=================
(use-package dired-async
  :diminish dired-async-mode
  :commands dired-async-mode
  :init
  (add-hook 'dired-mode-hook '(lambda () (dired-async-mode 1)))
  :config
  (defun dired-do-copy-before (&optional arg)
    "Redefine dired-do-copy to fix conflict between dired-async-mode and dired-sync-highlight."
    (let* ((fn-list (dired-get-marked-files nil arg))
           (fn-list-nodirectory (mapcar 'file-name-nondirectory fn-list))
           (annotated-file-list (if (dired-k--parse-status t)
                                    (hash-table-keys (dired-k--parse-status t))))
           (fn-list-annotated (remove-if-not (lambda (x)
                                               (member x annotated-file-list))
                                             fn-list-nodirectory))
           (annotation-storage-files
            (remove nil (mapcar (lambda (from)
                                  (directory-files "~/org/annotated/" t
                                                   (concat "annotated-("
                                                           (replace-regexp-in-string
                                                            "/" "_"
                                                            (substring-no-properties
                                                             (abbreviate-file-name from) 1)))))
                                fn-list))))
      (when (or fn-list-annotated annotation-storage-files)
        (dired-async-mode 0))))
  (advice-add 'dired-do-copy :before #'dired-do-copy-before)
  (advice-add 'dired-do-copy :after #'(lambda (&optional arg) (dired-async-mode 1))))
;; ================dired-async=================
;;; dired-narrow
;; ===============dired-narrow=================
(use-package dired-narrow
  :bind (:map dired-mode-map
              ("/" . dired-narrow))
  :config
  (defun dired-narrow--string-filter-py (filter)
    (let ((words (split-string filter " ")))
      (--all? (save-excursion (or (search-forward it (line-end-position) t)
                                  (re-search-forward (pinyinlib-build-regexp-string it)
                                                     (line-end-position) t)))
              words)))
  (advice-add 'dired-narrow--string-filter :override #'dired-narrow--string-filter-py))
;; ===============dired-narrow=================
;;; dired-ranger
;; ===============dired-ranger=================
(use-package dired-ranger
  :bind (:map dired-mode-map
              ("M-w" . swint-dired-ranger-copy))
  :config
  (defun swint-dired-ranger-copy ()
    (interactive)
    (easy-kill)
    (call-interactively 'dired-ranger-copy))
  ;; 加C-u不清除clipboards。
  (bind-key "C-y" 'dired-ranger-paste dired-mode-map)
  (bind-key "M-y" 'dired-ranger-move dired-mode-map))
;; ===============dired-ranger=================
;;; neotree
;; =================neotree====================
(use-package neotree
  :bind ("C-x j" . neotree-project-or-current-dir)
  :config
  (setq neo-smart-open nil)
  (setq neo-show-hidden-files nil)
  (setq neo-confirm-change-root 'off-p)
  (define-key neotree-mode-map "\C-j" 'neotree-shell-command)
  (define-key neotree-mode-map (kbd "b") 'neotree-select-previous-sibling-node)
  (define-key neotree-mode-map (kbd "f") 'neotree-select-next-sibling-node)
  (define-key neotree-mode-map (kbd "RET") (neotree-make-executor :file-fn 'neo-open-file
                                                                  :dir-fn  'neo-open-dired))
  (define-key neotree-mode-map (kbd "a") 'neotree-stretch-toggle)
  (define-key neotree-mode-map (kbd "u") 'neotree-select-up-node)
  (define-key neotree-mode-map (kbd "d") 'neotree-select-down-node)
  (define-key neotree-mode-map (kbd "h") 'neotree-hidden-file-toggle)
  (defun neotree-project-or-current-dir ()
    "Open NeoTree using project root or current directory."
    (interactive)
    (let ((project-dir (ignore-errors (projectile-project-root)))
          (find-file-name (if (eq major-mode 'dired-mode)
                              (dired-get-filename)
                            (buffer-file-name)))
          (current-dir default-directory))
      (if (neo-global--window-exists-p)
          (neotree-hide)
        (progn (neotree-show)
               (neotree-dir (or project-dir current-dir))
               (when find-file-name
                 (neotree-find find-file-name))))))
  (defun neotree-shell-command ()
    "Open file with external app."
    (interactive)
    (let ((file (neo-buffer--get-filename-current-line)))
      (cond (is-lin (dired-async-shell-command file))
            (is-win (progn (if (and (or (string-equal (file-name-extension file) "doc")
                                        (string-equal (file-name-extension file) "docx"))
                                    (not (string-match "WINWORD.EXE" (concat (prin1-to-string (proced-process-attributes))))))
                               (progn (w32-shell-execute "open" "word")
                                      (sit-for 5)))
                           (w32-browser file)))))))
;; =================neotree====================
(provide 'setup_dired)
