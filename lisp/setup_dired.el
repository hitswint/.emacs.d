;; ======================dired========================
(use-package dired
  ;; Enabled automatically.
  :config
  (use-package dired-x)
  (use-package dired-details
    :config
    (dired-details-install))
  (use-package diredful
    :config
    (diredful-mode 1))
  (setq dired-recursive-copies 'top)
  (setq dired-recursive-deletes 'top)
  ;; =============Auto-revert-mode=============
  ;; Auto refresh buffers
  (global-auto-revert-mode 1)
  ;; Also auto refresh dired, but be quiet about it
  (setq global-auto-revert-non-file-buffers t)
  (setq auto-revert-verbose nil)
  ;; dired-k--highlight会使auto-revert-mode出错，在dired-mode中禁用auto-revert-mode。
  ;; 已修复上述问题，在dired-mode中重新开启auto-revert-mode。
  ;; (setq global-auto-revert-ignore-modes '(dired-mode))
  ;; 使用dired-mode自带的auto-revert。
  (setq dired-auto-revert-buffer t)
  ;; =============Auto-revert-mode=============
  (put 'dired-find-alternate-file 'disabled nil)
  ;; 显示文件大小
  (setq dired-listing-switches "-alh")
  ;; 文件夹间复制
  (setq dired-dwim-target t)
  ;; 将annotated显示加hook放在前面，使其出现在dired-after-readin-hook中函数列表最后，进而最后生效。
  (add-hook 'dired-after-readin-hook 'dired-k--highlight)
  ;; 不折行显示
  (add-hook 'dired-after-readin-hook '(lambda ()
                                        (setq truncate-lines t)))
  ;; 快捷键
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
               ;; 在dired对mark的多个文件内容进行查找
               (define-key dired-mode-map (kbd "C-c C-s") 'dired-do-isearch)
               (define-key dired-mode-map (kbd "C-c C-M-s") 'dired-do-isearch-regexp)
               (define-key dired-mode-map (kbd "C-c C-p") 'dired-k--previous-annotated-file)
               (define-key dired-mode-map (kbd "C-c C-n") 'dired-k--next-annotated-file)
               (define-key dired-mode-map (kbd "C-/") 'helm-dired-current-file)))
  ;; ================Kill subdir=================
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
  ;; ================Kill subdir=================
  ;; ===========helm-dired-current-file==========
  (defun helm-dired-current-file ()
    (interactive)
    (let ((swint-dired-current-file (dired-get-filename)))
      (if (file-directory-p swint-dired-current-file)
          (helm-find-files-1 (file-name-as-directory swint-dired-current-file))
        (helm-find-files-1 (expand-file-name default-directory)
                           (if helm-ff-transformer-show-only-basename
                               (helm-basename swint-dired-current-file) swint-dired-current-file)))))
  ;; ===========helm-dired-current-file==========
  ;; ==========默认文件夹排在最前面==============
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
  ;; ==========默认文件夹排在最前面==============
  ;; =====================文件夹排序=======================
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
  ;; =====================文件夹排序=======================
  ;; =====================跳转至dired顶部和尾部==================
  (defun dired-back-to-top ()
    (interactive)
    (beginning-of-buffer)
    (cond
     (is-lin (dired-next-line 4))
     (is-win (dired-next-line 3))))
  (define-key dired-mode-map
    (vector 'remap 'beginning-of-buffer) 'dired-back-to-top)
  (defun dired-jump-to-bottom ()
    (interactive)
    (end-of-buffer)
    (dired-next-line -1))
  (define-key dired-mode-map
    (vector 'remap 'end-of-buffer) 'dired-jump-to-bottom)
  (defun dired-beginning-of-line ()
    (interactive)
    (smart-beginning-of-line)
    (forward-char 1))
  (define-key dired-mode-map
    (vector 'remap 'smart-beginning-of-line) 'dired-beginning-of-line)
  ;; =====================跳转至dired顶部和尾部==================
  ;; ===================webdav_sync同步文件======================
  (defun swint-webdav-sync-down ()
    "Sync files in webdav server to ~/Nutstore-sync."
    (interactive)
    (let ((process
           (start-process-shell-command
            "webdav_sync" "*webdav_sync*"
            (concat "java -Dderby.system.home=" (expand-file-name "~/.webdav_sync/")
                    " -Dbe.re.http.no-compress -jar " (expand-file-name "~/.webdav_sync/webdav_sync1_1_4.jar")
                    " -r -down -u https://wgq_713%40163.com:arxg55upvg9urwus@dav.jianguoyun.com/dav/Nutstore-sync/ -d "
                    (expand-file-name "~/Nutstore-sync/"))))
          (pos (memq 'mode-line-modes mode-line-format)))
      (setcdr pos (cons "webdav-sync-down " (cdr pos)))
      (set-process-sentinel
       process
       (lambda (process signal)
         (when (memq (process-status process) '(exit signal))
           (let ((webdav_sync-process-output (with-current-buffer "*webdav_sync*"
                                               (buffer-substring-no-properties (- (point-max) 6) (point-max))))
                 (pos (memq 'mode-line-modes mode-line-format)))
             (if (string-equal webdav_sync-process-output "Done.\n")
                 (message "swint-webdav-sync-down done.")
               (message "swint-webdav-sync-down failed"))
             (setcdr pos (remove "webdav-sync-down " (cdr pos)))))))))
  (defun swint-webdav-sync-up ()
    "Sync files in ~/Nutstore-sync to webdav server."
    (interactive)
    (let ((process
           (start-process-shell-command
            "webdav_sync" "*webdav_sync*"
            (concat "java -Dderby.system.home=" (expand-file-name "~/.webdav_sync/")
                    " -Dbe.re.http.no-compress -jar " (expand-file-name "~/.webdav_sync/webdav_sync1_1_4.jar")
                    " -r -up -u https://wgq_713%40163.com:arxg55upvg9urwus@dav.jianguoyun.com/dav/Nutstore-sync/ -d "
                    (expand-file-name "~/Nutstore-sync/"))))
          (pos (memq 'mode-line-modes mode-line-format)))
      (setcdr pos (cons "webdav-sync-up " (cdr pos)))
      (set-process-sentinel
       process
       (lambda (process signal)
         (when (memq (process-status process) '(exit signal))
           (let ((webdav_sync-process-output (with-current-buffer "*webdav_sync*"
                                               (buffer-substring-no-properties (- (point-max) 6) (point-max))))
                 (pos (memq 'mode-line-modes mode-line-format)))
             (if (string-equal webdav_sync-process-output "Done.\n")
                 (message "swint-webdav-sync-up done.")
               (message "swint-webdav-sync-up failed"))
             (setcdr pos (remove "webdav-sync-up " (cdr pos)))))))))
  (defun swint-webdav-sync-bi ()
    "Sync files in ~/Nutstore-sync to webdav server."
    (interactive)
    (let ((process
           (start-process-shell-command
            "webdav_sync" "*webdav_sync*"
            (concat "java -Dderby.system.home=" (expand-file-name "~/.webdav_sync/")
                    " -Dbe.re.http.no-compress -jar " (expand-file-name "~/.webdav_sync/webdav_sync1_1_4.jar")
                    " -r -bi -u https://wgq_713%40163.com:arxg55upvg9urwus@dav.jianguoyun.com/dav/Nutstore-sync/ -d "
                    (expand-file-name "~/Nutstore-sync/"))))
          (pos (memq 'mode-line-modes mode-line-format)))
      (setcdr pos (cons "webdav-sync-bi " (cdr pos)))
      (set-process-sentinel
       process
       (lambda (process signal)
         (when (memq (process-status process) '(exit signal))
           (let ((webdav_sync-process-output (with-current-buffer "*webdav_sync*"
                                               (buffer-substring-no-properties (- (point-max) 6) (point-max))))
                 (pos (memq 'mode-line-modes mode-line-format)))
             (if (string-equal webdav_sync-process-output "Done.\n")
                 (message "swint-webdav-sync-bi done.")
               (message "swint-webdav-sync-bi failed"))
             (setcdr pos (remove "webdav-sync-bi " (cdr pos)))))))))
  (global-set-key (kbd "C-x M-,") 'swint-webdav-sync-down)
  (global-set-key (kbd "C-x M-.") 'swint-webdav-sync-up)
  (global-set-key (kbd "C-x M-/") 'swint-webdav-sync-bi)
  ;; ===================webdav_sync同步文件======================
  ;; ===================unison====================
  (defun swint-unison-sync-backups ()
    "Sync files in ~/Nutstore-sync to webdav server."
    (interactive)
    (let ((process
           (start-process-shell-command
            "unison" "*unison*"
            (concat (cond
                     (is-lin "unison")
                     (is-win "c:/cygwin64/bin/unison-2.40.exe"))
                    " Nutstore-backups")))
          (pos (memq 'mode-line-modes mode-line-format)))
      (setcdr pos (cons "unison-sync-backups " (cdr pos)))
      (set-process-sentinel
       process
       (lambda (process signal)
         (when (memq (process-status process) '(exit signal))
           (let ((pos (memq 'mode-line-modes mode-line-format)))
             (message "swint-unison-sync-backups done.")
             (setcdr pos (remove "unison-sync-backups " (cdr pos)))))))))
  (global-set-key (kbd "C-c M-/") 'swint-unison-sync-backups)
  ;; ===================unison====================
  )
;; =====================默认程序打开文件==================
(cond
 (is-lin
  (add-hook 'dired-mode-hook
            (lambda ()
              (define-key dired-mode-map (kbd "C-j") 'dired-async-shell-command-for-alternate-file)))
  (defun dired-async-shell-command-for-alternate-file ()
    (interactive)
    (async-shell-command-no-output-buffer-from-file (dired-get-file-for-visit)))
  (defun async-shell-command-no-output-buffer-from-file (file)
    (interactive)
    (if (not (boundp 'file-extension-app-alist))
        (setq file-extension-app-alist
              '(("pdf" . "llpp") ("djvu" . "llpp")
                ("rmvb" . "mplayer") ("rm" . "mplayer") ("mp4" . "mplayer") ("avi" . "mplayer") ("flv" . "mplayer") ("f4v" . "mplayer") ("mpg" . "mplayer") ("mkv" . "mplayer") ("3gp" . "mplayer") ("wmv" . "mplayer") ("mov" . "mplayer") ("dat" . "mplayer") ("asf" . "mplayer") ("mpeg" . "mplayer") ("wma" . "mplayer")
                ("mp3" . "mpg321")
                ("ape" . "mplayer")
                ("xoj" . "xournal")
                ("jpg" . "~/feh.sh") ("png" . "~/feh.sh") ("bmp" . "~/feh.sh") ("jpeg" . "~/feh.sh")
                ("eps" . "gv") ("ps" . "gv")
                ("html" . "firefox") ("htm" . "firefox")
                ("doc" . "wine-development /home/swint/.wine/drive_c/Program\\ Files/Microsoft\\ Office/Office12/WINWORD.EXE")
                ("docx" . "wine-development /home/swint/.wine/drive_c/Program\\ Files/Microsoft\\ Office/Office12/WINWORD.EXE")
                ("xls" . "wine-development /home/swint/.wine/drive_c/Program\\ Files/Microsoft\\ Office/Office12/EXCEL.EXE")
                ("xlsx" . "wine-development /home/swint/.wine/drive_c/Program\\ Files/Microsoft\\ Office/Office12/EXCEL.EXE")
                ("ppt" . "wine-development /home/swint/.wine/drive_c/Program\\ Files/Microsoft\\ Office/Office12/POWERPNT.EXE")
                ("pptx" . "wine-development /home/swint/.wine/drive_c/Program\\ Files/Microsoft\\ Office/Office12/POWERPNT.EXE")
                ("ods" . "libreoffice")("odt" . "libreoffice")
                ("dwg" . "wine-development /home/swint/.wine/drive_c/Program\\ Files/AutoCAD\\ 2004/acad.exe")
                ("dxf" . "librecad")
                ("caj" . "wine-development /home/swint/.wine/drive_c/Program\\ Files/CAJViewer/CAJViewer.exe")
                ("nh" . "wine-development /home/swint/.wine/drive_c/Program\\ Files/CAJViewer/CAJViewer.exe")
                ("kdh" . "wine-development /home/swint/.wine/drive_c/Program\\ Files/CAJViewer/CAJViewer.exe")
                ("gp" . "gnuplot")
                ("rar" . "unrar x -o+")
                ("zip" . "unzip")
                ("gz" . "tar zvxf")
                ("tgz" . "tar zvxf")
                ("bz2" . "tar jvxf")
                ("tar" . "tar xf")
                ("tex" . "xelatex")
                ("dot" . "dot -Tpng -o dot.png")
                ("c" . "gcc -Wall")
                )))
    (let ((file-exten (downcase (file-name-extension file))))
      (let ((command (cdr (assoc file-exten file-extension-app-alist))))
        (start-process "Shell" nil shell-file-name shell-command-switch
                       (concat command " " "\""
                               (if (member file-exten '("doc" "docx" "xls" "xlsx" "ppt" "pptx" "dwg" "caj" "nh" "kdh"))
                                   (file-name-nondirectory file)
                                 file)
                               "\"")))))
  ;; 设置一些文件的默认打开方式，此功能必须在(require 'dired-x)之后。
  (setq dired-guess-shell-alist-user
        (list
         (list "\\.pdf$" "/home/swint/adobe.sh * >/dev/null 2>&1 &")
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
         (list "\\.dxf$" "librecad * >/dev/null 2>&1 &")
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
  ;; ===============在当前目录下打开urxvt===============
  (defun urxvt-default-directory ()
    (interactive)
    (start-process "Urxvt" nil shell-file-name shell-command-switch
                   (concat "tabbed -c " "urxvt" " -cd " "\""
                           (expand-file-name default-directory) "\"" " -embed")))
  (global-set-key (kbd "C-s-m") 'urxvt-default-directory))
 (is-win
  ;; =====================w32-browser======================
  (use-package w32-browser
    ;; Enabled automatically.
    :config
    ;; (eval-after-load "dired"
    ;;   '(define-key dired-mode-map (kbd "TAB") (lambda ()
    ;;                                             (interactive)
    ;;                                             (w32-browser
    ;;                                              (dired-replace-in-string
    ;;                                               "/" "\\"
    ;;                                               (dired-get-filename))))))
    (define-key dired-mode-map (kbd "C-j") 'swint-w32-browser-open)
    (define-key dired-mode-map (kbd "C-i") 'w32explore)
    (defun w32-browser-open ()
      (interactive)
      (w32-browser
       (dired-replace-in-string
        "/" "\\"
        (dired-get-filename))))
    (defun swint-w32-browser-open ()
      "Fix problems of openning word"
      (interactive)
      (if (and (or (string-equal (file-name-extension (dired-get-filename)) "doc")
                   (string-equal (file-name-extension (dired-get-filename)) "docx"))
               (not (string-match "WINWORD.EXE" (concat (prin1-to-string (proced-process-attributes))))))
          (progn (w32-shell-execute "open" "word")
                 (sit-for 5)))
      (w32-browser-open)))
  ;; ==============默认程序打开，但是emacs会冻结=================
  ;; (eval-after-load "dired"
  ;;   '(progn
  ;;      ;; Dired 原来的 “o” 对我来说基本没用。
  ;;      (define-key dired-mode-map (kbd "TAB") 'chunyu-dired-open-explorer)))
  ;; (defun chunyu-dired-open-explorer ()
  ;;   (interactive)
  ;;   (let ((file-name (dired-get-file-for-visit)))
  ;;     (if (file-exists-p file-name)
  ;;      (start-process "dir" nil
  ;;                     "cmd.exe" "/c" "start" file-name))))
  ;; ;;用如下的方法解决空格问题
  ;; (defun chunyu-dired-open-explorer ()
  ;;   (interactive)
  ;;   (let ((file-name (dired-get-file-for-visit)))
  ;;     (if (file-exists-p file-name)
  ;;      (shell-command (format "\"%s\"" file-name) ))))
  ))
;; =====================默认程序打开文件==================
;; ===================peep-dired====================
(use-package peep-dired
  ;; Enabled at commands.
  :defer t
  :commands peep-dired
  :init
  (bind-key "q" '(lambda ()
                   (interactive)
                   (let ((image-buffer (get-buffer image-dired-display-image-buffer)))
                     (if (buffer-live-p image-buffer)
                         (progn
                           (kill-buffer image-buffer)))
                     (call-interactively 'peep-dired))) dired-mode-map)
  :config
  (bind-key "q" nil peep-dired-mode-map)
  ;; image-dired默认使用convert作为图片转换程序。
  ;; C-t C-t: toggle thumbs display; C-t i: display image; C-t d: display thumbs; C-t x: display external。
  ;; win上c:/cygwin64/bin同时存在convert.exe和imgconvert.exe，需要将转换程序设置为imgconvert.exe。
  (when is-win
    (setq image-dired-cmd-create-temp-image-program "imgconvert"))
  (defcustom peep-dired-image-extensions
    '("png" "PNG" "JPG" "jpg" "bmp" "BMP" "jpeg" "JPEG")
    "Extensions to not try to open"
    :group 'peep-dired
    :type 'list)
  (defun peep-dired-display-file-other-window ()
    (let ((entry-name (dired-file-name-at-point)))
      (unless (member (file-name-extension entry-name)
                      peep-dired-ignored-extensions)
        (add-to-list 'peep-dired-peeped-buffers
                     (if (member (file-name-extension entry-name)
                                 peep-dired-image-extensions)
                         (window-buffer
                          (display-buffer
                           (progn
                             (image-dired-create-display-image-buffer)
                             (display-buffer image-dired-display-image-buffer)
                             (image-dired-display-image entry-name)
                             image-dired-display-image-buffer)
                           t))
                       (window-buffer
                        (display-buffer
                         (if (file-directory-p entry-name)
                             (peep-dired-dir-buffer entry-name)
                           (or
                            (find-buffer-visiting entry-name)
                            (find-file-noselect entry-name)))
                         t)))))))
  (setq peep-dired-cleanup-on-disable t)
  ;; (setq peep-dired-cleanup-eagerly t)
  (setq peep-dired-enable-on-directories nil)
  (setq peep-dired-ignored-extensions '("mkv" "iso" "mp4"))
  (add-hook 'peep-dired-hook
            '(lambda ()
               (define-key peep-dired-mode-map (kbd "p") 'peep-dired-prev-file)
               (define-key peep-dired-mode-map (kbd "n") 'peep-dired-next-file)
               (define-key peep-dired-mode-map (kbd "C-p") nil)
               (define-key peep-dired-mode-map (kbd "C-n") nil))))
;; ===================peep-dired====================
;; ===================async====================
;; async: Simple library for asynchronous processing in Emacs.
;; async-start async-start-process async-get async-ready async-wait
(use-package dired-async
  ;; Enabled in dired-mode.
  :defer t
  :init
  (add-hook 'dired-mode-hook '(lambda () (dired-async-mode 1)))
  :config
  (defun dired-do-copy (&optional arg)
    "Redefine dired-do-copy to fix conflict between dired-async-mode and dired-sync-annotated."
    (interactive "P")
    (let* ((fn-list (dired-get-marked-files nil arg))
           (fn-list-nodirectory (mapcar 'file-name-nondirectory fn-list))
           (annotated-file-list (if (dired-k--parse-annotated-status)
                                    (hash-table-keys (dired-k--parse-annotated-status))))
           (fn-list-annotated (remove-if-not (lambda (x)
                                               (member x annotated-file-list))
                                             fn-list-nodirectory))
           (annotation-storage-files
            (remove nil (mapcar (lambda (from)
                                  (directory-files "~/org/annotated/" t
                                                   (concat "annotated-("
                                                           (replace-regexp-in-string
                                                            "/" "_" (substring-no-properties (abbreviate-file-name from) 1)) "_")))
                                fn-list))))
      (if (or fn-list-annotated annotation-storage-files)
          (progn (message "%s" fn-list-annotated)
                 (message "%s" annotation-storage-files)
                 (dired-async-mode 0))))
    (let ((dired-recursive-copies dired-recursive-copies))
      (dired-do-create-files 'copy (function dired-copy-file)
                             "Copy"
                             arg dired-keep-marker-copy
                             nil dired-copy-how-to-fn))
    (dired-async-mode 1)))
;; (autoload 'dired-async-mode "dired-async.el" nil t)
;; ===================async====================
;; ===============dired-narrow=================
(use-package dired-narrow
  ;; Enabled at commands.
  :defer t
  :bind (:map dired-mode-map
              ("/" . dired-narrow))
  :config
  (defun dired-narrow--string-filter (filter)
    (let ((words (split-string filter " ")))
      (--all? (save-excursion (or (search-forward it (line-end-position) t)
                                  (unless (or (string-match "[iuv]" it) ;当字符串中有iuv时，不转换string
                                              (string-empty-p (pinyin-search--pinyin-to-regexp it))) ;当搜索中文或符号时，不转换string
                                    (re-search-forward (pinyin-search--pinyin-to-regexp it) (line-end-position) t)))) words))))
;; ===============dired-narrow=================
;; ===============dired-ranger=================
(use-package dired-ranger
  ;; Enabled at commands.
  :defer t
  :bind (:map dired-mode-map
              ;; 加C-u不清除clipboards。
              (("C-y" . dired-ranger-paste)
               ("C-M-y" . dired-ranger-move)))
  :init
  (bind-key "M-w" '(lambda ()
                     (interactive)
                     (easy-kill)
                     (call-interactively 'dired-ranger-copy)) dired-mode-map)
  :config
  ;; C-M-y快捷键绑定会被pop-kill-ring覆盖，这里重新定义。
  (bind-key "C-M-y" 'dired-ranger-move dired-mode-map))
;; ===============dired-ranger=================
;; ======================dired========================
(provide 'setup_dired)
