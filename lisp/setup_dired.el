;;; dired
;; ==================dired=====================
(use-package dired
  :delight "Dired"
  :config
  (use-package dired-x
    :config
    (bind-key ")" 'dired-omit-mode dired-mode-map)
    (add-hook 'dired-mode-hook #'(lambda () (dired-omit-mode 1) (diminish 'dired-omit-mode)))
    (setq dired-omit-verbose nil
          dired-omit-size-limit nil
          dired-omit-extensions nil))
  (use-package dired-filetype-face)
  (add-hook 'dired-mode-hook 'dired-hide-details-mode)
  (setq dired-recursive-copies 'top)
  (setq dired-recursive-deletes 'always)
  (custom-set-faces '(diredp-compressed-file-suffix ((t (:foreground "#7b68ee"))) t)
                    '(diredp-ignored-file-name ((t (:foreground "#aaaaaa"))) t))
;;;; setup-and-keybindings
  ;; ==========setup-and-keybindings===========
  (setq dired-mouse-drag-files t)
  (setq dired-auto-revert-buffer t) ;使用dired/dired-other-window/dired-other-frame时更新，与auto-revert-mode不同
  (advice-add 'dired-buffer-stale-p :around #'(lambda (fn &rest args) ;只有dired可见时才启用auto-revert-mode更新
                                                (when (get-buffer-window (current-buffer)) (apply fn args))))
  (advice-add 'dired-goto-file :around #'(lambda (fn &rest args)
                                           (let ((default-directory (dired-current-directory)))
                                             (apply fn args))))
  (put 'dired-find-alternate-file 'disabled nil)
  (setq dired-listing-switches "--group-directories-first -alhG1")
  (setq dired-subdir-switches dired-listing-switches)
  (setq dired-dwim-target t)
  (setq wdired-allow-to-change-permissions t)
  (add-hook 'dired-after-readin-hook #'(lambda () (setq truncate-lines t)))
  (defvar file_video_exts '("rmvb" "rm" "mp4" "avi" "flv" "f4v" "mpg" "mkv" "3gp" "wmv" "mov" "dat" "asf" "mpeg" "wma" "webm"))
  (defvar file_image_exts '("jpg" "png" "bmp" "jpeg" "svg" "tif"))
  (defvar file-extension-app-alist (append '(("pdf" . "pdfviewer.sh") ("djvu" . "llpp") ("chm" . "xchm")
                                             ("mp3" . "mpg321") ("ape" . "mpv")
                                             ("xoj" . "xournal")
                                             ("gif" . "animate")
                                             ("xcf" . "gimp")
                                             ("eps" . "gv") ("ps" . "gv")
                                             ("html" . "firefox") ("htm" . "firefox")
                                             ("doc" . "wps") ("docx" . "wps")
                                             ("xls" . "et") ("xlsx" . "et") ("csv" . "et")
                                             ("ppt" . "wpp") ("pptx" . "wpp")
                                             ("ods" . "libreoffice") ("odt" . "libreoffice")
                                             ("dwg" . "w.CADReader.sh") ("dxf" . "w.CADReader.sh")
                                             ("caj" . "w.caj.sh") ("nh" . "w.caj.sh") ("kdh" . "w.caj.sh")
                                             ("gp" . "gnuplot")
                                             ("rar" . "unrar x -o+") ("zip" . "unar") ("gz" . "tar zvxf") ("tgz" . "tar zvxf") ("bz2" . "tar jvxf") ("tar" . "tar xf")
                                             ("dot" . "dot -Tpng -o dot.png")
                                             ("dia" . "env GTK_IM_MODULE=xim dia")
                                             ("drawio" . "env GTK_IM_MODULE=xim drawio")
                                             ("blend" . "blender")
                                             ("foam" . "paraview") ("vtk" . "paraview") ("openfoam" . "paraview")
                                             ("eso" . "w.xEsoView.sh") ("mtr" . "w.xEsoView.sh")
                                             ("idf" . "urxvt -e zsh -is eval ep.sh")
                                             ("osm" . "OpenStudioApp"))
                                           (mapcar (lambda (x)
                                                     (cons x "mpv.sh"))
                                                   file_video_exts)
                                           (mapcar (lambda (x)
                                                     (cons x "feh.sh"))
                                                   file_image_exts)))
  (setq async-shell-command-buffer 'new-buffer)
  (setq async-shell-command-display-buffer nil)
  (setq dired-guess-shell-alist-user ;dired-do(async)-shell-command(!/&)的默认命令
        ;; 若命令行中有 * ，则只执行一次，使用 * 替代所有文件名。使用*""输入*本义
        ;; 若命令行中有 ? 或`?`，则分别对每个文件执行一次，使用 ? 替代每个文件名
        ;; 若命令行中无上述字符，则只对当前文件执行，自动在末尾加文件名
        (list
         (list "\\.pdf$" "w.adobe.sh * >/dev/null 2>&1 &")
         (list "\\.doc$" "w.word.sh * >/dev/null 2>&1 &")
         (list "\\.docx$" "w.word.sh * >/dev/null 2>&1 &")
         (list "\\.ppt$" "w.ppt.sh * >/dev/null 2>&1 &")
         (list "\\.pptx$" "w.ppt.sh * >/dev/null 2>&1 &")
         (list "\\.xls$" "w.excel.sh * >/dev/null 2>&1 &")
         (list "\\.xlsx$" "w.excel.sh * >/dev/null 2>&1 &")
         (list "\\.ps$" "display -flatten * >/dev/null 2>&1 &")
         (list "\\.eps$" "display -flatten * >/dev/null 2>&1 &")
         (list "\\.jpg$" "gimp * >/dev/null 2>&1 &")
         (list "\\.jpeg$" "gimp * >/dev/null 2>&1 &")
         (list "\\.png$" "gimp * >/dev/null 2>&1 &")
         (list "\\.bmp$" "gimp * >/dev/null 2>&1 &")
         (list "\\.dwg$" "w.acad.sh * >/dev/null 2>&1 &")
         (list "\\.dxf$" "w.acad.sh * >/dev/null 2>&1 &")
         (list "\\.mp3$" "mpg321 * >/dev/null 2>&1 &")
         (list "\\.ape$" "vlc * >/dev/null 2>&1 &")
         (list "\\.avi$" "vlc * >/dev/null 2>&1 &")
         (list "\\.mkv$" "vlc * >/dev/null 2>&1 &")
         (list "\\.rmvb$" "vlc * >/dev/null 2>&1 &")
         (list "\\.mp4$" "vlc * >/dev/null 2>&1 &")
         (list "\\.rm$" "vlc * >/dev/null 2>&1 &")
         (list "\\.flv$" "vlc * >/dev/null 2>&1 &")
         (list "\\.mov$" "vlc * >/dev/null 2>&1 &")
         (list "\\.mpg$" "vlc * >/dev/null 2>&1 &")
         (list "\\.webm$" "vlc * >/dev/null 2>&1 &")
         (list "\\.tex$" "xelatex * >/dev/null 2>&1 &")
         (list "\\.c$" "gcc -Wall * >/dev/null 2>&1 &")
         (list "\\.ipynb$" "jupyter nbconvert --to python * >/dev/null 2>&1 &")
         (list "\\.eso$" "ReadVarsESO.sh * >/dev/null 2>&1 &")
         (list "\\.mtr$" "ReadVarsESO.sh * >/dev/null 2>&1 &")))
  (add-hook 'dired-mode-hook
            (lambda ()
              (define-key dired-mode-map (kbd "M-=") nil)
              (define-key dired-mode-map (kbd "Q") 'dired-do-query-replace-regexp)
              (define-key dired-mode-map (kbd "e") (lambda ()
                                                     (interactive)
                                                     (find-file-literally (dired-get-file-for-visit))))
              (define-key dired-mode-map (kbd "r") (lambda ()
                                                     (interactive)
                                                     (let ((current-directory default-directory))
                                                       (find-alternate-file "..")
                                                       (dired-goto-file (expand-file-name current-directory)))))
              (define-key dired-mode-map (kbd "i") #'(lambda ()
                                                       (interactive)
                                                       (call-interactively 'dired-maybe-insert-subdir)
                                                       (revert-buffer)))
              (define-key dired-mode-map (kbd "I") #'(lambda ()
                                                       (interactive)
                                                       (dired-kill-and-next-subdir)
                                                       (revert-buffer)))
              (define-key dired-mode-map (kbd "C-j") 'dired-async-shell-command-on-files)
              (define-key dired-mode-map (kbd "v") 'txm-dired-view-file-or-dir)
              (define-key dired-mode-map (kbd "M-RET") 'helm-dired-current-file)
              (define-key dired-mode-map (kbd "C-M-j") 'tc-lister-open-file)
              (define-key dired-mode-map (vector 'remap 'beginning-of-buffer) 'dired-beginning-of-buffer)
              (define-key dired-mode-map (vector 'remap 'end-of-buffer) 'dired-end-of-buffer)
              (define-key dired-mode-map (kbd "C-c C-s") 'dired-do-isearch) ;对mark的多个文件内容进行查找
              (define-key dired-mode-map (kbd "C-c C-M-s") 'dired-do-isearch-regexp)
              (smartrep-define-key dired-mode-map "M-s" '(("m" . dired-duplicate-file)))
              (make-local-variable 'dired-sort-map)
              (setq dired-sort-map (make-sparse-keymap))
              (define-key dired-mode-map "s" dired-sort-map)
              (define-key dired-sort-map "s" #'(lambda () (interactive) (dired-sort-switch "S"))) ;sort by Size
              (define-key dired-sort-map "x" #'(lambda () (interactive) (dired-sort-switch "X"))) ;sort by eXtension
              (define-key dired-sort-map "t" #'(lambda () (interactive) (dired-sort-switch "t"))) ;sort by Time
              (define-key dired-sort-map "n" #'(lambda () (interactive) (dired-sort-switch "v"))))) ;sort by Name
  ;; ==========setup-and-keybindings===========
;;;; dired-sort-switch
  ;; ============dired-sort-switch=============
  (defvar dired-subdir-actual-switches nil)
  (put 'dired-subdir-actual-switches 'safe-local-variable 'dired-safe-switches-p)
  (setq dired-switches-in-mode-line (lambda (arg) (string-remove-prefix dired-listing-switches arg)))
  (defun dired-sort-switch (switch-arg)
    (if (dired-subtree-in-subtree-p)
        (let ((dired-listing-switches (concat dired-subdir-switches "A" switch-arg
                                              (when (string-suffix-p switch-arg dired-subdir-actual-switches) "r"))))
          (setq-local dired-subdir-actual-switches dired-listing-switches)
          (dired-subtree-revert))
      (dired-sort-other
       (concat dired-listing-switches switch-arg
               (when (string-suffix-p switch-arg dired-actual-switches) "r")))))
  ;; ============dired-sort-switch=============
;;;; sof/dired-sort
  ;; =============sof/dired-sort===============
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
  ;; (add-hook 'dired-after-readin-hook 'sof/dired-sort)
  ;; =============sof/dired-sort===============
;;;; dired-duplicate-file
  ;; ==========dired-duplicate-file============
  (defun dired-duplicate-file ()
    "Duplicate the current file in Dired."
    (interactive)
    (cl-loop for f in (dired-get-marked-files)
             do (let* ((ctr 1)
                       (base (file-name-sans-extension f))
                       (ext (file-name-extension f t))
                       (target (format "%s_[%d]%s" base ctr ext)))
                  (while (file-exists-p target)
                    (setq ctr (1+ ctr)
                          target (format "%s_[%d]%s" base ctr ext)))
                  (if (file-directory-p f)
                      (copy-directory f target)
                    (copy-file f target))))
    (revert-buffer))
  ;; ==========dired-duplicate-file============
  )
;; ==================dired=====================
;;; image-dired
;; ===============image-dired==================
(use-package image-dired
  ;; 默认以C-t为快捷键前缀
  ;; C-t C-t 当前dired中显示缩略图
  ;; C-t (./d/a) image-dired中显示(当前/选中/追加)文件缩略图
  ;; C-t (i/x) Emacs(内/外)查看图片文件
  ;; image-dired中m/u/d标记原文件，l/r/L/R旋转缩略图和原文件
  ;; 缩略图和数据库文件保存在~/.emacs.d/image-dired/下
  ;; 依赖：convert/mogrify/pngnq/pngcrush/optipng/jpegtran/exiftool/pdftoppm/ffmpegthumbnailer/openscad/unoconv/libreoffice
  :commands (global-image-dired-minor-mode image-dired-display-thumbs-all)
  :init
  (add-hook 'dired-mode-hook (lambda ()
                               (bind-key "C-M-i" #'(lambda () (interactive) (if (global-image-dired-minor-mode 'toggle)
                                                                                (image-dired-display-thumbs-all)
                                                                              (when (get-buffer image-dired-thumbnail-buffer)
                                                                                (kill-buffer image-dired-thumbnail-buffer))))
                                         dired-mode-map)))
  :config
  (require 'image-dired-dired)
  (bind-key "C-j" 'image-dired-thumbnail-display-external image-dired-thumbnail-mode-map)
  (bind-key [tab] nil image-dired-minor-mode-map)
  (setq image-dired-external-viewer "xdg-open"
        image-dired-queue-active-limit  ;最大进程数为逻辑cpu数量的1/2
        (/ (string-to-number (shell-command-to-string "cat /proc/cpuinfo | grep \"processor\" | wc -l")) 2)
        image-dired-cmd-create-thumbnail-options ; 使用-thumbnail替换-resize，并配置-define读取大小，以加快速度
        '("-define" "jpeg:size=%wx%h" "%f[0]" "-thumbnail" "%wx%h>" "-strip" "jpeg:%t")
        image-dired-cmd-create-temp-image-options image-dired-cmd-create-thumbnail-options)
  (define-globalized-minor-mode global-image-dired-minor-mode image-dired-minor-mode
    (lambda () (when (eq major-mode 'dired-mode) (image-dired-minor-mode 1)))
    :lighter " image-dired")
  (defvar image-dired-file-extension-all (append image-file-name-extensions
                                                 '("doc" "docx" "xls" "xlsx" "ppt" "pptx" "pdf" "3mf" "amf" "dxf" "off" "stl" "rmvb" "rm" "mp4" "avi" "flv" "f4v" "mpg" "mkv" "3gp" "wmv" "mov" "dat" "asf" "mpeg" "wma" "webm")))
  (defvar image-dired-file-regexp-all (concat "\\."
                                              (regexp-opt (nconc (mapcar #'upcase image-dired-file-extension-all)
                                                                 image-dired-file-extension-all)
                                                          t)
                                              "\\'"))
  (defun image-dired-refresh-thumbnail-buffer (&optional arg)
    (when (string-match-p image-dired-file-regexp-all (dired-get-filename))
      (if (and (get-buffer image-dired-thumbnail-buffer)
               (equal (dired-current-directory)
                      (buffer-local-value 'default-directory
                                          (get-buffer image-dired-thumbnail-buffer))))
          (if (image-dired-track-thumbnail)
              (display-buffer image-dired-thumbnail-buffer)
            (image-dired-display-thumbs t t t)
            (image-dired-track-thumbnail))
        (image-dired-display-thumbs-all))))
  (advice-add 'image-dired-dired-previous-line :after #'image-dired-refresh-thumbnail-buffer)
  (advice-add 'image-dired-dired-next-line :after #'image-dired-refresh-thumbnail-buffer)
  (defun image-dired-display-thumbs-all ()
    (when (get-buffer image-dired-thumbnail-buffer)
      (kill-buffer image-dired-thumbnail-buffer))
    (let* ((default-directory (dired-current-directory))
           (file-num (length (directory-files default-directory nil image-dired-file-regexp-all)))
           (mark-num (dired-mark-if (and (not (looking-at-p dired-re-dot))
                                         (not (eolp))
                                         (let ((fn (dired-get-filename t t)))
                                           (and fn (string-match-p image-dired-file-regexp-all fn)))
                                         ;; 只mark当前subdir的文件
                                         (equal default-directory (dired-current-directory))
                                         ;; 当文件数量>100时，预览前100张文件
                                         ;; count为宏dired-mark-if内部变量
                                         (not (and (> file-num 100) (> count 100))))
                                    "matching file")))
      (when (and mark-num (> mark-num 0))
        (image-dired-display-thumbs nil nil t)
        (dired-unmark-all-files ?*)
        (revert-buffer)
        (image-dired-track-thumbnail))))
  (defun image-dired-create-thumb-1/around (fn original-file thumbnail-file &rest args)
    (if (string-match-p (image-file-name-regexp) original-file)
        (apply fn original-file thumbnail-file args)
      (let* ((file-ext (ignore-errors (downcase (file-name-extension original-file))))
             (office-ext '("doc" "docx" "xls" "xlsx" "ppt" "pptx"))
             (pdf-ext '("pdf"))
             (cad-ext '("3mf" "amf" "dxf" "off" "stl"))
             (video-ext '("rmvb" "rm" "mp4" "avi" "flv" "f4v" "mpg" "mkv" "3gp" "wmv" "mov" "dat" "asf" "mpeg" "wma" "webm"))
             (process (start-process-shell-command "image-dired-create-thumbnail" nil
                                                   (format (cond
                                                            ((member file-ext office-ext)
                                                             ;; "unoconv -f pdf -e PageRange=1-1 --stdout \"%1$s\" | pdftoppm -jpeg -f 1 -l 1 | convert -define jpeg:size=100x100 - -thumbnail 100x100 -strip jpeg:\"%2$s\""
                                                             ;; libreoffice无法指定生成文件名字，也无法输出至stdout
                                                             (let* ((temp-dir (make-temp-name (expand-file-name image-dired-dir)))
                                                                    (temp-file (concat temp-dir "/" (file-name-base original-file) ".jpg")))
                                                               ;; 同时运行多个实例需加-env:UserInstallation，当转换时间超过5秒，自动终止
                                                               (concat "timeout 5 libreoffice \"-env:UserInstallation=file://" temp-dir "\" --headless --convert-to jpg --outdir \""
                                                                       temp-dir "\" \"%1$s\" && convert -define jpeg:size=100x100 \""
                                                                       temp-file "\" -thumbnail 100x100 -strip jpeg:\"%2$s\" || convert -font 黑体 label:\""
                                                                       (file-name-nondirectory original-file) "\" -thumbnail 100x100 -strip jpeg:\"%2$s\" ; rm -r \""
                                                                       temp-dir "\"")))
                                                            ((member file-ext pdf-ext)
                                                             ;; 使用convert转换pdf速度太慢
                                                             ;; "convert -define jpeg:size=100x100 \"%1$s\"[0] -thumbnail 100x100 -strip jpeg:%2$s"
                                                             "pdftoppm -jpeg -f 1 -l 1 \"%1$s\" | convert -define jpeg:size=100x100 - -thumbnail 100x100 -strip jpeg:\"%2$s\"")
                                                            ((member file-ext cad-ext)
                                                             (concat "openscad -o " image-dired-dir "image-dired.png"
                                                                     " <(echo \"import(\\\"%1$s\\\");\") && convert -define jpeg:size=100x100 "
                                                                     image-dired-dir "image-dired.png"
                                                                     " -thumbnail 100x100 -strip jpeg:\"%2$s\""))
                                                            ((member file-ext video-ext)
                                                             "ffmpegthumbnailer -i \"%1$s\" -t 0% -s 0 -c jpeg -o - | convert -define jpeg:size=100x100 - -thumbnail 100x100 -strip jpeg:\"%2$s\""))
                                                           original-file thumbnail-file))))
        (lexical-let ((thumbnail-file-temp thumbnail-file)
                      (original-file-temp original-file))
          (setf (process-sentinel process)
                (lambda (process status)
                  (cl-decf image-dired-queue-active-jobs)
                  (image-dired-thumb-queue-run)
                  (if (not (and (eq (process-status process) 'exit)
                                (zerop (process-exit-status process))))
                      (message "Thumb could not be created for %s: %s"
                               (abbreviate-file-name original-file-temp)
                               (replace-regexp-in-string "\n" "" status))
                    (set-file-modes thumbnail-file-temp #o600)
                    (clear-image-cache thumbnail-file-temp)))))
        process)))
  (advice-add 'image-dired-create-thumb-1 :around #'image-dired-create-thumb-1/around)
  (defun image-dired-thumb-name/around (fn file &rest args)
    (if (string-match-p (image-file-name-regexp) file)
        (apply fn file args)
      (concat (apply fn file args) ".jpg")))
  (advice-add 'image-dired-thumb-name :around #'image-dired-thumb-name/around))
;; ===============image-dired==================
;;; peep-dired
;; ================peep-dired==================
(use-package peep-dired
  :delight (peep-dired (:eval (propertize " P" 'face 'font-lock-function-name-face)))
  :commands (global-peep-dired peep-dired-find-file)
  :init
  (add-hook 'dired-mode-hook (lambda ()
                               (bind-key "M-i" #'(lambda () (interactive) (unless (global-peep-dired 'toggle)
                                                                            (delete-other-windows)
                                                                            (peep-dired-cleanup)
                                                                            (when (and (boundp 'image-dired-display-image-buffer)
                                                                                       (get-buffer image-dired-display-image-buffer))
                                                                              (kill-buffer image-dired-display-image-buffer))))
                                         dired-mode-map)))
  :config
  (define-globalized-minor-mode global-peep-dired peep-dired
    (lambda () (when (eq major-mode 'dired-mode) (peep-dired 1))))
  (defvar peep-preview-timer nil)
  (defvar ranger-scope-extensions (cl-remove-if
                                   (lambda (x)
                                     (member x (list "gp" "tex" "dot" "c")))
                                   (cl-loop for file-extension-pair in file-extension-app-alist
                                            collect (car file-extension-pair))))
  (defun peep-dired-find-file (file-name)
    (let ((image-extensions (list "png" "jpg" "bmp" "jpeg" "gif"))
          (cad-extensions (list "3mf" "amf" "dxf" "off" "stl"))
          (video-extensions (list "rmvb" "rm" "mp4" "avi" "flv" "f4v" "mpg" "mkv" "3gp" "wmv" "mov" "dat" "asf" "mpeg" "wma" "webm"))
          (file-extension (ignore-errors (downcase (file-name-extension file-name)))))
      (cond
       ((member file-extension image-extensions)
        (image-dired-display-image file-name)
        image-dired-display-image-buffer)
       ((member file-extension (append video-extensions cad-extensions))
        (shell-command (format (cond ((member file-extension video-extensions)
                                      "ffmpegthumbnailer -i \"%2$s\" -o %1$speep-dired.png -t 0%% -s 0")
                                     ((member file-extension cad-extensions)
                                      "openscad -o %1$speep-dired.png <(echo \"import(\\\"%2$s\\\");\")"))
                               image-dired-dir (expand-file-name file-name)))
        (image-dired-display-image (expand-file-name "peep-dired.png" image-dired-dir))
        image-dired-display-image-buffer)
       (t (let ((peep-preview-buffer (get-buffer-create "*peep-preview*")))
            (with-current-buffer peep-preview-buffer
              (buffer-disable-undo)
              (erase-buffer)
              (font-lock-mode -1)
              (if (member file-extension ranger-scope-extensions)
                  (insert (shell-command-to-string (format "%s %s 1000 100 '/tmp' 'False'"
                                                           (expand-file-name "repos/ranger/ranger/data/scope.sh" user-emacs-directory)
                                                           (replace-regexp-in-string "\\( \\|(\\|)\\|\\[\\|\\]\\|{\\|}\\)" "\\\\\\1" file-name))))
                (insert-file-contents file-name))
              (goto-char (point-min))
              (set-buffer-modified-p nil))
            (display-buffer peep-preview-buffer)
            peep-preview-buffer)))))
  (defun peep-dired-display-file-other-window/around (fn)
    (unless (timerp peep-preview-timer)
      (setq peep-preview-timer
            (run-with-idle-timer
             0.05 nil
             (lambda (func)
               (let ((file-entry-name (dired-file-name-at-point)))
                 (if (file-directory-p file-entry-name)
                     (funcall func)
                   (let ((peep-dired-preview-buffer (peep-dired-find-file file-entry-name)))
                     (add-to-list 'peep-dired-peeped-buffers
                                  (window-buffer (display-buffer peep-dired-preview-buffer t))))))
               (setq peep-preview-timer nil))
             fn))))
  (advice-add 'peep-dired-display-file-other-window :around #'peep-dired-display-file-other-window/around)
  (advice-add 'peep-dired-disable :override #'(lambda () (setq peep-preview-timer nil)))
  (setq peep-dired-enable-on-directories nil)
  (setq peep-dired-cleanup-on-disable t)
  (setq peep-dired-cleanup-eagerly nil)
  (define-key peep-dired-mode-map (kbd "p") 'peep-dired-prev-file)
  (define-key peep-dired-mode-map (kbd "n") 'peep-dired-next-file)
  (define-key peep-dired-mode-map (kbd "<SPC>") #'(lambda () (interactive)
                                                    (peep-dired-display-file-other-window)))
  (define-key peep-dired-mode-map (kbd "<backspace>") nil)
  (define-key peep-dired-mode-map (kbd "C-p") nil)
  (define-key peep-dired-mode-map (kbd "C-n") nil)
  (define-key peep-dired-mode-map (kbd "q") nil))
;; ================peep-dired==================
;;; dired-async
;; ================dired-async=================
(use-package dired-async
  :diminish dired-async-mode
  :commands dired-async-mode
  :init
  (add-hook 'dired-mode-hook #'(lambda () (dired-async-mode 1)))
  :config
  (defun dired-do-copy/before (&optional arg)
    "Redefine dired-do-copy to fix conflict between dired-async-mode and dired-sync-highlight."
    (let* ((fn-list (dired-get-marked-files nil arg))
           (fn-list-nodirectory (mapcar 'file-name-nondirectory fn-list))
           (annotated-file-list (if (dired-k--parse-status t)
                                    (hash-table-keys (dired-k--parse-status t))))
           (fn-list-annotated (cl-remove-if-not (lambda (x)
                                                  (member x annotated-file-list))
                                                fn-list-nodirectory))
           (annotation-storage-files
            (remove nil (cl-loop for from in fn-list
                                 collect (directory-files "~/org/annotated/" t
                                                          (concat "annotated-("
                                                                  (replace-regexp-in-string
                                                                   "/" "_"
                                                                   (substring-no-properties
                                                                    (abbreviate-file-name from) 1))))))))
      (when (or fn-list-annotated annotation-storage-files)
        (dired-async-mode 0))))
  (advice-add 'dired-do-copy :before #'dired-do-copy/before)
  (advice-add 'dired-do-copy :after #'(lambda (&optional arg) (dired-async-mode 1))))
;; ================dired-async=================
;;; dired-narrow
;; ===============dired-narrow=================
(use-package dired-narrow
  :bind (:map dired-mode-map
              ("/" . dired-narrow))
  :config
  (defun dired-narrow--string-filter/override (filter)
    (let ((words (split-string filter " ")))
      (--all? (save-excursion (or (search-forward it (line-end-position) t)
                                  (re-search-forward (pinyinlib-build-regexp-string it)
                                                     (line-end-position) t)))
              words)))
  (advice-add 'dired-narrow--string-filter :override #'dired-narrow--string-filter/override))
;; ===============dired-narrow=================
;;; dired-ranger
;; ===============dired-ranger=================
(use-package dired-ranger
  :bind (:map dired-mode-map
              ;; w: dired-copy-filename-as-kill 复制文件名，加0复制绝对路径
              ("M-w" . swint-dired-ranger-copy)
              ("C-c w" . swint-dired-clipboard-copy)
              ("C-c y" . swint-dired-clipboard-paste))
  :config
  (defun swint-dired-ranger-copy ()
    (interactive)
    (if (region-active-p)
        (easy-kill)
      (dired-copy-filename-as-kill 0)
      (call-interactively 'dired-ranger-copy)))
  ;; 由于dired-async对dired-create-files增加advice，导致复制多个文件时加C-u只保留第1个
  (defun swint-dired-ranger-paste ()
    (interactive)
    (cl-flet ((get-files () (mapcar #'(lambda (x)
                                        (mapconcat 'identity (cdr x) " "))
                                    (ring-elements dired-ranger-copy-ring))))
      (let ((selected-files (helm-comp-read "Paste: " (get-files)
                                            :marked-candidates t
                                            :buffer "*helm dired ranger paste-swint*")))
        (cl-loop for f in selected-files do
                 (let ((index (cl-position f (get-files) :test 'equal)))
                   (dired-ranger-paste index)
                   (unless helm-current-prefix-arg
                     (ring-remove dired-ranger-copy-ring index)))))))
  (defun swint-dired-ranger-move ()
    (interactive)
    (cl-flet ((get-files () (mapcar #'(lambda (x)
                                        (mapconcat 'identity (cdr x) " "))
                                    (ring-elements dired-ranger-copy-ring))))
      (let ((selected-files (helm-comp-read "Move: " (get-files)
                                            :marked-candidates t
                                            :buffer "*helm dired ranger move-swint*")))
        (cl-loop for f in selected-files do
                 (let ((index (cl-position f (get-files) :test 'equal)))
                   (dired-ranger-move index)
                   (unless helm-current-prefix-arg
                     (ring-remove dired-ranger-copy-ring index)))))))
  (defun swint-dired-clipboard-copy (&optional filetocopy) ;导致界面卡死，可粘贴图片；C-g杀死xclip进程，无法复制
    (interactive)
    (let ((filename (or filetocopy
                        (dired-get-filename nil t))))
      ;; xclip复制大图片时卡住
      ;; (shell-command (format "xclip -i -selection clipboard -t \"$(file -b --mime-type %s)\" %s" filename filename))
      (when (file-exists-p filename)
        (shell-command (format "copyq copy \"$(file -b --mime-type \"%s\")\" - < \"%s\"" filename filename)))))
  (defun swint-dired-clipboard-paste (&optional filetopaste) ;只可粘贴图片
    (interactive)
    (let ((filename (or filetopaste
                        (format-time-string "%Y%m%d_%H%M%S.png"))))
      (shell-command (format "xclip -selection clipboard -t image/png -o > \"%s\""
                             filename))))
  ;; 加C-u不清除clipboards
  (bind-key "C-y" 'dired-ranger-paste dired-mode-map)
  (bind-key "M-y" 'dired-ranger-move dired-mode-map)
  (bind-key "C-M-y" 'swint-dired-ranger-paste dired-mode-map)
  (bind-key "M-Y" 'swint-dired-ranger-move dired-mode-map))
;; ===============dired-ranger=================
;;; neotree
;; =================neotree====================
(use-package neotree
  :bind ("C-x j" . neotree-project-or-current-dir)
  :config
  (setq neo-smart-open nil)
  (setq neo-show-hidden-files t)
  (setq neo-confirm-change-root 'off-p)
  (setq neo-window-fixed-size nil)
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow))
  (define-key neotree-mode-map "\C-j" 'neotree-shell-command)
  (define-key neotree-mode-map (kbd "RET") (neotree-make-executor :file-fn 'neo-open-file
                                                                  :dir-fn 'neo-open-dired))
  (define-key neotree-mode-map (kbd "<backtab>") (neotree-make-executor
                                                  :dir-fn  'neo-open-dir-recursive))
  (define-key neotree-mode-map (kbd "a") 'neotree-stretch-toggle)
  (define-key neotree-mode-map (kbd "u") 'neotree-select-up-node)
  (define-key neotree-mode-map (kbd "d") 'neotree-select-down-node)
  (define-key neotree-mode-map (kbd "b") 'neotree-select-previous-sibling-node)
  (define-key neotree-mode-map (kbd "f") 'neotree-select-next-sibling-node)
  (define-key neotree-mode-map (kbd "h") 'neotree-hidden-file-toggle)
  (defun neotree-project-or-current-dir ()
    "Open NeoTree using project root or current directory."
    (interactive)
    (let ((project-dir (ignore-errors (projectile-project-root)))
          (find-file-name (if (eq major-mode 'dired-mode)
                              (dired-get-filename nil t)
                            (buffer-file-name)))
          (current-dir default-directory))
      (if (neo-global--window-exists-p)
          (if (eql (window-buffer neo-global--window)
                   (current-buffer))
              (neotree-hide)
            (neo-global--select-window))
        (neotree-show)
        (neotree-dir (or project-dir current-dir))
        (when find-file-name
          (neotree-find find-file-name)))))
  (defun neotree-shell-command ()
    "Open file with external app."
    (interactive)
    (let ((file (neo-buffer--get-filename-current-line)))
      (dired-async-shell-command file))))
;; =================neotree====================
;;; dired-du
;; ================dired-du====================
(use-package dired-du
  :bind (:map dired-mode-map
              ("V" . dired-du-mode))
  :config
  (setq dired-du-bind-mode nil
        dired-du-bind-human-toggle nil
        dired-du-bind-count-sizes nil
        dired-du-size-format t))
;; ================dired-du====================
;;; dired-subtree
;; =============dired-subtree==================
(use-package dired-subtree
  :commands dired-subtree-in-subtree-p
  :bind (:map dired-mode-map
              ("TAB" . dired-subtree-toggle)
              ("<backtab>" . dired-subtree-cycle))
  :config
  (defface dired-subtree-depth-7-face '((t :inherit dired-subtree-depth-1-face)) "" :group 'dired-subtree-faces)
  (defface dired-subtree-depth-8-face '((t :inherit dired-subtree-depth-2-face)) "" :group 'dired-subtree-faces)
  (defface dired-subtree-depth-9-face '((t :inherit dired-subtree-depth-3-face)) "" :group 'dired-subtree-faces)
  (defface dired-subtree-depth-10-face '((t :inherit dired-subtree-depth-4-face)) "" :group 'dired-subtree-faces)
  (defface dired-subtree-depth-11-face '((t :inherit dired-subtree-depth-5-face)) "" :group 'dired-subtree-faces)
  (defface dired-subtree-depth-12-face '((t :inherit dired-subtree-depth-6-face)) "" :group 'dired-subtree-faces)
  (define-key dired-mode-map (kbd "S-TAB") #'(lambda () (interactive)
                                               (while (dired-subtree-in-subtree-p) (dired-subtree-remove))
                                               (when (dired-subtree--is-expanded-p) (dired-subtree-toggle))))
  (advice-add 'dired-subtree-insert :around #'(lambda (fn) (when (and (dired-subtree--dired-line-is-directory-or-link-p)
                                                                      (not (directory-empty-p (dired-utils-get-filename))))
                                                             (funcall fn))))
  (advice-add 'dired-subtree-next-sibling :around #'(lambda (fn &rest args) (let ((current-point (point))
                                                                                  (current-ov (dired-subtree--get-ov))
                                                                                  (orig-ret (apply fn args)))
                                                                              (when (/= (dired-subtree--get-depth (dired-subtree--get-ov))
                                                                                        (dired-subtree--get-depth current-ov))
                                                                                (goto-char current-point))
                                                                              (not (equal current-point (point))))))
  (advice-add 'dired-subtree-previous-sibling :around #'(lambda (fn &rest args) (let ((current-point (point))
                                                                                      (current-ov (dired-subtree--get-ov))
                                                                                      (orig-ret (apply fn args)))
                                                                                  (while (< (dired-subtree--get-depth current-ov)
                                                                                            (dired-subtree--get-depth (dired-subtree--get-ov)))
                                                                                    (dired-subtree-up))
                                                                                  (not (equal current-point (point))))))
  (defun dired-subtree-in-subtree-p ()
    "Return non-nil if current file is in subtree."
    (save-excursion
      (when (dired-utils-get-filename)
        (> (dired-subtree--get-depth (dired-subtree--get-ov)) 0))))
  ;; C-M-p/n/u/d
  (cl-loop for (key . value) in '((dired-prev-subdir . dired-subtree-previous-sibling)
                                  (dired-next-subdir . dired-subtree-next-sibling)
                                  (dired-tree-up . dired-subtree-up)
                                  (dired-tree-down . dired-subtree-down))
           do (lexical-let ((value value))
                (advice-add key :around #'(lambda (fn &rest args) (if dired-subtree-overlays
                                                                      (call-interactively value)
                                                                    (apply fn args))))))
  ;; *s/U/M-</M->
  (cl-loop for (key . value) in '((dired-mark-subdir-files . dired-subtree-mark-subtree)
                                  (dired-unmark-all-marks . dired-subtree-unmark-subtree)
                                  (dired-beginning-of-buffer . dired-subtree-beginning)
                                  (dired-end-of-buffer . dired-subtree-end))
           do (lexical-let ((value value))
                (advice-add key :around #'(lambda (fn &rest args) (if (and (not (eq last-command this-command))
                                                                           (dired-subtree-in-subtree-p))
                                                                      (call-interactively value)
                                                                    (apply fn args)))))))
;; =============dired-subtree==================
;;; all-the-icons-dired
;; ==========all-the-icons-dired===============
(use-package all-the-icons-dired
  :diminish all-the-icons-dired-mode
  :commands swint-init-all-the-icons-dired
  :init
  (if (and (fboundp 'daemonp) (daemonp))
      (add-hook 'after-make-frame-functions 'swint-init-all-the-icons-dired)
    (add-hook 'window-setup-hook 'swint-init-all-the-icons-dired))
  :config
  (setq all-the-icons-dired-monochrome nil)
  (defun swint-init-all-the-icons-dired (&optional frame)
    (let ((dired-buffer-list (cl-remove-if-not (lambda (x)
                                                 (equal (buffer-mode x) 'dired-mode))
                                               (buffer-list))))
      (if (display-graphic-p frame)
          (progn (add-hook 'dired-mode-hook 'all-the-icons-dired-mode)
                 (dolist (buf dired-buffer-list)
                   (with-current-buffer buf
                     (all-the-icons-dired-mode 1)
                     (revert-buffer))))
        (progn (remove-hook 'dired-mode-hook 'all-the-icons-dired-mode)
               (dolist (buf dired-buffer-list)
                 (with-current-buffer buf
                   (all-the-icons-dired-mode -1)
                   (revert-buffer)))))))
  (defun all-the-icons-dired--put-icon/override (pos)
    "Propertize POS with icon."
    (let* ((file (dired-get-filename 'relative 'noerror))
           (icon (all-the-icons-dired--icon file)))
      (put-text-property (1- pos) pos 'display
                         (if (member file '("." ".."))
                             "   \t"
                           (concat " " icon "\t")))))
  (advice-add 'all-the-icons-dired--put-icon :override #'all-the-icons-dired--put-icon/override)
  (advice-add 'all-the-icons-dired--setup :before #'(lambda () (setq-local tab-width 1))))
;; ==========all-the-icons-dired===============
;;; find-dired
;; ===============find-dired===================
(use-package find-dired
  :commands (find-dired-fd find-dired-rg)
  :init
  (bind-key "C-x d" nil)
  (bind-key "C-x d d" 'dired)
  (bind-key "C-x d f" 'find-dired-fd)
  (bind-key "C-x d g" 'find-dired-rg)
  :config
  (setq find-ls-option find-ls-option-default-ls)
  (defvar find-dired-fd-args nil)
  (defvar find-dired-fd-args-history nil)
  (defconst find-dired-fd-pre-args " --color never ")
  (defconst find-dired-rg-pre-args " --color never --files-with-matches -0 --regexp ")
  (defun find-dired-fd (dir args)
    "Run `fd' and go into Dired mode on a buffer of the output.
The default command run is fd ARGS -l."
    (interactive (list
                  (if current-prefix-arg
                      (read-directory-name "Run fd in directory: " nil "" t)
                    (helm-current-directory))
                  (read-string "Run fd (with args): " find-dired-fd-args
                               (if find-dired-fd-args
                                   '(find-dired-fd-args-history . 1)
                                 'find-dired-fd-args-history))))
    (setq find-dired-fd-args args
          args (concat "fdfind" find-dired-fd-pre-args args " -l"))
    (find-dired-with-command dir args))
  (defun find-dired-rg (dir regexp)
    "Find files in DIR that contain matches for REGEXP and Dired on output.
The default command run is fd -X rg -l0 --regexp REGEXP | xargs -0 ls."
    (interactive "DFd-rg (directory): \nsFd-rg (rg regexp): ")
    (find-dired-with-command dir
                             (concat "fdfind" find-dired-fd-pre-args
                                     "-X rg" find-dired-rg-pre-args
                                     (shell-quote-argument regexp)
                                     " | xargs -0 ls "
                                     (cdr find-ls-option)))))
;; ===============find-dired===================
(provide 'setup_dired)
