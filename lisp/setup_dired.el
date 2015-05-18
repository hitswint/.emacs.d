;;=============================================dired==========================================
(setq dired-recursive-copies 'top)
(setq dired-recursive-deletes 'top)
;; (add-to-list 'load-path "~/.emacs.d/dired")
(require 'dired-x)
(require 'dired-details)
(dired-details-install)
;; Auto refresh buffers
(global-auto-revert-mode 1)
;; Also auto refresh dired, but be quiet about it
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)
(put 'dired-find-alternate-file 'disabled nil)
;; 让dired显示文件大小
(setq dired-listing-switches "-alh")
;; (define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file) ; was dired-advertised-find-file
(define-key dired-mode-map (kbd "r") (lambda () (interactive) (find-alternate-file ".."))) ; was dired-up-directory
;; 在dired对mark的多个文件内容进行查找
(define-key dired-mode-map (kbd "C-c C-s") 'dired-do-isearch)
(define-key dired-mode-map (kbd "C-c C-M-s") 'dired-do-isearch-regexp)
;; 将annotated显示加hook放在前面，使其出现在dired-after-readin-hook中函数列表最后，进而最后生效。
(add-hook 'dired-after-readin-hook 'dired-k--highlight)
(define-key dired-mode-map (kbd "C-c C-p") 'dired-k--previous-annotated-file)
(define-key dired-mode-map (kbd "C-c C-n") 'dired-k--next-annotated-file)
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
                             (make-local-variable  'dired-sort-map)
                             (setq dired-sort-map (make-sparse-keymap))
                             (define-key dired-mode-map "s" dired-sort-map)
                             (define-key dired-sort-map "s"
                               '(lambda () ;"sort by Size"
                                  (interactive) (dired-sort-other (concat dired-listing-switches "S"))))
                             (define-key dired-sort-map "x"
                               '(lambda () ;"sort by eXtension"
                                  (interactive) (dired-sort-other (concat dired-listing-switches "X"))))
                             (define-key dired-sort-map "t"
                               '(lambda () ;"sort by Time"
                                  (interactive) (dired-sort-other (concat dired-listing-switches "t"))))
                             (define-key dired-sort-map "n"
                               '(lambda () ;"sort by Name"
                                  (interactive) (dired-sort-other (concat dired-listing-switches ""))))))
;; =====================文件夹排序=======================
;; =====================跳转至dired顶部和尾部==================
(defun dired-back-to-top ()
  (interactive)
  (beginning-of-buffer)
  (dired-next-line 4))
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
;; ==============tab用默认方式打开文件===================
(add-hook 'dired-mode-hook
          (lambda ()
            (setq truncate-lines t)
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
              ("doc" . "wps") ("ppt" . "wpp")("xls" . "et")("ods" . "wps")("odt" . "wps")
              ("docx" . "wps") ("pptx" . "wpp")("xlsx" . "et")
              ("dxf" . "librecad")
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
      (start-process "Shell" nil shell-file-name shell-command-switch (concat command " " "\"" file "\"")))))
;; ==============tab用默认方式打开文件===================
;; 设置一些文件的默认打开方式，此功能必须在(require 'dired-x)之后
(setq dired-guess-shell-alist-user
      (list
       (list "\\.pdf$" "evince * >/dev/null 2>&1 &")
       (list "\\.doc$" "libreoffice * >/dev/null 2>&1 &")
       (list "\\.docx$" "libreoffice * >/dev/null 2>&1 &")
       (list "\\.ppt$" "libreoffice * >/dev/null 2>&1 &")
       (list "\\.pptx$" "libreoffice * >/dev/null 2>&1 &")
       (list "\\.xls$" "libreoffice * >/dev/null 2>&1 &")
       (list "\\.xlsx$" "libreoffice * >/dev/null 2>&1 &")
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
       (list "\\.c$" "gcc -Wall")
       )
      )
(add-hook 'dired-mode-hook
          '(lambda ()
             (define-key dired-mode-map [(meta =)] nil)))
;; =========================文件夹之间复制================================
(setq dired-dwim-target t)              ; 从一个文件夹拷贝到打开的另一个文件夹
;; =========================文件夹之间复制================================
;; ===============在当前目录下打开urxvt===============
(defun urxvt-cd-directory (path)
  (interactive)
  (start-process "Shell" nil shell-file-name shell-command-switch (concat "tabbed -c " "urxvt" " -cd " "\"" path "\"" " -embed"))
  )
(defun urxvt-for-dired-file ()
  (interactive)
  (urxvt-cd-directory (dired-current-directory)))
(defun urxvt-for-file ()
  (interactive)
  (urxvt-cd-directory (file-name-directory (buffer-file-name))))
(global-set-key (kbd "<C-s-return>") 'urxvt-for-file)
(add-hook 'dired-mode-hook
          (lambda ()
            (setq truncate-lines t)
            (define-key dired-mode-map (kbd "<C-s-return>") 'urxvt-for-dired-file)))
;; ===============在当前目录下打开urxvt===============
;;=============================================dired==========================================
(provide 'setup_dired)
