;;; dired
;; ==================dired=====================
(def-package! dired
  :config
  (def-package! dired-x)
  (def-package! dired-details
    :config
    (dired-details-install)
    (setq dired-omit-verbose nil)
    (setq dired-omit-size-limit nil)
    (add-hook 'dired-mode-hook (lambda () (dired-omit-mode)))
    (setq dired-details-hidden-string "")
    (advice-add 'dired-details-show :after #'(lambda () (dired-hide-details-mode 0)))
    (advice-add 'dired-details-hide :after #'(lambda () (dired-hide-details-mode 1))))
  (def-package! dired-filetype-face)
  (setq dired-recursive-copies 'top)
  (setq dired-recursive-deletes 'always)
  (custom-set-faces '(diredp-compressed-file-suffix ((t (:foreground "#7b68ee"))) t)
                    '(diredp-ignored-file-name ((t (:foreground "#aaaaaa"))) t))
;;;; setup-and-keybindings
  ;; ==========setup-and-keybindings===========
  (setq dired-auto-revert-buffer t)
  (put 'dired-find-alternate-file 'disabled nil)
  ;; 显示文件大小。
  (setq dired-listing-switches "-alh")
  ;; 文件夹间复制。
  (setq dired-dwim-target t)
  ;; Allow editing file permissions.
  (setq wdired-allow-to-change-permissions t)
  (add-hook 'wdired-mode-hook 'undo-tree-mode)
  ;; 不折行显示。
  (add-hook 'dired-after-readin-hook '(lambda () (setq truncate-lines t)))
  ;; 默认打开方式。
  (setq file-extension-app-alist
        '(("pdf" . "llpp_qpdfview.sh") ("djvu" . "llpp")
          ("rmvb" . "mplayer") ("rm" . "mplayer") ("mp4" . "mplayer") ("avi" . "mplayer") ("flv" . "mplayer") ("f4v" . "mplayer") ("mpg" . "mplayer") ("mkv" . "mplayer") ("3gp" . "mplayer") ("wmv" . "mplayer") ("mov" . "mplayer") ("dat" . "mplayer") ("asf" . "mplayer") ("mpeg" . "mplayer") ("wma" . "mplayer") ("gif" . "mplayer")
          ("mp3" . "mpg321") ("ape" . "mplayer")
          ("xoj" . "xournal")
          ("jpg" . "feh.sh") ("png" . "feh.sh") ("bmp" . "feh.sh") ("jpeg" . "feh.sh")
          ("eps" . "gv") ("ps" . "gv")
          ("html" . "firefox") ("htm" . "firefox")
          ("doc" . "wps") ("docx" . "wps")
          ("xls" . "et") ("xlsx" . "et")
          ("ppt" . "wpp") ("pptx" . "wpp")
          ("ods" . "libreoffice")("odt" . "libreoffice")
          ("dwg" . "cad-2004.sh") ("dxf" . "cad-2004.sh")
          ("caj" . "caj.sh") ("nh" . "caj.sh") ("kdh" . "caj.sh")
          ("gp" . "gnuplot")
          ("rar" . "unrar x -o+")
          ("zip" . "unar")
          ("gz" . "tar zvxf") ("tgz" . "tar zvxf") ("bz2" . "tar jvxf") ("tar" . "tar xf")
          ("tex" . "xelatex")
          ("dot" . "dot -Tpng -o dot.png")
          ("c" . "gcc -Wall")
          ("dia" . "dia")))
  ;; dired-do-shell-command打开方式。
  (setq async-shell-command-buffer 'new-buffer)
  (setq async-shell-command-display-buffer nil)
  (setq dired-guess-shell-alist-user
        (list
         (list "\\.pdf$" "adobe.sh * >/dev/null 2>&1 &")
         (list "\\.doc$" "word.sh * >/dev/null 2>&1 &")
         (list "\\.docx$" "word.sh * >/dev/null 2>&1 &")
         (list "\\.ppt$" "ppt.sh * >/dev/null 2>&1 &")
         (list "\\.pptx$" "ppt.sh * >/dev/null 2>&1 &")
         (list "\\.xls$" "excel.sh * >/dev/null 2>&1 &")
         (list "\\.xlsx$" "excel.sh * >/dev/null 2>&1 &")
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
         (list "\\.c$" "gcc -Wall * >/dev/null 2>&1 &")
         (list "\\.ipynb$" "jupyter nbconvert --to python * >/dev/null 2>&1 &")))
  (add-hook 'dired-mode-hook
            (lambda ()
              (define-key dired-mode-map (kbd "M-=") nil)
              (define-key dired-mode-map (kbd "e") (lambda ()
                                                     (interactive)
                                                     (find-file-literally (dired-get-file-for-visit))))
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
              (define-key dired-mode-map (kbd "C-j") 'dired-async-shell-command-on-files)
              (define-key dired-mode-map (kbd "C-c c") 'swint-dired-cad-converter)
              (define-key dired-mode-map (kbd "C-c C") '(lambda () (interactive)
                                                          (swint-dired-cad-converter t)))
              (define-key dired-mode-map (kbd "v") 'txm-dired-view-file-or-dir)
              (define-key dired-mode-map (kbd "M-RET") 'helm-dired-current-file)
              (define-key dired-mode-map (kbd "C-M-j") 'tc-lister-open-file)
              (define-key dired-mode-map (vector 'remap 'beginning-of-buffer) 'dired-beginning-of-buffer)
              (define-key dired-mode-map (vector 'remap 'end-of-buffer) 'dired-end-of-buffer)
              ;; 在dired对mark的多个文件内容进行查找。
              (define-key dired-mode-map (kbd "C-c C-s") 'dired-do-isearch)
              (define-key dired-mode-map (kbd "C-c C-M-s") 'dired-do-isearch-regexp)
              ;; 文件夹排序。
              (make-local-variable 'dired-sort-map)
              (setq dired-sort-map (make-sparse-keymap))
              (define-key dired-mode-map "s" dired-sort-map)
              (define-key dired-sort-map "s" '(lambda () (interactive) ;"sort by Size"
                                                (dired-sort-other (concat dired-listing-switches "S"))))
              (define-key dired-sort-map "x" '(lambda () (interactive) ;"sort by eXtension"
                                                (dired-sort-other (concat dired-listing-switches "X"))))
              (define-key dired-sort-map "t" '(lambda () (interactive) ;"sort by Time"
                                                (dired-sort-other (concat dired-listing-switches "t"))))
              (define-key dired-sort-map "n" '(lambda () (interactive) ;"sort by Name"
                                                (dired-sort-other (concat dired-listing-switches ""))))))
  ;; ==========setup-and-keybindings===========
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
;;;; dired-next/previous-line
  ;; ========dired-next/previous-line==========
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
  ;; ========dired-next/previous-line==========
  )
;; ==================dired=====================
;;; peep-dired
;; ================peep-dired==================
(def-package! peep-dired
  ;; image-dired: Use C-t as prefix.
  :bind (:map dired-mode-map
              ("q" . peep-dired))
  :config
  (defun peep-dired-display-file-other-window/around (fn)
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
  (advice-add 'peep-dired-display-file-other-window :around #'peep-dired-display-file-other-window/around)
  (advice-add 'peep-dired-disable :after #'(lambda () (if (and (boundp 'image-dired-display-image-buffer)
                                                               (get-buffer image-dired-display-image-buffer))
                                                          (swint-kill-buffer image-dired-display-image-buffer))))
  (setq peep-dired-enable-on-directories nil)
  (define-key peep-dired-mode-map (kbd "p") 'peep-dired-prev-file)
  (define-key peep-dired-mode-map (kbd "n") 'peep-dired-next-file)
  (define-key peep-dired-mode-map (kbd "C-p") nil)
  (define-key peep-dired-mode-map (kbd "C-n") nil))
;; ================peep-dired==================
;;; dired-async
;; ================dired-async=================
(def-package! dired-async
  :diminish dired-async-mode
  :commands dired-async-mode
  :init
  (add-hook 'dired-mode-hook '(lambda () (dired-async-mode 1)))
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
(def-package! dired-narrow
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
(def-package! dired-ranger
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
(def-package! neotree
  :bind ("C-x j" . neotree-project-or-current-dir)
  :config
  (setq neo-smart-open nil)
  (setq neo-show-hidden-files nil)
  (setq neo-confirm-change-root 'off-p)
  (setq neo-window-fixed-size nil)
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
      (dired-async-shell-command file))))
;; =================neotree====================
(provide 'setup_dired)
