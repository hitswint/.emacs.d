;;; org-mode
;; =================org-mode====================
(use-package org
  :mode ("\\.[oO][rR][gG]\\'" . org-mode)
  :init
  (setq org-babel-key-prefix (kbd "C-c C-'"))
  :config
  (setq org-modules nil)                ;随org启动的ol模块
  (defvar org-outline-cmd-list '(org-previous-visible-heading
                                 org-next-visible-heading
                                 org-backward-heading-same-level
                                 org-forward-heading-same-level
                                 org-previous-item
                                 org-next-item
                                 swint-org-previous-item
                                 swint-org-next-item
                                 org-beginning-of-item-list
                                 org-end-of-item-list
                                 outline-previous-visible-heading
                                 outline-next-visible-heading
                                 outline-backward-same-level
                                 outline-forward-same-level))
  (dolist (cmd org-outline-cmd-list)
    (lexical-let ((cmd cmd))
      (advice-add cmd :before #'(lambda (&rest arg)
                                  (and (eq this-command cmd)
                                       (or (member last-command org-outline-cmd-list) (push-mark)))))))
;;;; Appearance
  ;; =================Appearance================
  (set-face-attribute 'org-level-1 nil :height 1.2)
  (set-face-attribute 'org-level-5 nil :weight 'normal :foreground "cyan" :height 1.0)
  (set-face-attribute 'org-level-6 nil :weight 'normal :foreground "violet" :height 1.0)
  (set-face-attribute 'org-level-7 nil :weight 'normal :foreground "orange" :height 1.0)
  (set-face-attribute 'org-level-8 nil :weight 'normal :foreground "khaki" :height 1.0)
  (setq org-hide-emphasis-markers t)
  (font-lock-add-keywords 'org-mode
                          '(("^ +\\([-*]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))
  (setq org-use-sub-superscripts "{}")
  ;; 使用org-toggle-pretty-entities (C-c C-x \) 启闭
  (setq org-pretty-entities t
        org-pretty-entities-include-sub-superscripts t
        org-adapt-indentation t)
  ;; =================Appearance================
;;;; Capture
  ;; =================Capture===================
  (global-set-key (kbd "M-O l") 'org-store-link)
  (global-set-key (kbd "M-O L") #'(lambda () (interactive)
                                    (cl-letf (((symbol-value 'org-create-file-search-functions)
                                               (lambda ()
                                                 (number-to-string (org-current-line)))))
                                      (call-interactively 'org-store-link))))
  (global-set-key (kbd "M-O c") 'org-capture)
  (global-set-key (kbd "M-O a") 'org-agenda)
  (setq org-capture-templates
        '(("i" "Idea" entry (file+headline "~/webdav-sync/orgzly/task.org" "Idea") "* TODO %? %^g")
          ("w" "Work" entry (file+headline "~/org/notes-work.org" "Work") "* %? %U %^g")
          ("c" "Computer" entry (file+headline "~/org/notes-computer.org" "Computer") "* %? %U %^g")
          ("j" "Journal" entry (file+olp+datetree "~/org/journal.org.gpg") "* %? %U")))
  ;; =================Capture===================
;;;; Keybindings
  ;; ===============Keybindings=================
  ;; %^{Description}
  (add-hook 'org-mode-hook
            #'(lambda ()
                ;; 插入source code时高亮，C-c '打开相应major-mode编辑窗口
                (setq org-src-fontify-natively t)
                ;; (setq org-startup-indented t)
                (setq truncate-lines nil)
                (setq org-hide-leading-stars t)
                (setq org-list-allow-alphabetical t)
                (setq org-startup-folded 'showeverything)
                ;; org-mode先于imenu加载时未设置org-imenu-get-tree
                (setq imenu-create-index-function 'org-imenu-get-tree)
                (setq org-imenu-depth 8)
                (setq org-special-ctrl-a/e t)
                (setq org-special-ctrl-o t)
                (setq org-special-ctrl-k t)
                (setq org-capture-bookmark nil)
                (setq org-file-apps (append
                                     '((auto-mode . emacs)
                                       (directory . "thunar"))
                                     (cl-loop for file-extension-pair in file-extension-app-alist
                                              collect (cons
                                                       (concat "\\." (car file-extension-pair) "\\'")
                                                       (concat (cdr file-extension-pair) " %s")))))
                ;; 如果有#+ATTR_ORG: :width 100则设置为图片宽度为100，否则显示原尺寸
                (setq org-image-actual-width nil)
                ;; org-redisplay-inline-images (C-c C-x C-M-v) 更新图片
                ;; org-toggle-inline-images (C-c C-x C-v) 开关图片
                (org-display-inline-images)
                (setq org-use-speed-commands t)
                ;; ? org-speed-command-help
                ;; n/p/f/b/u navigation commands
                ;; U/D/R/L org-shiftmeta up/down/right/left
                ;; c org-cycle
                ;; C org-shifttab
                ;; @ org-mark-subtree
                ;; i C-RET
                ;; w org-refile
                ;; I/O org-clock-in/out
                ;; t org-todo
                ;; ;/0/1/2/3 org-priority/A/B/C
                ;; v org-agenda
                ;; : org-set-tags-command
                (add-to-list 'org-speed-commands '("P" . org-eaf-pdf-sync-prev))
                (add-to-list 'org-speed-commands '("N" . org-eaf-pdf-sync-next))
                (add-to-list 'org-speed-commands '("O" . org-eaf-pdf-sync))
                (add-to-list 'org-speed-commands '("I" . swint-annotate-new))
                (define-key org-mode-map (kbd "C-c e") 'org-beamer-select-environment)
                (define-key org-mode-map (kbd "C-c C-v") 'swint-open-output-file)
                (define-key org-mode-map (kbd "C-c j") 'swint-org-open-at-point)
                (define-key org-mode-map (kbd "C-c o") #'(lambda () (interactive) (swint-org-open-at-point t)))
                (define-key org-mode-map (kbd "C-c O") 'swint-org-open-dired-at-point)
                (smartrep-define-key org-mode-map "M-s"
                  '(("p" . org-previous-visible-heading)
                    ("n" . org-next-visible-heading)
                    ("u" . outline-up-heading)
                    ("b" . org-backward-heading-same-level)
                    ("f" . org-forward-heading-same-level)
                    ("P" . org-eaf-pdf-sync-prev)
                    ("N" . org-eaf-pdf-sync-next)
                    ("O" . org-eaf-pdf-sync)
                    ("I" . swint-annotate-new)))
                (smartrep-define-key org-mode-map "C-c"
                  '(("p" . swint-org-previous-item)
                    ("n" . swint-org-next-item)
                    ("P" . org-beginning-of-item-list)
                    ("N" . org-end-of-item-list)
                    ("d" . org-down-element)
                    ("u" . org-up-element)))
                (define-key org-mode-map (kbd "C-a") #'(lambda () (interactive)
                                                         (if (or (org-at-heading-p) (org-at-item-p))
                                                             (call-interactively 'org-beginning-of-line)
                                                           (call-interactively 'smart-beginning-of-line))))
                (define-key org-mode-map (kbd "C-c m") 'helm-insert-latex-math)
                (define-key org-mode-map (kbd "C-c l") #'(lambda () (interactive)
                                                           (insert (swint-cursor-localtion))))
                (define-key org-mode-map (kbd "C-c w") 'org-clipboard-copy)
                (define-key org-mode-map (kbd "C-c y") 'org-clipboard-paste)
                (define-key org-mode-map (kbd "C-c r") 'helm-org-ref-link)
                (define-key org-mode-map (kbd "C-c C-x C-\\") 'org-toggle-link-display)
                (define-key org-mode-map (kbd "C-j") #'(lambda () (interactive)
                                                         (if (org-in-regexp org-link-any-re nil t)
                                                             (swint-org-open-at-point)
                                                           (call-interactively 'open-line-or-new-line-dep-pos))))
                (define-key org-mode-map (kbd "C-o") #'(lambda () (interactive)
                                                         (if (org-in-regexp org-link-any-re nil t)
                                                             (swint-org-open-at-point t)
                                                           (call-interactively 'org-open-line))))
                (define-key org-mode-map (kbd "RET") nil)
                (define-key org-mode-map [(control \,)] nil)
                (define-key org-mode-map [(control \.)] nil)
                (define-key org-mode-map [(control \#)] nil)
                (define-key org-mode-map [(control tab)] nil)
                (define-key org-mode-map [(control \')] nil)))
  ;; ===============Keybindings=================
;;;; org-agenda
  ;; ===============org-agenda==================
  (setq org-agenda-files (directory-files "~/webdav-sync/orgzly/" t ".+\\.org")
        org-agenda-span 'month)
  ;; 设定todo的子项完成后主项自动完成
  (add-hook 'org-after-todo-statistics-hook #'(lambda (n-done n-not-done)
                                                (let (org-log-done org-log-states)
                                                  (org-todo (if (= n-not-done 0) "DONE" "TODO")))))
  ;; 设置todo关键词。|后面的项以绿颜色的字出现，(a!/@)：()中出现!和@分别代表记录状态改变的时间以及需要输入备注，多个状态时使用/分隔
  (setq org-todo-keywords
        '((sequence "TODO(t)" "WAITING(w)" "STARTED(s)" "|" "DONE(d)" "ABORTED(a)")))
  (add-hook 'org-agenda-finalize-hook #'(lambda () (save-some-buffers t 'org-agenda-file-p)))
  (add-hook 'org-agenda-mode-hook
            #'(lambda ()
                (define-key org-agenda-mode-map (kbd "C-c C-x C-q") 'org-agenda-clock-cancel)
                (define-key org-agenda-mode-map (kbd "C-c C-x C-x") 'org-clock-in-last)
                (define-key org-agenda-mode-map (kbd "Q") 'org-agenda-clock-cancel)
                (define-key org-agenda-mode-map (kbd "X") 'org-clock-in-last)))
  (setq org-clock-clocked-in-display 'frame-title
        org-clock-frame-title-format (append '((t org-mode-line-string)) '(" ") frame-title-format)
        org-clock-in-switch-to-state "STARTED"
        org-clock-mode-line-total 'current
        org-clock-idle-time 5  ;k 保留idle时间，s 丢弃idle时间；S(加/不加)，clock(out/in)
        org-clock-auto-clockout-timer 3600  ;需先调用org-clock-toggle-auto-clockout或org-clock-auto-clockout-insinuate
        org-timer-display nil
        org-timer-default-timer 25)
  (add-hook 'org-clock-in-hook (lambda () (unless (bound-and-true-p org-timer-countdown-timer) (org-timer-set-timer '(16)))))
  (add-hook 'org-clock-out-hook (lambda () (when (bound-and-true-p org-timer-countdown-timer) (org-timer-stop))))
  (add-hook 'org-clock-cancel-hook (lambda () (when (bound-and-true-p org-timer-countdown-timer) (org-timer-stop))))
  (add-hook 'org-timer-done-hook (lambda () (shell-command "notify-send \"Time for break\"")))
  ;; ===============org-agenda==================
;;;; org-babel
  ;; ================org-babel==================
  (setq org-confirm-babel-evaluate nil)
  (add-hook 'org-babel-after-execute-hook 'org-display-inline-images 'append)
  (setq org-babel-load-languages '((emacs-lisp . t)
                                   (python . t)
                                   (shell . t)
                                   (latex . t)
                                   (gnuplot . t)
                                   (ditaa . t)
                                   (dot . t)
                                   (octave . t)
                                   (matlab . t)
                                   (sqlite . t)
                                   (js . t)
                                   (css . t)))
  ;; 自动加载过慢，改为手动加载
  (advice-add 'org-babel-execute-src-block :before #'(lambda (&optional arg info params)
                                                       (org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages)))
  (defun org-ref-label-list ()
    (org-with-point-at 1
      (let ((case-fold-search t)
            (regexp (org-make-options-regexp '("NAME")))
            label-list)
        (while (re-search-forward regexp nil t)
          (let* ((element (org-element-at-point))
                 (value (org-element-property :name element)))
            (push value label-list)))
        (nreverse label-list))))
  ;; ================org-babel==================
;;;; org-persist
  ;; ================org-persist================
  (setq org-persist-remote-files nil)   ;不保存远程文件的缓存数据
  (advice-add 'org-persist-gc :around #'(lambda (fn) (let ((file-name-handler-alist (cons file-remote-handler-alist file-name-handler-alist)))
                                                       (funcall fn))))
  (defvar file-remote-handler-alist (cons "\\`\\(/mnt/share/\\|/mnt/sshfs/\\).*\\'" 'file-remote-handler))
  (defun file-remote-handler (operation &rest args)
    (cond ((eq operation 'file-remote-p) t)
          (t (let ((inhibit-file-name-handlers
                    (cons 'file-remote-handler
                          (and (eq inhibit-file-name-operation operation)
                               inhibit-file-name-handlers)))
                   (inhibit-file-name-operation operation))
               (apply operation args)))))
  ;; ================org-persist================
;;;; org-table
  ;; =================orgtbl====================
  (use-package org-table
    :diminish orgtbl-mode
    :config
    (define-key org-mode-map (kbd "C-c t i") 'org-table-import)
    (define-key org-mode-map (kbd "C-c t e") 'org-table-export)
    (define-key org-mode-map (kbd "M-o p") #'(lambda () (interactive)
                                               (swint-python-plot-data (org-table-make-csv))))
    (define-key org-mode-map (kbd "M-o C-p") #'(lambda () (interactive)
                                                 (swint-python-load-file (org-table-make-csv))))
    (defun org-table-make-csv ()
      (when (org-at-table-p)
        (let ((export-file (expand-file-name "org_table.csv" temporary-file-directory)))
          (org-table-export export-file "orgtbl-to-csv")
          export-file)))
    ;; S-<return> 拷贝当前列之上的行，并递增数字
    (defun org-table-copy-down/around (fn n)
      (condition-case ex
          (funcall fn n)
        (error (cond ((org-at-table-p)
                      (message (error-message-string ex)))
                     ((or (org-at-heading-p)
                          (save-excursion
                            (back-to-indentation)
                            (equal (point) (line-beginning-position))))
                      (org-toggle-heading))
                     (t
                      (org-toggle-item nil))))))
    (advice-add 'org-table-copy-down :around #'org-table-copy-down/around))
  ;; =================orgtbl====================
;;;; iimage
  ;; =================iimage====================
  (use-package iimage
    :diminish iimage-mode
    :commands iimage-mode
    :init
    (add-hook 'org-mode-hook 'iimage-mode)
    (advice-add 'org-toggle-inline-images :after #'(lambda (&optional include-linked beg end)
                                                     (if org-inline-image-overlays
                                                         (iimage-mode 1)
                                                       (iimage-mode -1)))))
  ;; =================iimage====================
;;;; cdlatex
  ;; ================cdlatex====================
  (use-package cdlatex
    :diminish (cdlatex-mode org-cdlatex-mode)
    :commands (turn-on-cdlatex turn-on-org-cdlatex)
    :init
    ;; C-c { 插入环境
    ;; TAB 展开，如fr TAB展开\frac{}{}
    ;; _^ 自动加{}
    ;; ` `` ``` 插入符号，如` d输入\delta，`` d输入\partial
    ;; ' 修改其后字符的字体
    (add-hook 'org-mode-hook 'turn-on-org-cdlatex)
    (add-hook 'LaTeX-mode-hook 'turn-on-cdlatex)
    (setq cdlatex-takeover-parenthesis nil)
    :config
    (define-key cdlatex-mode-map (kbd "<tab>") #'cdlatex-tab)
    (define-key cdlatex-mode-map (kbd "TAB") nil))
  ;; ================cdlatex====================
;;;; org输出doc
  ;; =================org输出doc================
  ;; 先生成odt文件(需要zip支持)，然后使用libreoffice转化成doc文件
  (setq org-odt-preferred-output-format "docx")
  (define-key org-mode-map (kbd "C-c C-M-e") 'org-odt-export-to-odt)
  ;; =================org输出doc================
;;;; org-latex-preview
  ;; =============org-latex-preview=============
  ;; 默认 -> 预览当前位置或当前节 / C-u -> 清除当前节预览 / C-u C-u -> 预览当前buffer / C-u C-u C-u 清除全部预览
  (define-key org-mode-map (kbd "C-c v") 'org-latex-preview)
  (setq org-format-latex-options (plist-put org-format-latex-options :scale 2.0))
  ;; (setq org-highlight-latex-and-related '(latex))  ;高亮显示公式环境，会造成卡顿
  ;; =============org-latex-preview=============
;;;; org-clipboard-copy/paste
  ;; =========org-clipboard-copy/paste==========
  (defun org-clipboard-copy ()
    (interactive)
    (let* ((context (org-element-lineage (org-element-context) '(link) t))
           (filename (when (equal (org-element-property :type context) "file")
                       (org-element-property :path context))))
      (if (file-exists-p filename)
          (swint-dired-clipboard-copy filename)
        (message "No file at point"))))
  (defun org-clipboard-paste ()
    (interactive)
    (let ((filename (format-time-string "./pic/%Y%m%d_%H%M%S.png")))
      (unless (file-exists-p "pic")
        (dired-create-directory "pic"))
      (swint-dired-clipboard-paste filename)
      (insert "[[" filename "]]")))
  ;; =========org-clipboard-copy/paste==========
;;;; swint-org-previous/next-item
  ;; =======swint-org-previous/next-item========
  (defun swint-org-next-item ()
    (interactive)
    (condition-case _
        (org-next-item)
      (error
       (org-forward-element))))
  (defun swint-org-previous-item ()
    (interactive)
    (condition-case _
        (org-previous-item)
      (error
       (org-backward-element))))
  ;; =======swint-org-previous/next-item========
  )
;; =================org-mode====================
;;; outline
;; ==================outline====================
(use-package outline-magic
  :commands outline-cycle)
(use-package outline
  :diminish outline-minor-mode
  :commands outline-minor-mode
  :init
  (add-hook 'prog-mode-hook 'outline-minor-mode)
  (add-hook 'LaTeX-mode-hook 'outline-minor-mode)
  (add-hook 'message-mode-hook 'outline-minor-mode)
  (defvar outline-minor-mode-prefix "\M-O")
  :config
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (derived-mode-p 'prog-mode 'tex-mode 'message-mode)
        (outline-minor-mode))))
  (smartrep-define-key outline-minor-mode-map "M-s"
    '(("p" . outline-previous-visible-heading)
      ("n" . outline-next-visible-heading)
      ("u" . outline-up-heading)
      ("b" . outline-backward-same-level)
      ("f" . outline-forward-same-level)))
  (define-key outline-minor-mode-map (kbd "C-M-i") nil))
;; ==================outline====================
;;; outshine
;; ==================outshine===================
(use-package outshine
  :diminish outshine-mode
  :commands (outshine-mode
             outshine-cycle-buffer
             outshine-calc-outline-regexp)
  :init
  ;; Heading格式随mode不同，通常是M-;加*加空格
  (add-hook 'outline-minor-mode-hook
            (lambda ()            ;latex-mode/org-mode中不开启outshine
              (unless (derived-mode-p 'LaTeX-mode 'org-mode)
                (outshine-mode))))
  :config
  ;; 与lsp-bridge冲突，acm-filter依赖self-insert-command，但outshine对其重新绑定
  (setq outshine-use-speed-commands nil)
  (define-key outshine-mode-map (vector 'remap 'self-insert-command) nil)
  (setq outshine-imenu-show-headlines-p nil)
  (add-hook 'outline-insert-heading-hook (lambda () (if (string-equal "" head)
                                                        (progn (call-interactively 'comment-dwim)
                                                               (insert "* "))
                                                      (when (memq major-mode '(c-mode arduino-mode))
                                                        (insert "/* * ")))))
  (define-key outshine-mode-map (kbd "<M-S-return>") 'outshine-insert-heading)
  (define-key outshine-mode-map (kbd "<backtab>") 'outshine-cycle-buffer)
  (define-key outshine-mode-map (kbd "C-M-i") nil))
(use-package outorg
  ;; M-O # current heading.
  ;; C-u M-O # current buffer.
  ;; M-# outorg-copy-edits-and-exit.
  :after outshine)
;; ==================outshine===================
;;; org-annotate
;; ===============org-annotate==================
(use-package dired-x-highlight
  :load-path "site-lisp/org-annotate-file/"
  :commands (dired-k--parse-status
             dired-k--previous-highlighted-file
             dired-k--next-highlighted-file
             dired-k--goto-file)
  :init
  (bind-key "P" 'dired-k--previous-highlighted-file dired-mode-map)
  (bind-key "N" 'dired-k--next-highlighted-file dired-mode-map)
  (bind-key "J" 'dired-k--goto-file dired-mode-map)
  :config
  (add-hook 'dired-after-readin-hook 'dired-k--highlight-buffer t)
  ;; 在已有dired-mode中开启dired-x-highlight
  (dolist (buf (cl-remove-if-not (lambda (x)
                                   (equal (buffer-mode x) 'dired-mode))
                                 (buffer-list)))
    (with-current-buffer buf
      (dired-k--highlight-buffer)))
  (defun dired-k--goto-file ()
    (interactive)
    (let* ((files-status (dired-k--parse-status))
           (highlighted-files-list (if files-status
                                       (hash-table-keys files-status)))
           (file-to-go (helm-comp-read "Annotated File: " highlighted-files-list
                                       :marked-candidates nil
                                       :buffer "*helm dired-k--highlight file*")))
      (when file-to-go
        (dired-goto-file (expand-file-name file-to-go))))))
;; Sync annotated status as operating.
(use-package dired-sync-highlight
  :load-path "site-lisp/org-annotate-file/"
  :after dired-x-highlight)
;; 原有org-annotate-file用于全局注释
(use-package org-annotate-file
  :load-path "site-lisp/org-annotate-file/"
  :commands org-annotate-file
  :config
  (setq org-annotate-file-storage-file "~/org/annotated/annotated.org"))
;; 新建swint-org-annotate-file.el用于局部注释
(use-package swint-org-annotate-file
  :load-path "site-lisp/org-annotate-file/"
  :commands (swint-org-annotate-file swint-org-annotation-storage-file))
;; ===============org-annotate==================
;;; org-noter
;; =================org-noter===================
(use-package org-noter
  :commands (org-noter
             swint-annotate-new)
  :bind ("C-M-'" . swint-annotate)
  :config
  (require 'djvu)
  (require 'nov)
  (setq org-noter-always-create-frame nil
        org-noter-disable-narrowing t
        org-noter-use-indirect-buffer nil)
  (defun swint-annotate-new ()
    (interactive)
    (cond ((and (bound-and-true-p org-noter-notes-mode) org-noter--session
                (buffer-live-p (org-noter--session-doc-buffer org-noter--session)))
           (org-noter-insert-note))
          ((bound-and-true-p eaf-interleave-mode)
           (if-let ((pdf-buffer (eaf-interleave--find-buffer
                                 (org-entry-get-with-inheritance eaf-interleave--url-prop))))
               (progn (switch-to-buffer-other-window pdf-buffer)
                      (eaf-interleave-add-note))
             (message "You need to add the first note from pdf side.")))
          (t (swint-annotate-generic-new))))
  (defun swint-annotate ()
    (interactive)
    (if (derived-mode-p 'org-mode)
        (let* ((key (org-entry-get nil "Custom_ID"))
               (pdf-file (car (bibtex-completion-find-pdf key))))
          (when (and (not (org-entry-get nil org-noter-property-doc-file t)) key pdf-file)
            (org-entry-put nil org-noter-property-doc-file (file-relative-name pdf-file)))
          (cond ((org-entry-get nil org-noter-property-doc-file t)
                 (call-interactively 'org-noter))
                ((org-entry-get-with-inheritance "interleave_url")
                 (eaf-interleave-mode)
                 (call-interactively 'eaf-interleave-sync-current-note))
                (t (message "No note system was applied!"))))
      (let* ((target-file (or (dired-get-filename nil t)
                              (buffer-file-name)
                              (bound-and-true-p eaf--buffer-url)))
             (files-status (dired-k--parse-status))
             (existed-note (when files-status (gethash (file-name-nondirectory target-file) files-status)))
             (entry-for-pdf (when (file-in-directory-p target-file "~/Zotero")
                              (bibtex-completion-get-entry-for-pdf target-file)))
             (new-note (unless (or existed-note entry-for-pdf)
                         (message (format "[%s]ocally or [%s]lobally or [%s]af-interleave"
                                          (propertize "l" 'face 'font-lock-warning-face)
                                          (propertize "g" 'face 'font-lock-warning-face)
                                          (propertize "e" 'face 'font-lock-warning-face)))
                         (downcase (read-char-exclusive)))))
        (cond (entry-for-pdf
               (let ((is-eaf (eq major-mode 'eaf-mode))
                     (key-for-pdf (list (bibtex-completion-get-value "=key=" entry-for-pdf))))
                 (bibtex-completion-edit-notes key-for-pdf)
                 (if (not is-eaf)
                     (swint-annotate)
                   (bibtex-completion-notes-mode -1)
                   (widen)
                   (setq-local header-line-format nil))))
              ((or (equal existed-note 'annotated-locally) (equal new-note ?l))
               (swint-org-annotate-file target-file))
              ((or (equal existed-note 'annotated-globally) (equal new-note ?g))
               (org-annotate-file (abbreviate-file-name target-file)))
              ((or (equal existed-note 'interleaved) (equal new-note ?e))
               (eaf-open target-file)
               (while (not (eaf--get-eaf-buffers))
                 (sit-for 0.1))
               (with-current-buffer (car (eaf--get-eaf-buffers))
                 (eaf-interleave-open-notes-file)))
              (t (user-error "Quit"))))))
  (define-key org-noter-doc-mode-map (kbd "C-i") nil)
  (define-key org-noter-doc-mode-map (kbd "M-i") nil)
  (define-key org-noter-doc-mode-map (kbd "C-M-i") nil)
  (define-key org-noter-doc-mode-map (kbd "M-p") nil)
  (define-key org-noter-doc-mode-map (kbd "M-n") nil)
  (define-key org-noter-doc-mode-map (kbd "M-.") nil)
  (define-key org-noter-doc-mode-map (kbd "C-M-.") nil)
  (define-key org-noter-doc-mode-map (kbd "C-M-p") 'org-noter-sync-prev-page-or-chapter)
  (define-key org-noter-doc-mode-map (kbd "C-M-o") 'org-noter-sync-current-page-or-chapter)
  (define-key org-noter-doc-mode-map (kbd "C-M-n") 'org-noter-sync-next-page-or-chapter)
  (define-key org-noter-notes-mode-map (kbd "M-p") nil)
  (define-key org-noter-notes-mode-map (kbd "M-n") nil)
  (define-key org-noter-notes-mode-map (kbd "M-.") nil)
  (define-key org-noter-notes-mode-map (kbd "C-M-.") nil)
  (define-key org-noter-notes-mode-map (kbd "C-M-p") 'org-noter-sync-prev-note)
  (define-key org-noter-notes-mode-map (kbd "C-M-o") 'org-noter-sync-current-note)
  (define-key org-noter-notes-mode-map (kbd "C-M-n") 'org-noter-sync-next-note))
;; =================org-noter===================
;;; org-pdftools
;; ================org-pdftools=================
(use-package org-pdftools
  ;; :hook (org-load-hook . org-pdftools-setup-link)
  :after (:all org pdf-tools)
  :config
  (org-pdftools-setup-link)
  (setq org-pdftools-link-prefix "pdfview"))
;; ================org-pdftools=================
;;; org-brain
;; =================org-brain===================
(use-package org-brain
  :commands eh-org-set-tags-command
  :bind-keymap ("M-O b" . org-brain-prefix-map)
  :init
  (add-hook 'org-mode-hook (lambda ()
                             (bind-key "C-c c" 'eh-org-set-tags-command org-mode-map)))
  :config
  (bind-key "b" 'swint-helm-ag-org-brain-tags org-brain-prefix-map)
  (add-hook 'before-save-hook #'org-brain-ensure-ids-in-buffer)
  (setq org-brain-include-file-entries t
        org-brain-file-entries-use-title nil
        org-brain-headline-entry-name-format-string "%s:%s"
        org-brain-default-file-parent nil
        org-brain-completion-system 'helm
        org-brain-child-linebreak-sexp 'most-positive-fixnum)
  ;; 使用org-id-locations-file(~/.emacs.d/.org-id-locations)保存条目id，使用org-brain-update-id-locations手动更新
  (advice-add 'org-brain-visualize-quit :after #'(lambda () (kill-buffer "*org-brain*")
                                                   (cl-loop for b in (buffer-list)
                                                            do (--if-let (buffer-file-name b)
                                                                   (when (file-in-directory-p it org-brain-path)
                                                                     (kill-buffer b))))))
  (defun eh-org-brain-as-tags ()
    (cl-remove-duplicates
     (mapcar
      (lambda (x)
        (list (replace-regexp-in-string "/" ":" (car x))))
      (org-brain--all-targets))))
  (defun swint-helm-ag-org-brain-tags ()
    (interactive)
    (let ((org-brain-tags (helm-comp-read "Select org brain tags: "
                                          (eh-org-brain-as-tags)
                                          :marked-candidates t
                                          :buffer "*helm ag org brain-swint*")))
      (helm-do-ag org-directory org-directory
                  (concat ":" (string-join org-brain-tags ":") ":"))))
  (defun eh-org-set-tags-command ()
    (interactive)
    (let ((org-current-tag-alist (eh-org-brain-as-tags)))
      (call-interactively 'counsel-org-tag))))
;; =================org-brain===================
;;; ox-latex
;; =================ox-latex====================
(use-package ox-latex
  :after ox
  :config
  (setq org-latex-compiler "xelatex")
  ;; 定义org markup(*_+/=~)等的转换
  (setq org-latex-text-markup-alist '((bold . "\\textbf{%s}")
                                      (code . verb)
                                      (italic . "\\emph{%s}")
                                      (strike-through . "\\sout{%s}")
                                      (underline . "\\underline{%s}")
                                      (verbatim . protectedtexttt)))
  ;; 使用Listings宏包格式化源代码(只是把代码框用listing环境框起来，还需要额外的设置
  (setq org-export-latex-listings t)
  (setq org-latex-image-default-width ".6\\linewidth")
  (add-to-list 'org-latex-classes
               '("cn-article"
                 "\\documentclass[11pt]{ctexart}
\\usepackage[top=1in,bottom=1in,left=0.8in,right=0.8in]{geometry}
\\usepackage{graphicx,amsmath,amssymb,subfigure,url,xspace,booktabs,tikz,float}
\\usepackage[autoplay,loop]{animate}
\\usepackage[absolute,overlay]{textpos}
\\usetikzlibrary{arrows,arrows.meta,shapes,chains,calc,positioning,decorations.markings}
\\newcommand{\\eg}{e.g.,\\xspace}
\\newcommand{\\bigeg}{E.g.,\\xspace}
\\newcommand{\\etal}{\\textit{et~al.\\xspace}}
\\newcommand{\\etc}{etc.\@\\xspace}
\\newcommand{\\ie}{i.e.,\\xspace}
\\newcommand{\\bigie}{I.e.,\\xspace}
\\usepackage[super,square,sort&compress]{natbib}
\\usepackage{hyperref}
\\usepackage{hypernat}
% \\renewcommand{\\citet}[1]{\\textsuperscript{\\cite{#1}}}
\\usepackage[]{caption}
\\captionsetup{font={small,it}}
\\usepackage{comment}
% \\usepackage[]{ctex}
% \\usepackage[]{xeCJK}
% \\setmainfont{Times New Roman}
% \\setsansfont{Times New Roman}
% \\setCJKmainfont{SimSun}
% \\setCJKsansfont{SimSun}
\\newcommand{\\song}{\\CJKfamily{zhsong}}
\\newcommand{\\hei}{\\CJKfamily{zhhei}}
\\newcommand{\\kai}{\\CJKfamily{zhkai}}
\\newcommand{\\fang}{\\CJKfamily{zhfs}}
\\newcommand{\\li}{\\CJKfamily{zhli}}
\\newcommand{\\you}{\\CJKfamily{zhyou}}
\\newcommand{\\yihao}{\\zihao{1}}
\\newcommand{\\xiaoyi}{\\zihao{-1}}
\\newcommand{\\erhao}{\\zihao{2}}
\\newcommand{\\xiaoer}{\\zihao{-2}}
\\newcommand{\\sanhao}{\\zihao{3}}
\\newcommand{\\xiaosan}{\\zihao{-3}}
\\newcommand{\\sihao}{\\zihao{4}}
\\newcommand{\\xiaosi}{\\zihao{-4}}
\\newcommand{\\wuhao}{\\zihao{5}}
\\newcommand{\\xiaowu}{\\zihao{-5}}
\\newcommand{\\liuhao}{\\zihao{6}}
[NO-DEFAULT-PACKAGES]
[NO-PACKAGES]"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
  ;; Beamer默认采用sansfont(无袖衬)，而不是mainfont(有袖衬)
  ;; 设定mainfont会导致公式环境中变量变成正体
  ;; 设定setsansfont使用Times New Roman无法使用英文斜体和粗体
  ;; 使用某些字体可以实现粗斜体，例如DejaVu Sans/DejaVu Sans Mono/DejaVu Serif等
  (add-to-list 'org-latex-classes
               '("cn-beamer"
                 "\\documentclass[11pt]{beamer}
% [xcolor=dvipsnames]
\\usepackage{graphicx,subfigure,url,booktabs,tikz,float,fontspec}
\\usepackage{amsmath,amssymb}
\\DeclareGraphicsRule{*}{mps}{*}{}
\\usepackage{xmpmulti}
\\usepackage{colortbl,dcolumn}
% \\usepackage[autoplay,loop]{animate}
\\usepackage[absolute,overlay]{textpos}
\\usetikzlibrary{arrows,arrows.meta,shapes,chains,calc,positioning,decorations.markings}
\\usepackage{thumbpdf}
\\usepackage{wasysym}
% \\usepackage{ucs}
\\usepackage[utf8]{inputenc}
\\usepackage{pgf,pgfarrows,pgfnodes,pgfautomata,pgfheaps,pgfshade}
\\usepackage{verbatim}
\\usepackage[BoldFont,SlantFont,CJKnumber,CJKchecksingle]{xeCJK}
% \\usepackage{times}
% {DejaVu Sans}{DejaVu Sans Mono}{DejaVu Serif}
\\setmainfont{Times New Roman}
\\setsansfont{Times New Roman}
\\setCJKmainfont{SimSun}
\\setCJKsansfont{SimSun}
\\usefonttheme[onlymath]{serif}
\\setCJKfamilyfont{song}{SimSun}
\\setCJKfamilyfont{kai}{KaiTi}
\\setCJKfamilyfont{hei}{SimHei}
\\setCJKfamilyfont{you}{YouYuan}
\\setCJKfamilyfont{li}{LiSu}
\\setCJKfamilyfont{fang}{FangSong}
\\setCJKfamilyfont{nsong}{NSimSun}
\\setCJKfamilyfont{yh}{Microsoft YaHei}
\\setCJKfamilyfont{asong}{Adobe Song Std}
\\setCJKfamilyfont{afang}{Adobe FangSong Std}
\\setCJKfamilyfont{ahei}{Adobe Heiti Std}
\\setCJKfamilyfont{akai}{Adobe Kaiti Std}
\\newcommand{\\song}{\\CJKfamily{song}}
\\newcommand{\\kai}{\\CJKfamily{kai}}
\\newcommand{\\hei}{\\CJKfamily{hei}}
\\newcommand{\\you}{\\CJKfamily{you}}
\\newcommand{\\li}{\\CJKfamily{li}}
\\newcommand{\\fang}{\\CJKfamily{fang}}
\\newcommand{\\nsong}{\\CJKfamily{nsong}}
\\newcommand{\\yh}{\\CJKfamily{yh}}
\\newcommand{\\asong}{\\CJKfamily{asong}}
\\newcommand{\\afang}{\\CJKfamily{afang}}
\\newcommand{\\ahei}{\\CJKfamily{ahei}}
\\newcommand{\\akai}{\\CJKfamily{akai}}
\\newcommand{\\chuhao}{\\fontsize{42pt}{\\baselineskip}\\selectfont}
\\newcommand{\\xiaochu}{\\fontsize{36pt}{\\baselineskip}\\selectfont}
\\newcommand{\\yihao}{\\fontsize{26pt}{\\baselineskip}\\selectfont}
\\newcommand{\\xiaoyi}{\\fontsize{24pt}{\\baselineskip}\\selectfont}
\\newcommand{\\erhao}{\\fontsize{22pt}{\\baselineskip}\\selectfont}
\\newcommand{\\xiaoer}{\\fontsize{18pt}{\\baselineskip}\\selectfont}
\\newcommand{\\sanhao}{\\fontsize{16pt}{\\baselineskip}\\selectfont}
\\newcommand{\\xiaosan}{\\fontsize{15pt}{\\baselineskip}\\selectfont}
\\newcommand{\\sihao}{\\fontsize{14pt}{\\baselineskip}\\selectfont}
\\newcommand{\\xiaosi}{\\fontsize{12pt}{\\baselineskip}\\selectfont}
\\newcommand{\\wuhao}{\\fontsize{10.5pt}{\\baselineskip}\\selectfont}
\\newcommand{\\xiaowu}{\\fontsize{9pt}{\\baselineskip}\\selectfont}
\\newcommand{\\liuhao}{\\fontsize{7.5pt}{\\baselineskip}\\selectfont}
\\newcommand{\\qihao}{\\fontsize{5.5pt}{\\baselineskip}\\selectfont}
\\renewcommand{\\today}{\\number\\year 年 \\number\\month 月 \\number\\day 日}
\\usepackage[]{caption}
\\captionsetup{font={small,it}}
\\setbeamertemplate{caption}[numbered]
\\setbeamertemplate{frametitle continuation}{}
\\usepackage{comment}
\\subtitle{}
\\subject{}
\\institute{}
[NO-DEFAULT-PACKAGES]
[NO-PACKAGES]"
                 ("\\section{%s}" . "\\section*{%s}")
                 ;; ("\\begin{frame}[fragile]\\frametitle{%s}"
                 ;;  "\\end{frame}"
                 ;;  "\\begin{frame}[fragile]\\frametitle{%s}"
                 ;;  "\\end{frame}")
                 )))
;; =================ox-latex====================
;;; ox-beamer
;; =================ox-beamer===================
(use-package ox-beamer
  :after ox
  :config
  ;; beamer-preview编译beamer文件时，需当前frame以外的页面保持不变
  ;; 输出tex时默认自动生成随机label，导致每次导出的tex文件都不相同
  ;; 对headline，取:CUSTOM_ID:或BEAMER_opt属性，可使用C-c C-x p设置
  ;; 对figure/table，取#+NAME的值
  ;; 若查找不到，则仍采用自动生成机制
  (setq org-latex-prefer-user-labels t  ;使生成tex文件使用org文件中的label
        org-export-time-stamp-file nil  ;导出时间戳会使每次导出文件都不相同
        org-beamer-outline-frame-title "Outline"
        org-beamer-frame-default-options "label=")  ;设置\begin{frame}后[]选项
  ;; 当org-export-preserve-breaks设为nil时，会自动在行尾加\\[0pt]，导致center环境出错
  ;; 此时，可设置org-latex-line-break-safe为空字符串
  (setq org-export-preserve-breaks nil
        org-latex-line-break-safe "\\\\[0pt]")
  ;; 局部变量设置方法1：
  ;; # -*- org-beamer-outline-frame-title: "目录"; -*-
  ;; (put 'org-beamer-outline-frame-title 'safe-local-variable 'stringp)  ;避免弹出提醒
  ;; 局部变量设置方法2：
  ;; #+BIND: org-beamer-outline-frame-title "目录"  ;只用于导出
  (setq org-export-allow-bind-keywords t)  ;允许#+BIND设置局部变量
  (add-to-list 'org-beamer-environments-extra
               '("onlyenv" "O" "\\begin{onlyenv}%a" "\\end{onlyenv}"))
  (add-to-list 'org-beamer-environments-extra
               '("uncoverenv" "U" "\\begin{uncoverenv}%a" "\\end{uncoverenv}")))
;; =================ox-beamer===================
;;; ox-pandoc
;; =================ox-pandoc===================
(use-package ox-pandoc
  :after ox
  :config
  (setq org-pandoc-with-cite-processors nil  ;禁止oc对[cite:@xxx]处理，发送pandoc处理
        org-pandoc-options '((standalone . t))
        org-pandoc-options-for-docx '((standalone . nil))
        org-pandoc-options-for-beamer-pdf '((pdf-engine . "xelatex"))
        org-pandoc-options-for-latex-pdf '((pdf-engine . "xelatex")))
  (add-to-list 'org-pandoc-valid-options 'citeproc))
;; =================ox-pandoc===================
;;; org-appear
;; ================org-appear===================
(use-package org-appear
  :hook (org-mode . org-appear-mode)
  :config
  (setq org-appear-trigger 'always
        org-appear-delay 0.5
        org-appear-autoemphasis t
        org-appear-autosubmarkers t
        org-appear-autoentities t
        org-appear-autolinks nil
        org-appear-autokeywords nil
        org-appear-inside-latex t)
  ;; 当命令导致buffer切换时，org-appear--post-cmd在新buffer中执行，导致org-appear--current-elem执行错误
  (defvar-local org-appear-command-timer nil)
  (advice-add 'org-appear--post-cmd :around #'(lambda (fn) (unless org-appear-command-timer
                                                             (setq org-appear-command-timer
                                                                   (run-with-idle-timer
                                                                    0.5 nil (lambda (func buf)
                                                                              (when (equal (current-buffer) buf)
                                                                                (funcall func))
                                                                              (when (buffer-live-p buf)
                                                                                (with-current-buffer buf
                                                                                  (when (timerp org-appear-command-timer)
                                                                                    (cancel-timer org-appear-command-timer)
                                                                                    (setq org-appear-command-timer nil)))))
                                                                    fn (current-buffer)))))))
;; ================org-appear===================
;;; oc
;; ====================oc=======================
(use-package oc
  :after org
  :config
  ;; 速度较慢，RET选择，可选多个，M-RET插入
  (bind-key "C-c C-x b" 'org-cite-insert org-mode-map)
  (require 'oc-basic)
  (require 'oc-csl)
  (require 'oc-biblatex)
  (require 'oc-natbib)
  (setq org-cite-global-bibliography (delete (expand-file-name "~/.bib/Zotero.bib")
                                             (directory-files "~/.bib" t "\\.bib$")))
  ;; org-cite-activate-processor/org-cite-follow-processor/org-cite-insert-processor/org-cite-export-processors -> 高亮/打开/插入/导出
  (setq org-cite-csl-styles-dir "~/Zotero/styles/")
  ;; locale影响本地化日期等，默认使用en-US，可下载其他：https://github.com/citation-style-language/locales
  (setq org-cite-csl-locales-dir nil)
  (org-cite-register-processor 'bibtex-actions :follow #'org-cite-bibtex-actions)
  (defun org-cite-bibtex-actions (datum arg)
    (interactive)
    (let ((key (if (org-element-type-p datum 'citation-reference)
                   (org-element-property :key datum)
                 (pcase (org-cite-get-references datum t)
                   (`(,key) key)
                   (keys
                    (or (completing-read "Select citation key: " keys nil t)
                        (user-error "Aborted")))))))
      (if (or (string-prefix-p "fig:" key)
              (string-prefix-p "tbl:" key)
              (string-prefix-p "eq:" key))
          (when-let ((p (save-excursion (goto-char (point-min))
                                        (search-forward (format "#+NAME: %s" key) nil t))))
            (push-mark)
            (goto-char p))
        (when-let* ((warning-suppress-log-types '((:warning)))
                    (bibtex-completion-find-pdf key))
          (if arg
              (org-cite-basic-goto datum arg)
            (if (not (buffer-live-p (get-buffer "*eaf*")))
                (bibtex-completion-open-pdf-externally (list key))
              (if-let ((eaf-pdf-win (eaf-find-pdf-window)))
                  (select-window eaf-pdf-win)
                (switch-to-buffer-other-window (current-buffer)))
              (bibtex-completion-open-pdf (list key))))))))
  (setq org-cite-follow-processor 'bibtex-actions))
;; ====================oc=======================
;;; org-extra-emphasis
;; ============org-extra-emphasis===============
(use-package org-extra-emphasis
  :load-path "repos/org-extra-emphasis/"
  :commands helm-insert-org-extra-emphasis
  :init
  (add-hook 'org-mode-hook (lambda ()
                             (bind-key "M-O '" 'helm-insert-org-extra-emphasis org-mode-map)))
  :config
  ;; 弹出org-odt-extra-styles未找到，若需odt支持：https://github.com/kjambunathan/org-mode-ox-odt
  ;; org-extra-emphasis-zws-display-char定义导致加载文件错误，可byte-compile-file解决：
  ;; File mode specification error: (invalid-read-syntax \N{SPACING UNDERSCORE} 1 0)
  (diminish 'org-extra-emphasis-intraword-emphasis-mode)
  (set-face-attribute 'org-extra-emphasis-01 nil :foreground "red" :underline t)
  (set-face-attribute 'org-extra-emphasis-02 nil :foreground "orange" :underline t)
  (set-face-attribute 'org-extra-emphasis-03 nil :foreground "yellow" :underline t)
  (set-face-attribute 'org-extra-emphasis-04 nil :foreground "green" :underline t)
  (set-face-attribute 'org-extra-emphasis-05 nil :foreground "cyan" :underline t)
  (set-face-attribute 'org-extra-emphasis-06 nil :foreground "DodgerBlue" :underline t)
  (set-face-attribute 'org-extra-emphasis-07 nil :foreground "SlateBlue" :underline t)
  (set-face-attribute 'org-extra-emphasis-08 nil :foreground "orchid" :underline t)
  (set-face-attribute 'org-extra-emphasis-09 nil :background "DarkOrchid")
  (set-face-attribute 'org-extra-emphasis-10 nil :background "DarkSlateBlue")
  (set-face-attribute 'org-extra-emphasis-11 nil :background "DarkBlue")
  (set-face-attribute 'org-extra-emphasis-12 nil :background "DarkCyan")
  (set-face-attribute 'org-extra-emphasis-13 nil :background "DarkGreen")
  (set-face-attribute 'org-extra-emphasis-14 nil :background "DarkGoldenrod")
  (set-face-attribute 'org-extra-emphasis-15 nil :background "DarkOrange")
  (set-face-attribute 'org-extra-emphasis-16 nil :background "DarkRed")
  (defvar org-extra-emphasis-faces-alist
    '(("red" "!!")
      ("orange" "!@")
      ("yellow" "!%")
      ("green" "!&")
      ("cyan" "@!")
      ("DodgerBlue" "@@")
      ("SlateBlue" "@%")
      ("orchid" "@&")
      ("DarkOrchid" "%!")
      ("DarkSlateBlue" "%@")
      ("DarkBlue" "%%")
      ("DarkCyan" "%&")
      ("DarkGreen" "&!")
      ("DarkGoldenrod" "&@")
      ("DarkOrange" "&%")
      ("DarkRed" "&&")))
  (defun helm-insert-org-extra-emphasis ()
    (interactive)
    (let* ((color-list (cl-loop for emphasis-faces-pair in org-extra-emphasis-faces-alist
                                collect (car emphasis-faces-pair)))
           (color (helm-comp-read "Color: " color-list
                                  :buffer "*helm insert org extra emphasis*"))
           (wrapper (cadr (assoc color org-extra-emphasis-faces-alist))))
      (if (region-active-p)
          (wrap-region-with (concat (unless (or (char-equal (char-before (region-beginning)) 32)
                                                (char-equal (char-before (region-beginning)) 10))
                                      " ")
                                    wrapper)
                            (concat wrapper
                                    (unless (or (char-equal (char-after (region-end)) 32)
                                                (char-equal (char-after (region-end)) 10))
                                      " ")))
        (insert-bracket-pair-with-space wrapper wrapper)
        (backward-char 1)))))
;; ============org-extra-emphasis===============
(provide 'setup_org)
