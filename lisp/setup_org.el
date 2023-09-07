;;; org-mode
;; =================org-mode====================
(use-package org
  :mode ("\\.[oO][rR][gG]\\'" . org-mode)
  :config
  (setq org-modules nil)                ;随org启动的ol模块
  (defvar org-outline-cmd-list '(org-previous-visible-heading
                                 org-next-visible-heading
                                 org-backward-heading-same-level
                                 org-forward-heading-same-level
                                 org-previous-item
                                 org-next-item
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
  (set-face-attribute 'org-level-5 nil :weight 'normal :foreground "cyan" :height 1.0)
  (set-face-attribute 'org-level-6 nil :weight 'normal :foreground "violet" :height 1.0)
  (set-face-attribute 'org-level-7 nil :weight 'normal :foreground "orange" :height 1.0)
  (set-face-attribute 'org-level-8 nil :weight 'normal :foreground "khaki" :height 1.0)
  (custom-set-faces '(org-code ((t :inherit shadow)))
                    '(org-meta-line ((t :inherit (font-lock-comment-face)))))
  (setq org-hide-emphasis-markers t)
  (font-lock-add-keywords 'org-mode
                          '(("^ +\\([-*]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))
  (setq org-use-sub-superscripts "{}")
  ;; 使用org-toggle-pretty-entities (C-c C-x \) 启闭
  (setq org-pretty-entities t)
  (setq org-pretty-entities-include-sub-superscripts t)
  (setq org-adapt-indentation t)
  ;; =================Appearance================
;;;; Capture
  ;; =================Capture===================
  (global-set-key (kbd "M-O l") 'org-store-link)
  (global-set-key (kbd "M-O c") 'org-capture)
  (global-set-key (kbd "M-O a") 'org-agenda)
  (setq org-capture-templates
        '(("i" "Idea" entry (file+headline "~/webdav-sync/orgzly/task.org" "Idea List") "* TODO %? %^g")
          ("w" "Work" entry (file+headline "~/org/notes-work.org" "Work") "* %? %U %^g")
          ("c" "Computer" entry (file+headline "~/org/notes-computer.org" "Computer") "* %? %U %^g")
          ("j" "Journal" entry (file+olp+datetree "~/org/journal.org.gpg") "* %? %U")))
  ;; =================Capture===================
;;;; ox
  ;; ====================ox=====================
  (put 'org-beamer-outline-frame-title 'safe-local-variable 'stringp)
  (setq org-export-preserve-breaks t)
  ;; ====================ox=====================
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
                (setq org-startup-folded 'showeverything)
                ;; org-mode先于imenu加载时未设置org-imenu-get-tree
                (setq imenu-create-index-function 'org-imenu-get-tree)
                (setq org-imenu-depth 8)
                (setq org-special-ctrl-a/e t)
                (setq org-special-ctrl-o t)
                (setq org-special-ctrl-k t)
                (setq org-capture-bookmark nil)
                (setq org-file-apps (cl-loop for file-extension-pair in file-extension-app-alist
                                             collect (cons
                                                      (concat "\\." (car file-extension-pair) "\\'")
                                                      (concat (cdr file-extension-pair) " %s"))))
                (turn-on-font-lock)
                (iimage-mode 1)
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
                (define-key org-mode-map (kbd "<C-M-return>") 'org-insert-todo-heading)
                (define-key org-mode-map (kbd "C-c e") 'org-beamer-select-environment)
                (define-key org-mode-map (kbd "C-c C-v") 'swint-open-output-file)
                (define-key org-mode-map (kbd "C-c j") 'swint-org-open-at-point)
                (define-key org-mode-map (kbd "C-c o") #'(lambda () (interactive) (swint-org-open-at-point t)))
                (define-key org-mode-map (kbd "C-c :") 'swint-qpdfview-annotated-new)
                (define-key org-mode-map (kbd "C-c M-,") #'(lambda () (interactive) (swint-org-mobile-sync "down")))
                (define-key org-mode-map (kbd "C-c M-.") #'(lambda () (interactive) (swint-org-mobile-sync "up")))
                (smartrep-define-key org-mode-map "M-s"
                  '(("p" . org-previous-visible-heading)
                    ("n" . org-next-visible-heading)
                    ("u" . outline-up-heading)
                    ("b" . org-backward-heading-same-level)
                    ("f" . org-forward-heading-same-level)))
                (smartrep-define-key org-mode-map "C-c"
                  '(("p" . org-previous-item)
                    ("n" . org-next-item)
                    ("P" . org-beginning-of-item-list)
                    ("N" . org-end-of-item-list)))
                (define-key org-mode-map (kbd "C-a") #'(lambda () (interactive)
                                                         (if (or (org-at-heading-p) (org-at-item-p))
                                                             (call-interactively 'org-beginning-of-line)
                                                           (call-interactively 'smart-beginning-of-line))))
                (define-key org-mode-map (kbd "C-c m") 'helm-insert-latex-math)
                (define-key org-mode-map (kbd "C-c l") #'(lambda () (interactive)
                                                           (insert (swint-cursor-localtion))))
                (define-key org-mode-map (kbd "C-c w") 'org-clipboard-copy)
                (define-key org-mode-map (kbd "C-c y") 'org-clipboard-paste)
                (define-key org-mode-map (kbd "C-c C-x C-\\") 'org-toggle-link-display)
                (define-key org-mode-map (kbd "C-j") nil)
                (define-key org-mode-map (kbd "RET") nil)
                (define-key org-mode-map [(control \,)] nil)
                (define-key org-mode-map [(control \.)] nil)
                (define-key org-mode-map [(control \#)] nil)
                (define-key org-mode-map [(control tab)] nil)
                (define-key org-mode-map [(control \')] nil)))
  ;; ===============Keybindings=================
;;;; GTD
  ;; ===================GTD=====================
  (setq org-agenda-files (directory-files "~/webdav-sync/orgzly/" t ".+\\.org"))
  ;; Do not show title of task in mode-line when using org-clock.
  (setq org-clock-heading-function
        (lambda ()
          (substring (nth 4 (org-heading-components)) 0 0)))
  ;; 显示两周的agenda
  (setq org-agenda-span 'month)
  ;; 设定todo的子项完成后主项自动完成
  (add-hook 'org-after-todo-statistics-hook #'(lambda (n-done n-not-done)
                                                (let (org-log-done org-log-states)
                                                  (org-todo (if (= n-not-done 0) "DONE" "TODO")))))
  ;; 设定todo关键词
  (setq org-todo-keywords
        '((sequence "TODO(t)" "Waiting(w)" "Started(s)" "|" "DONE(d)" "Aborted(a)")))
  ;; |后面的项以绿颜色的字出现，(a!/@)：()中出现!和@分别代表记录状态改变的时间以及需要输入备注，多个状态时使用/分隔
  ;; ===================GTD=====================
;;;; org-babel
  ;; ===========使用ditaa输出ascii图片==========
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
  ;; ===========使用ditaa输出ascii图片==========
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
    (add-hook 'LaTeX-mode-hook 'turn-on-cdlatex))
  ;; ================cdlatex====================
;;;; org输出doc
  ;; =================org输出doc================
  ;; 先生成odt文件(需要zip支持)，然后使用libreoffice转化成doc文件
  (setq org-odt-preferred-output-format "docx")
  (define-key org-mode-map (kbd "C-c C-S-e") 'org-odt-export-to-odt)
  ;; =================org输出doc================
;;;; org-latex-preview
  ;; =============org-latex-preview=============
  ;; 默认 -> 预览当前位置或当前节 / C-u -> 清除当前节预览 / C-u C-u -> 预览当前buffer / C-u C-u C-u 清除全部预览
  (define-key org-mode-map (kbd "C-c v") 'org-latex-preview)
  (setq org-format-latex-options (plist-put org-format-latex-options :scale 2.0))
  (setf org-highlight-latex-and-related '(latex)) ;高亮显示公式环境
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
  )
;; =================org-mode====================
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
  :commands (org-annotate-file-current org-annotate-file)
  :bind ("C-x :" . org-annotate-file-current)
  :init
  (dolist (hook '(dired-mode-hook pdf-view-mode-hook))
    (add-hook hook (lambda () ()
                     (local-set-key (kbd ":") 'org-annotate-file-current))))
  :config
  (defun org-annotate-file-current ()
    (interactive)
    (cond
     ((equal major-mode 'dired-mode)
      (org-annotate-file (abbreviate-file-name (dired-get-filename))))
     ((equal major-mode 'pdf-view-mode)
      (dired-jump-other-window)
      (org-annotate-file (abbreviate-file-name (dired-get-filename))))
     (t
      (switch-to-buffer-other-window (current-buffer))
      (org-annotate-file (abbreviate-file-name (buffer-file-name))))))
  (setq org-annotate-file-storage-file "~/org/annotated/annotated.org"))
;; 新建swint-org-annotate-file.el用于局部注释
(use-package swint-org-annotate-file
  :load-path "site-lisp/org-annotate-file/"
  :commands (swint-org-annotate-file-current swint-org-annotation-storage-file)
  :bind ("C-x ;" . swint-org-annotate-file-current)
  :init
  (dolist (hook '(dired-mode-hook pdf-view-mode-hook))
    (add-hook hook (lambda () ()
                     (local-set-key (kbd ";") 'swint-org-annotate-file-current)))))
;; ===============org-annotate==================
;;; outline
;; ==================outline====================
(use-package outline-magic
  :commands outline-cycle)
(use-package outline
  :diminish outline-minor-mode
  :commands outline-minor-mode
  :init
  (add-hook 'prog-mode-hook 'outline-minor-mode)
  (add-hook 'TeX-mode-hook 'outline-minor-mode)
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
  (define-key outline-minor-mode-map (kbd "C-M-i") nil)
  (add-hook 'TeX-mode-hook
            (lambda ()
              (define-key outline-minor-mode-map (kbd "\C-i") '(menu-item "maybe-latex/hide-show" nil :filter
                                                                          (lambda (&rest _)
                                                                            (when (latex//header-at-point)
                                                                              #'outline-cycle))))))
  ;; Copied from latex-extra.
  (defcustom latex/section-hierarchy
    '("\\\\headerbox\\_>"
      "\\\\subparagraph\\*?\\_>"
      "\\\\paragraph\\*?\\_>"
      "\\\\subsubsection\\*?\\_>"
      "\\\\subsection\\*?\\_>"
      "\\\\section\\*?\\_>"
      "\\\\chapter\\*?\\_>"
      "\\\\part\\*?\\_>"
      ;; "\\\\maketitle\\_>"
      "\\\\appendix\\_>\\|\\\\\\(begin\\|end\\){document}"
      "\\\\documentclass\\_>")
    "List of regexps which define what a section can be.Ordered from deepest to highest level."
    :group 'outlines
    :type '(repeat (choice regexp function)))
  (defun latex/section-regexp ()
    "Return a regexp matching anything in `latex/section-hierarchy'."
    (format "^\\(%s\\)" (mapconcat 'identity latex/section-hierarchy "\\|")))
  (defun latex//header-at-point ()
    "Return header under point or nil, as per `latex/section-hierarchy'."
    (save-match-data
      (save-excursion
        (goto-char (line-beginning-position))
        (when (looking-at (latex/section-regexp))
          (match-string-no-properties 0))))))
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
              (unless (derived-mode-p 'latex-mode 'org-mode)
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
;;; interleave
;; =================interleave==================
(use-package interleave
  :commands (interleave-mode
             swint-dired-interleave
             interleave-open-notes-file-for-pdf
             interleave--find-pdf-path)
  :bind (:map dired-mode-map
              ("M-;" . swint-dired-interleave))
  :config
  (setq interleave-disable-narrowing t
        interleave-insert-relative-name nil)
  (defun swint-dired-interleave ()
    (interactive)
    (let* ((pdf-file (dired-get-file-for-visit))
           (note-file (concat "~/org/interleave_notes/" (file-name-base pdf-file) ".org")))
      (if (file-exists-p note-file)
          (find-file note-file)
        (find-file pdf-file)
        (interleave-open-notes-file-for-pdf))))
  (define-key interleave-mode-map (kbd "M-.") nil)
  (define-key interleave-mode-map (kbd "M-p") nil)
  (define-key interleave-mode-map (kbd "M-n") nil)
  (define-key interleave-mode-map (kbd "C-M-o") 'interleave-sync-pdf-page-current)
  (define-key interleave-mode-map (kbd "C-M-p") 'interleave-sync-pdf-page-previous)
  (define-key interleave-mode-map (kbd "C-M-n") 'interleave-sync-pdf-page-next)
  (define-key interleave-pdf-mode-map (kbd "M-.") nil)
  (define-key interleave-pdf-mode-map (kbd "M-p") nil)
  (define-key interleave-pdf-mode-map (kbd "M-n") nil)
  (define-key interleave-pdf-mode-map (kbd "C-M-o") 'interleave-sync-pdf-page-current)
  (define-key interleave-pdf-mode-map (kbd "C-M-p") 'interleave-sync-pdf-page-previous)
  (define-key interleave-pdf-mode-map (kbd "C-M-n") 'interleave-sync-pdf-page-next))
;; =================interleave==================
;;; org-noter
;; =================org-noter===================
(use-package org-noter
  :commands (org-noter
             swint-noter/interleave
             swint-open-notes-file-for-pdf)
  :init
  (add-hook 'org-mode-hook (lambda ()
                             (bind-key "C-c ;" 'swint-noter/interleave org-mode-map)))
  :config
  (require 'djvu)
  (require 'nov)
  (defun swint-noter/interleave ()
    (interactive)
    (let* ((key (org-entry-get nil "Custom_ID"))
           (pdf-file (car (bibtex-completion-find-pdf key))))
      (when (and (not (org-entry-get nil org-noter-property-doc-file t)) key pdf-file)
        (org-entry-put nil org-noter-property-doc-file (file-relative-name pdf-file)))
      (if (org-entry-get nil org-noter-property-doc-file t)
          (call-interactively 'org-noter)
        (call-interactively 'interleave-mode))))
  (defun swint-open-notes-file-for-pdf ()
    (interactive)
    (if (file-in-directory-p (buffer-file-name) "~/Zotero")
        (let* ((entry-for-pdf (bibtex-completion-get-entry-for-pdf (buffer-file-name)))
               (key-for-pdf (list (bibtex-completion-get-value "=key=" entry-for-pdf))))
          (when entry-for-pdf
            (bibtex-completion-edit-notes key-for-pdf)
            (swint-noter/interleave)))
      (interleave-open-notes-file-for-pdf)))
  (define-key org-noter-doc-mode-map (kbd "I") 'org-noter-insert-precise-note)
  (define-key org-noter-doc-mode-map (kbd "M-p") nil)
  (define-key org-noter-doc-mode-map (kbd "M-.") nil)
  (define-key org-noter-doc-mode-map (kbd "M-n") nil)
  (define-key org-noter-doc-mode-map (kbd "C-M-.") nil)
  (define-key org-noter-doc-mode-map (kbd "C-M-p") 'org-noter-sync-prev-page-or-chapter)
  (define-key org-noter-doc-mode-map (kbd "C-M-o") 'org-noter-sync-current-page-or-chapter)
  (define-key org-noter-doc-mode-map (kbd "C-M-n") 'org-noter-sync-next-page-or-chapter)
  (define-key org-noter-notes-mode-map (kbd "M-p") nil)
  (define-key org-noter-notes-mode-map (kbd "M-.") nil)
  (define-key org-noter-notes-mode-map (kbd "M-n") nil)
  (define-key org-noter-notes-mode-map (kbd "C-M-.") nil)
  (define-key org-noter-notes-mode-map (kbd "C-M-p") 'org-noter-sync-prev-note)
  (define-key org-noter-notes-mode-map (kbd "C-M-o") 'org-noter-sync-current-note)
  (define-key org-noter-notes-mode-map (kbd "C-M-n") 'org-noter-sync-next-note))
;; =================org-noter===================
;;; org-ref
;; ==================org-ref====================
(use-package org-ref-core
  :commands (org-ref-insert-link
             org-ref-insert-link-hydra/body
             org-ref-bibtex-hydra/body
             helm-org-ref-link)
  :init
  ;; org-ref-insert-link -> 文献引用(citation)
  ;; C-u -> 交叉引用(reference)，默认ref:xxx，C-u 选择类型，C-u C-u 引用[[#xxx]]
  ;; C-u C-u -> 跳转或新建label，若存在则跳转，否则新建
  (add-hook 'org-mode-hook (lambda ()
                             (bind-key "C-c r" 'helm-org-ref-link org-mode-map)
                             (bind-key "C-c R" 'org-ref-insert-link-hydra/body org-mode-map)))
  :config
  (bind-key "C-c r" 'org-ref-bibtex-hydra/body bibtex-mode-map)
  ;; org-ref-insert-link在org-ref-core中定义，若直接(use-package org-ref)提示函数未定义
  (require 'org-ref)
  (require 'helm)
  ;; 在已有org-mode中更新链接高亮
  (dolist (buf (cl-remove-if-not (lambda (x)
                                   (equal (buffer-mode x) 'org-mode))
                                 (buffer-list)))
    (with-current-buffer buf
      (org-restart-font-lock)))
  (defvar helm-org-ref-link-buffer "*helm org ref link-swint*")
  (defvar helm-org-ref-link-map
    (let ((map (make-sparse-keymap)))
      (set-keymap-parent map helm-map)
      (define-key map (kbd "C-j") #'(lambda () (interactive)
                                      (helm-run-after-exit #'(lambda (_candidates)
                                                               (goto-char (point-min))
                                                               (search-forward (format "#+NAME: %s" (car _candidates)) nil t))
                                                           (helm-marked-candidates))))
      map)
    "Keymap for `helm-org-ref-link'.")
  ;; RET 插入并退出 / C-l 连续插入 / C-j 跳转
  (defun helm-org-ref-link ()
    (interactive)
    (helm--push-and-remove-dups helm-org-ref-link-buffer 'helm-buffers)
    (setq helm-last-buffer helm-org-ref-link-buffer)
    (let* ((helm-split-window-default-side 'below)
           (helm-always-two-windows t)
           (label-names (helm-comp-read "Label: " (org-with-point-at 1
                                                    (let ((case-fold-search t)
                                                          (regexp (org-make-options-regexp '("NAME")))
                                                          label-list)
                                                      (while (re-search-forward regexp nil t)
                                                        (let* ((element (org-element-at-point))
                                                               (value (org-element-property :name element)))
                                                          (push value label-list)))
                                                      (nreverse label-list)))
                                        :marked-candidates t
                                        :buffer helm-org-ref-link-buffer
                                        :keymap helm-org-ref-link-map
                                        :persistent-action (lambda (candidate)
                                                             (with-helm-current-buffer
                                                               (if (string-prefix-p "office" (cadr (split-string candidate ":")))
                                                                   (insert (format "[cite:@%s] " candidate))
                                                                 (insert (format "[[%s]] " candidate))))))))
      ;; 将插入引用定义为keymap中的命令的话，存在类似helm-insert-or-copy的Quit警告问题
      (when (sequencep label-names)
        (if (string-prefix-p "office" (cadr (split-string (car label-names) ":")))
            (insert (bibtex-completion-format-citation-org-cite label-names))
          (insert (s-join " " (--map (format "[[%s]]" it) label-names))))))))
;; ==================org-ref====================
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
  (setq org-latex-pdf-process
        '("xelatex -interaction nonstopmode %f"
          "xelatex -interaction nonstopmode %f"))
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
\\usepackage{ucs}
% \\usepackage[utf8]{inputenc}
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
  (setq org-latex-prefer-user-labels t) ;使生成tex文件使用org文件中的label
  (setq org-export-time-stamp-file nil) ;导出时间戳会使每次导出文件都不相同
  (setq org-beamer-outline-frame-title "Outline")
  (setq org-beamer-frame-default-options "label=") ;; 设置\begin{frame}后[]选项
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
  (setq org-pandoc-options '((standalone . t)))
  (setq org-pandoc-options-for-docx '((standalone . nil)))
  (setq org-pandoc-options-for-beamer-pdf '((pdf-engine . "xelatex")))
  (setq org-pandoc-options-for-latex-pdf '((pdf-engine . "xelatex")))
  (add-to-list 'org-pandoc-valid-options 'citeproc))
;; =================ox-pandoc===================
;;; org-appear
;; ================org-appear===================
(use-package org-appear
  :hook (org-mode . org-appear-mode)
  :config
  (setq org-appear-trigger 'always)
  (setq org-appear-delay 0.5)
  (setq org-appear-autolinks nil)
  (setq org-appear-autoemphasis t)
  (setq org-appear-autosubmarkers t)
  (setq org-appear-autoentities t)
  (setq org-appear-autokeywords t))
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
  (setq org-cite-global-bibliography nil)
  ;; org-cite-activate-processor/org-cite-follow-processor/org-cite-insert-processor/org-cite-export-processors -> 高亮/打开/插入/导出
  (setq org-cite-csl-styles-dir "~/Zotero/styles/")
  ;; locale影响本地化日期等，默认使用en-US，可下载其他：https://github.com/citation-style-language/locales
  (setq org-cite-csl-locales-dir nil))
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
  ;; org-extra-emphasis-zws-display-char定义导致加载文件错误，可byte-compile-file解决：
  ;; File mode specification error: (invalid-read-syntax \N{SPACING UNDERSCORE} 1 0)
  (custom-set-faces
   '(org-extra-emphasis-01 ((t (:foreground "red" :underline t :inherit org-extra-emphasis))))
   '(org-extra-emphasis-02 ((t (:foreground "orange" :underline t :inherit org-extra-emphasis))))
   '(org-extra-emphasis-03 ((t (:foreground "yellow" :underline t :inherit org-extra-emphasis))))
   '(org-extra-emphasis-04 ((t (:foreground "green" :underline t :inherit org-extra-emphasis))))
   '(org-extra-emphasis-05 ((t (:foreground "cyan" :underline t :inherit org-extra-emphasis))))
   '(org-extra-emphasis-06 ((t (:foreground "DodgerBlue" :underline t :inherit org-extra-emphasis))))
   '(org-extra-emphasis-07 ((t (:foreground "SlateBlue" :underline t :inherit org-extra-emphasis))))
   '(org-extra-emphasis-08 ((t (:foreground "orchid" :underline t :inherit org-extra-emphasis))))
   '(org-extra-emphasis-09 ((t (:background "DarkOrchid" :inherit org-extra-emphasis))))
   '(org-extra-emphasis-10 ((t (:background "DarkSlateBlue" :inherit org-extra-emphasis))))
   '(org-extra-emphasis-11 ((t (:background "DarkBlue" :inherit org-extra-emphasis))))
   '(org-extra-emphasis-12 ((t (:background "DarkCyan" :inherit org-extra-emphasis))))
   '(org-extra-emphasis-13 ((t (:background "DarkGreen" :inherit org-extra-emphasis))))
   '(org-extra-emphasis-14 ((t (:background "DarkGoldenrod" :inherit org-extra-emphasis))))
   '(org-extra-emphasis-15 ((t (:background "DarkOrange" :inherit org-extra-emphasis))))
   '(org-extra-emphasis-16 ((t (:background "DarkRed" :inherit org-extra-emphasis)))))
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
