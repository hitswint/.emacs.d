;;; org-mode
;; =================org-mode====================
(def-package! org
  :mode ("\\.[oO][rR][gG]\\'" . org-mode)
  :config
  (setq org-modules nil)                ;随org启动的ol模块
;;;; Appearance
  ;; =================Appearance================
  (set-face-attribute 'org-level-5 nil :weight 'normal :foreground "cyan" :height 1.0)
  (set-face-attribute 'org-level-6 nil :weight 'normal :foreground "violet" :height 1.0)
  (set-face-attribute 'org-level-7 nil :weight 'normal :foreground "orange" :height 1.0)
  (set-face-attribute 'org-level-8 nil :weight 'normal :foreground "khaki" :height 1.0)
  (setq org-hide-emphasis-markers t)
  (font-lock-add-keywords 'org-mode
                          '(("^ +\\([-*]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))
  (setq org-use-sub-superscripts "{}")
  (setq org-pretty-entities t)
  (setq org-pretty-entities-include-sub-superscripts t)
  ;; =================Appearance================
;;;; Capture
  ;; =================Capture===================
  (global-set-key (kbd "M-O l") 'org-store-link)
  (global-set-key (kbd "M-O c") 'org-capture)
  (global-set-key (kbd "M-O a") 'org-agenda)
  (setq org-capture-templates
        '(("i" "Idea" entry (file+headline "~/Nutstore-sync/orgzly/task.org" "Idea List") "* TODO %? %^g")
          ("w" "Work" entry (file+headline "~/org/notes-work.org" "Work") "* %? %U %^g")
          ("c" "Computer" entry (file+headline "~/org/notes-computer.org" "Computer") "* %? %U %^g")
          ("o" "Others" entry (file+headline "~/org/notes-others.org" "Others") "* %? %U %^g")
          ("j" "Journal" entry (file+olp+datetree "~/org/journal.org.gpg") "* %? %U")))
  ;; =================Capture===================
;;;; ox
  ;; ====================ox=====================
  (put 'org-beamer-outline-frame-title 'safe-local-variable 'stringp)
  (put 'org-pandoc-table-fmt 'safe-local-variable 'stringp)
  (put 'org-pandoc-paragraph-fmt 'safe-local-variable 'stringp)
  (put 'org-pandoc-src-block-fmt 'safe-local-variable 'stringp)
  ;; ====================ox=====================
;;;; Keybindings
  ;; ===============Keybindings=================
  ;; %^{Description}
  (add-hook 'org-mode-hook
            '(lambda ()
               ;; 插入source code时高亮，C-c '打开相应major-mode编辑窗口。
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
               ;; 如果有#+ATTR_ORG: :width 100则设置为图片宽度为100，否则显示原尺寸。
               (setq org-image-actual-width nil)
               ;; org-redisplay-inline-images(C-c C-x C-M-v) 更新图片。
               ;; org-toggle-inline-images(C-c C-x C-v) 开关图片。
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
               (define-key org-mode-map (kbd "C-c o") '(lambda () (interactive) (swint-org-open-at-point t)))
               (define-key org-mode-map (kbd "C-c J") 'swint-qpdfview-annotated-open)
               (define-key org-mode-map (kbd "C-c :") 'swint-qpdfview-annotated-new)
               (define-key org-mode-map (kbd "C-c M-,") '(lambda () (interactive) (swint-org-mobile-sync "down")))
               (define-key org-mode-map (kbd "C-c M-.") '(lambda () (interactive) (swint-org-mobile-sync "up")))
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
               (define-key org-mode-map (kbd "C-a") '(lambda () (interactive)
                                                       (if (or (org-at-heading-p) (org-at-item-p))
                                                           (call-interactively 'org-beginning-of-line)
                                                         (call-interactively 'smart-beginning-of-line))))
               (define-key org-mode-map (kbd "C-c m") 'helm-insert-latex-math)
               (define-key org-mode-map (kbd "C-c l") '(lambda () (interactive)
                                                         (insert (swint-cursor-localtion))))
               (define-key org-mode-map (kbd "C-c w") 'org-clipboard-copy)
               (define-key org-mode-map (kbd "C-c y") 'org-clipboard-paste)
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
  (setq org-agenda-files (directory-files "~/Nutstore-sync/orgzly/" t ".+\\.org"))
  ;; Do not show title of task in mode-line when using org-clock.
  (setq org-clock-heading-function
        (lambda ()
          (substring (nth 4 (org-heading-components)) 0 0)))
  ;; 显示两周的agenda。
  (setq org-agenda-span 'month)
  ;; 设定todo的子项完成后主项自动完成。
  (add-hook 'org-after-todo-statistics-hook '(lambda (n-done n-not-done)
                                               (let (org-log-done org-log-states)
                                                 (org-todo (if (= n-not-done 0) "DONE" "TODO")))))
  ;; 设定todo关键词。
  (setq org-todo-keywords
        '((sequence "TODO(t)" "Waiting(w)" "Started(s)" "|" "DONE(d)" "Aborted(a)")))
  ;; |后面的项以绿颜色的字出现，(a!/@)：()中出现!和@分别代表记录状态改变的时间以及需要输入备注，多个状态时使用/分隔。
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
  (def-package! cdlatex
    :diminish org-cdlatex-mode
    :commands turn-on-org-cdlatex
    :init
    (add-hook 'org-mode-hook 'turn-on-org-cdlatex))
  ;; Environment templates can be inserted with C-c {.
  ;; The <TAB> key will do template expansion if the cursor is inside a LaTeX fragment1. For example, <TAB> will expand fr to \frac{}{} and position the cursor correctly inside the first brace. Another <TAB> will get you into the second brace. Even outside fragments, <TAB> will expand environment abbreviations at the beginning of a line. For example, if you write ‘equ’ at the beginning of a line and press <TAB>, this abbreviation will be expanded to an equation environment. To get a list of all abbreviations, type M-x cdlatex-command-help.
  ;; Pressing _ and ^ inside a LaTeX fragment will insert these characters together with a pair of braces. If you use <TAB> to move out of the braces, and if the braces surround only a single character or macro, they are removed again (depending on the variable cdlatex-simplify-sub-super-scripts).
  ;; Pressing the backquote ` followed by a character inserts math macros, also outside LaTeX fragments. If you wait more than 1.5 seconds after the backquote, a help window will pop up.
  ;; Pressing the single-quote ' followed by another character modifies the symbol before point with an accent or a font. If you wait more than 1.5 seconds after the single-quote, a help window will pop up. Character modification will work only inside LaTeX fragments; outside the quote is normal.
  ;; ================cdlatex====================
;;;; org输出doc
  ;; =================org输出doc================
  ;; 先生成odt文件(需要zip支持)，然后使用libreoffice转化成doc文件。
  ;; 在win上转doc格式路径名中不能有中文。
  (setq org-odt-preferred-output-format "doc")
  (define-key org-mode-map (kbd "C-c C-S-e") 'org-odt-export-to-odt)
  ;; =================org输出doc================
;;;; org-latex-preview
  ;; =============org-latex-preview=============
  ;; 默认预览当前位置或当前节，加C-u清除当前节预览
  ;; 加C-uC-u预览当前buffer，加C-uC-uC-u清除全部预览
  (define-key org-mode-map (kbd "C-c v") 'org-latex-preview)
  (setf org-highlight-latex-and-related '(latex)) ;高亮显示公式环境。
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
(def-package! dired-x-highlight
  :load-path "site-lisp/org-annotate-file/"
  :commands (dired-k--parse-status
             dired-k--previous-highlighted-file
             dired-k--next-highlighted-file
             dired-k--goto-file)
  :init
  (smartrep-define-key dired-mode-map "C-c"
    '(("p" . dired-k--previous-highlighted-file)
      ("n" . dired-k--next-highlighted-file)))
  (bind-key "J" 'dired-k--goto-file dired-mode-map)
  :config
  (add-hook 'dired-after-readin-hook 'dired-k--highlight-buffer t)
  ;; 在已有dired-mode中开启dired-x-highlight。
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
(def-package! dired-sync-highlight
  :load-path "site-lisp/org-annotate-file/"
  :after dired-x-highlight)
;; 原有org-annotate-file用于全局注释。
(def-package! org-annotate-file
  :load-path "site-lisp/org-annotate-file/"
  :commands org-annotate-file
  :bind ("C-x :" . org-annotate-file-current)
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
;; 新建swint-org-annotate-file.el用于局部注释。
(def-package! swint-org-annotate-file
  :load-path "site-lisp/org-annotate-file/"
  :commands swint-org-annotation-storage-file
  :bind ("C-x ;" . swint-org-annotate-file-current))
;; ===============org-annotate==================
;;; outline
;; ==================outline====================
(def-package! outline-magic
  :commands outline-cycle)
(def-package! outline
  :diminish outline-minor-mode
  :commands (outline-minor-mode
             outline-previous-visible-heading
             outline-next-visible-heading
             outline-up-heading
             outline-backward-same-level
             outline-forward-same-level)
  :init
  (dolist (hook '(prog-mode-hook TeX-mode-hook message-mode-hook))
    (add-hook hook (lambda ()
                     (local-set-key (kbd "M-s p") 'outline-previous-visible-heading)
                     (local-set-key (kbd "M-s n") 'outline-next-visible-heading)
                     (local-set-key (kbd "M-s u") 'outline-up-heading)
                     (local-set-key (kbd "M-s b") 'outline-backward-same-level)
                     (local-set-key (kbd "M-s f") 'outline-forward-same-level))))
  (defvar outline-minor-mode-prefix "\M-O")
  (add-hook 'outline-minor-mode-hook
            (lambda () ;; 在latex-mode和org-mode中不开启outshine。
              (unless (derived-mode-p 'latex-mode 'org-mode)
                (outshine-mode))))
  :config
  (add-hook 'prog-mode-hook 'outline-minor-mode)
  (add-hook 'TeX-mode-hook 'outline-minor-mode)
  (add-hook 'message-mode-hook 'outline-minor-mode)
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
  (define-key outline-minor-mode-map (kbd "<M-S-return>") 'outline-insert-heading)
  (define-key outline-minor-mode-map (kbd "<backtab>") 'outshine-cycle-buffer)
  (define-key outline-minor-mode-map (kbd "C-M-i") nil)
  (add-hook 'outline-insert-heading-hook (lambda ()
                                           (if (string-equal "" head)
                                               (progn (call-interactively 'comment-dwim)
                                                      (insert "* "))
                                             (when (memq major-mode '(c++-mode
                                                                      c-mode
                                                                      arduino-mode))
                                               (save-excursion (insert " */"))))))
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
(def-package! outshine
  :diminish outshine-mode
  :commands (outshine-mode
             outshine-cycle-buffer
             outshine-calc-outline-regexp)
  :config
  ;; Heading格式随mode不同，通常是M-;加*加空格。
  (setq outshine-use-speed-commands t)
  (setq outshine-imenu-show-headlines-p nil)
  (define-key outshine-mode-map (kbd "M-TAB") nil))
(def-package! outorg
  ;; M-O # current heading.
  ;; C-u M-O # current buffer.
  ;; M-# outorg-copy-edits-and-exit.
  :after outshine)
(def-package! navi-mode
  :commands (navi-search-and-switch
             navi-switch-to-twin-buffer)
  :init
  (add-hook 'outline-mode-hook (lambda ()
                                 (bind-key "i" 'navi-search-and-switch outline-mode-prefix-map)
                                 (bind-key "o" 'navi-switch-to-twin-buffer outline-mode-prefix-map)))
  :config
  (global-set-key (kbd "M-s n") nil)
  (global-set-key (kbd "M-s s") 'swint-swiper)
  (global-set-key (kbd "M-s M-s") 'helm-swoop))
;; ==================outshine===================
;;; interleave
;; =================interleave==================
(def-package! interleave
  :commands (interleave-mode
             swint-dired-interleave
             interleave-open-notes-file-for-pdf
             interleave--find-pdf-path)
  :bind (:map dired-mode-map
              ("C-c ;" . swint-dired-interleave))
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
(def-package! org-noter
  :commands (org-noter
             swint-noter/interleave
             swint-open-notes-file-for-pdf)
  :init
  (add-hook 'org-mode-hook (lambda ()
                             (bind-key "C-c ;" 'swint-noter/interleave org-mode-map)))
  :config
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
;;; org-protocol-capture-html
;; =========org-protocol-capture-html===========
(def-package! org-protocol-capture-html
  :load-path "site-lisp/org-protocol-capture-html/"
  :disabled
  :config
  (add-to-list 'org-capture-templates
               '("h" "Html" entry (file+olp "~/Nutstore-sync/orgzly/orgzly.org" "Html")
                 "* %c %U\n%?\n%:initial"))
  ;; Removed useless HTML elements.
  (defun org-protocol-capture-html--with-pandoc (data)
    (unless org-protocol-capture-html-pandoc-no-wrap-option
      (org-protocol-capture-html--define-pandoc-wrap-const))
    (let* ((template (or (plist-get data :template)
                         org-protocol-default-template-key))
           (url (org-protocol-sanitize-uri (plist-get data :url)))
           (type (if (string-match "^\\([a-z]+\\):" url)
                     (match-string 1 url)))
           (title (or (org-protocol-capture-html--nbsp-to-space (string-trim (plist-get data :title))) ""))
           (content (or (org-protocol-capture-html--nbsp-to-space (string-trim (plist-get data :body))) ""))
           (orglink (org-link-make-string
                     url (if (string-match "[^[:space:]]" title) title url)))
           (org-capture-link-is-already-stored t)) ; avoid call to org-store-link
      (setq org-stored-links
            (cons (list url title) org-stored-links))
      (kill-new orglink)
      (with-temp-buffer
        (insert content)
        (if (not (zerop (call-process-region
                         (point-min) (point-max)
                         ;; Change "html" to "html-native_divs".
                         "pandoc" t t nil "-f" "html-native_divs" "-t" "org" org-protocol-capture-html-pandoc-no-wrap-option)))
            (message "Pandoc failed: %s" (buffer-string))
          (progn
            ;; Pandoc succeeded
            (org-link-store-props :type type
                                  :annotation orglink
                                  :link url
                                  :description title
                                  :orglink orglink
                                  :initial (buffer-string)))))
      (org-protocol-capture-html--do-capture)
      nil)))
;; =========org-protocol-capture-html===========
;;; org-ref
;; ==================org-ref====================
(def-package! org-ref-core
  :commands (org-ref-insert-link org-ref-get-bibtex-key-and-file)
  :init
  ;; C-c b 文献引用(citation)
  ;; C-u 元素引用(reference) -> 默认ref:xxx，C-u 选择类型，C-u C-u 引用[[#xxx]]
  ;; C-u C-u 跳转或新建label -> 若存在则跳转，否则新建
  (add-hook 'org-mode-hook (lambda ()
                             (bind-key "C-c b" 'org-ref-insert-link org-mode-map)))
  (setq org-ref-insert-cite-key "\C-cb")
  (setq org-ref-bibtex-hydra-key-binding "\C-cj")
  :config
  ;; org-ref-insert-link在org-ref-core中定义，若直接(def-package! org-ref)提示函数未定义
  (require 'org-ref)
  (setq org-ref-default-bibliography (delete (expand-file-name "~/.bib/Zotero.bib")
                                             (directory-files "~/.bib" t "\\.bib$"))
        org-ref-bibliography-notes "~/Zotero/storage/TKM9D893/notes.org"
        org-latex-prefer-user-labels t
        org-ref-show-broken-links nil
        org-ref-get-pdf-filename-function 'org-ref-get-pdf-filename-helm-bibtex
        org-ref-notes-function '(lambda (thekey)
                                  (bibtex-completion-edit-notes
                                   (list (car (org-ref-get-bibtex-key-and-file thekey))))))
  (setf (cdr (assoc 'org-mode bibtex-completion-format-citation-functions))
        'org-ref-format-citation)
  ;; 在已有org-mode中更新链接高亮。
  (dolist (buf (cl-remove-if-not (lambda (x)
                                   (equal (buffer-mode x) 'org-mode))
                                 (buffer-list)))
    (with-current-buffer buf
      (org-restart-font-lock))))
;; ==================org-ref====================
;;; org-pdftools
;; ================org-pdftools=================
(def-package! org-pdftools
  ;; :hook (org-load-hook . org-pdftools-setup-link)
  :after (:all org pdf-tools)
  :config
  (org-pdftools-setup-link)
  (setq org-pdftools-link-prefix "pdfview"))
;; ================org-pdftools=================
;;; org-brain
;; =================org-brain===================
(def-package! org-brain
  :commands (swint-helm-ag-org-brain-tags eh-org-set-tags-command)
  :init
  (add-hook 'org-mode-hook (lambda ()
                             (bind-key "M-O b b" 'swint-helm-ag-org-brain-tags)
                             (bind-key "C-c c" 'eh-org-set-tags-command org-mode-map)))
  :config
  (bind-key "M-O b" 'org-brain-prefix-map)
  (bind-key "b" 'swint-helm-ag-org-brain-tags org-brain-prefix-map)
  (add-hook 'before-save-hook #'org-brain-ensure-ids-in-buffer)
  (setq org-brain-include-file-entries t
        org-brain-file-entries-use-title nil
        org-brain-headline-entry-name-format-string "%s:%s"
        org-brain-default-file-parent nil
        org-brain-completion-system 'helm)
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
(def-package! ox-latex
  :after ox
  :config
  (setq org-latex-pdf-process
        '("xelatex -interaction nonstopmode %f"
          "xelatex -interaction nonstopmode %f"))
  ;; 定义org markup(*_+/=~)等的转换。
  (setq org-latex-text-markup-alist '((bold . "\\textbf{%s}")
                                      (code . verb)
                                      (italic . "\\emph{%s}")
                                      (strike-through . "\\sout{%s}")
                                      (underline . "\\underline{%s}")
                                      (verbatim . protectedtexttt)))
  ;; 使用Listings宏包格式化源代码(只是把代码框用listing环境框起来，还需要额外的设置。
  (setq org-export-latex-listings t)
  (setq org-latex-image-default-width ".6\\linewidth")
  (setq org-beamer-outline-frame-title "Outline")
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
\\newcommand{\\erhao}{\\zihao{2}}
\\newcommand{\\xiaoer}{\\fontsize{18pt}{\\baselineskip}\\selectfont}
\\newcommand{\\sihao}{\\zihao{4}}
\\newcommand{\\xiaosi}{\\fontsize{12pt}{\\baselineskip}\\selectfont}
\\newcommand{\\wuhao}{\\zihao{5}}
\\newcommand{\\xiaowu}{\\fontsize{9pt}{\\baselineskip}\\selectfont}
\\newcommand{\\liuhao}{\\zihao{6}}
[NO-DEFAULT-PACKAGES]
[NO-PACKAGES]"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
  ;; Beamer默认采用sansfont(无袖衬)，而不是mainfont(有袖衬)。
  ;; 设定mainfont会导致公式环境中变量变成正体。
  ;; 设定setsansfont使用Times New Roman无法使用英文斜体和粗体。
  ;; 使用某些字体可以实现粗斜体，例如DejaVu Sans/DejaVu Sans Mono/DejaVu Serif等。
  (add-to-list 'org-latex-classes
               '("cn-beamer"
                 "\\documentclass[11pt]{beamer}
% [xcolor=dvipsnames]
\\usepackage{graphicx,subfigure,url,booktabs,tikz,float,fontspec}
\\usepackage{amsmath,amssymb}
\\DeclareGraphicsRule{*}{mps}{*}{}
\\usepackage{xmpmulti}
\\usepackage{colortbl,dcolumn}
\\usepackage[autoplay,loop]{animate}
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
\\newcommand{\\erhao}{\\zihao{2}}
\\newcommand{\\xiaoer}{\\fontsize{18pt}{\\baselineskip}\\selectfont}
\\newcommand{\\sihao}{\\zihao{4}}
\\newcommand{\\xiaosi}{\\fontsize{12pt}{\\baselineskip}\\selectfont}
\\newcommand{\\wuhao}{\\zihao{5}}
\\newcommand{\\xiaowu}{\\fontsize{9pt}{\\baselineskip}\\selectfont}
\\newcommand{\\liuhao}{\\zihao{6}}
\\renewcommand{\\today}{\\number\\year 年 \\number\\month 月 \\number\\day 日}
\\usepackage[]{caption}
\\captionsetup{font={small,it}}
\\setbeamertemplate{caption}[numbered]
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
(def-package! ox-beamer
  :after ox
  :config
  (add-to-list 'org-beamer-environments-extra
               '("onlyenv" "O" "\\begin{onlyenv}%a" "\\end{onlyenv}"))
  (add-to-list 'org-beamer-environments-extra
               '("uncoverenv" "U" "\\begin{uncoverenv}%a" "\\end{uncoverenv}")))
;; =================ox-beamer===================
;;; ox-pandoc
;; =================ox-pandoc===================
(def-package! ox-pandoc
  :after ox
  :config
  (setq org-pandoc-options '((standalone . t)))
  (setq org-pandoc-options-for-docx '((standalone . nil)))
  (setq org-pandoc-options-for-beamer-pdf '((pdf-engine . "xelatex")))
  (setq org-pandoc-options-for-latex-pdf '((pdf-engine . "xelatex")))
  (add-to-list 'org-pandoc-valid-options 'citeproc)
  (defvar org-pandoc-table-fmt "表 %d")
  (defvar org-pandoc-paragraph-fmt "图 %d")
  (defvar org-pandoc-src-block-fmt "列表 %d")
  ;; 仅支持[[xxx]]的引用，不支持ref:xxx形式(org-ref默认)，org-ref两者都支持
  (defun org-pandoc-table/override (table contents info)
    "Transcode a TABLE element from Org to Pandoc.
CONTENTS is the contents of the table.  INFO is a plist holding
contextual information."
    (org-pandoc-set-caption-title table info org-pandoc-table-fmt
                                  #'org-pandoc--has-caption-p)
    ;; Export the table with it's modified caption
    (org-export-expand table contents t))
  (defun org-pandoc-paragraph/override (paragraph contents info)
    "Transcode a PARAGRAPH element from Org to Pandoc.
CONTENTS is the contents of the paragraph, as a string.  INFO is
the plist used as a communication channel."
    (when (org-html-standalone-image-p paragraph info)
      ;; Standalone image.
      (org-pandoc-set-caption-title paragraph info org-pandoc-paragraph-fmt
                                    #'org-html-standalone-image-p))
    ;; Export the paragraph verbatim. Like `org-org-identity', but also
    ;; preserves #+ATTR_* tags in the output.
    (org-export-expand paragraph contents t))
  (defun org-pandoc-src-block/override (src-block contents info)
    "Transcode a SRC-BLOCK element from Org to Pandoc.
CONTENTS is the contents of the table. INFO is a plist holding
contextual information."
    (org-pandoc-set-caption-title src-block info org-pandoc-src-block-fmt
                                  #'org-pandoc--has-caption-p)
    ;; Export the src-block with it's modified caption
    (org-export-expand src-block contents t))
  (advice-add 'org-pandoc-table :override #'org-pandoc-table/override)
  (advice-add 'org-pandoc-paragraph :override #'org-pandoc-paragraph/override)
  (advice-add 'org-pandoc-src-block :override #'org-pandoc-src-block/override))
;; =================ox-pandoc===================
(provide 'setup_org_mode)
