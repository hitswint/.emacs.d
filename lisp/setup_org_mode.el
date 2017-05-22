;;; org-mode
;; =================org-mode====================
(use-package org
  ;; Enabled in modes.
  :defer t
  :config
;;;; Appearance
  ;; =================Appearance================
  (set-face-attribute 'org-level-5 nil :weight 'normal :foreground "cyan" :height 1.0)
  (set-face-attribute 'org-level-6 nil :weight 'normal :foreground "violet" :height 1.0)
  (set-face-attribute 'org-level-7 nil :weight 'normal :foreground "orange" :height 1.0)
  (set-face-attribute 'org-level-8 nil :weight 'normal :foreground "gray" :height 1.0)
  (setq org-hide-emphasis-markers t)
  (font-lock-add-keywords 'org-mode
                          '(("^ +\\([-*]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))
  ;; =================Appearance================
;;;; Capture
  ;; =================Capture===================
  (global-set-key (kbd "M-O l") 'org-store-link)
  (global-set-key (kbd "M-O c") 'org-capture)
  (global-set-key (kbd "M-O a") 'org-agenda)
  (setq org-capture-templates
        '(("i" "Idea" entry (file+headline "~/org/task.org" "Idea List") "* TODO %? %^g")
          ("w" "Work" entry (file+headline "~/org/notes-work.org" "Work") "* %? %U %^g")
          ("c" "Computer" entry (file+headline "~/org/notes-computer.org" "Computer") "* %? %U %^g")
          ("o" "Others" entry (file+headline "~/org/notes-others.org" "Others") "* %? %U %^g")
          ("j" "Journal" entry (file+datetree "~/org/journal.org.gpg") "* %? %U")))
  ;; =================Capture===================
;;;; Keybindings
  ;; ===============Keybindings=================
  ;; %^{Description}
  (add-hook 'org-mode-hook
            '(lambda ()
               ;; 插入source code时高亮，C-c ' 打开相应major-mode编辑窗口。
               (setq org-src-fontify-natively t)
               ;; (setq org-startup-indented t)
               (setq truncate-lines nil)
               (setq org-hide-leading-stars t)
               (setq org-startup-folded 'content)
               (setq org-imenu-depth 8)
               (setq org-special-ctrl-a/e t)
               (setq org-special-ctrl-o t)
               (setq org-special-ctrl-k t)
               (setq org-capture-bookmark nil)
               (setq org-format-latex-options '(:foreground default :background default :scale 1.5 :html-foreground "Black" :html-background "Transparent" :html-scale 1.0 :matchers ("begin" "$1" "$" "$$" "\\(" "\\[")))
               (setq org-latex-default-figure-position "htbp")
               (setq org-latex-image-default-width "1\\linewidth")
               (setq org-latex-pdf-process '("xelatex -interaction nonstopmode -output-directory %o %f"))
               (setq org-latex-remove-logfiles nil)
               (turn-on-font-lock)
               (define-key org-mode-map (kbd "<C-M-return>") 'org-insert-todo-heading)
               (define-key org-mode-map (kbd "C-c C-b") 'org-beamer-select-environment)
               (define-key org-mode-map (kbd "C-c C-v") 'swint-org-open-export-pdf)
               (define-key org-mode-map (kbd "C-c j") 'swint-open-at-point-with-apps)
               (define-key org-mode-map (kbd "C-c o") '(lambda () (interactive) (swint-open-at-point t)))
               (smartrep-define-key org-mode-map "M-s"
                 '(("p" . outline-previous-visible-heading)
                   ("n" . outline-next-visible-heading)
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
  ;; Do not show title of task in mode-line when using org-clock.
  (setq org-clock-heading-function
        (lambda ()
          (substring (nth 4 (org-heading-components)) 0 0)))
  ;; 显示两周的agenda。
  (setq org-agenda-span 14)
  ;; 设定todo的子项完成后主项自动完成。
  (add-hook 'org-after-todo-statistics-hook '(lambda (n-done n-not-done)
                                               (let (org-log-done org-log-states)
                                                 (org-todo (if (= n-not-done 0) "DONE" "TODO")))))
  ;; 设定todo关键词。
  (setq org-todo-keywords
        '((sequence "TODO(t)" "Waiting(w)" "Started(s)" "|" "DONE(d)" "Aborted(a)")))
  ;; |后面的项以绿颜色的字出现，(a!/@)：()中出现!和@分别代表记录状态改变的时间以及需要输入备注，多个状态时使用/分隔。
  ;; ===================GTD=====================
;;;; 使用ditaa输出ascii图片
  ;; ===========使用ditaa输出ascii图片==========
  (add-hook 'org-babel-after-execute-hook 'org-display-inline-images 'append)
  (org-babel-do-load-languages
   (quote org-babel-load-languages)
   (quote ((emacs-lisp . t)
           (dot . t)
           (ditaa . t)
           (R . t)
           (python . t)
           (ruby . t)
           (gnuplot . t)
           (clojure . t)
           (sh . t)
           (ledger . t)
           (org . t)
           (plantuml . t)
           (latex . t))))
  (setq org-confirm-babel-evaluate nil)
  ;; win中似乎不好使，应该是没装java。
  ;; ===========使用ditaa输出ascii图片==========
;;;; cdlatex
  ;; ================cdlatex====================
  (use-package cdlatex
    ;; Enabled in modes.
    :defer t
    :commands turn-on-org-cdlatex
    :init
    (add-hook 'org-mode-hook 'turn-on-org-cdlatex))
  ;; Environment templates can be inserted with C-c {.
  ;; The <TAB> key will do template expansion if the cursor is inside a LaTeX fragment1. For example, <TAB> will expand fr to \frac{}{} and position the cursor correctly inside the first brace. Another <TAB> will get you into the second brace. Even outside fragments, <TAB> will expand environment abbreviations at the beginning of a line. For example, if you write ‘equ’ at the beginning of a line and press <TAB>, this abbreviation will be expanded to an equation environment. To get a list of all abbreviations, type M-x cdlatex-command-help.
  ;; Pressing _ and ^ inside a LaTeX fragment will insert these characters together with a pair of braces. If you use <TAB> to move out of the braces, and if the braces surround only a single character or macro, they are removed again (depending on the variable cdlatex-simplify-sub-super-scripts).
  ;; Pressing the backquote ` followed by a character inserts math macros, also outside LaTeX fragments. If you wait more than 1.5 seconds after the backquote, a help window will pop up.
  ;; Pressing the single-quote ' followed by another character modifies the symbol before point with an accent or a font. If you wait more than 1.5 seconds after the single-quote, a help window will pop up. Character modification will work only inside LaTeX fragments; outside the quote is normal.
  ;; ================cdlatex====================
;;;; 截图
  ;; ===================截图====================
  ;; Screenshot-local截图到./pic文件夹中，screenshot截图到home/swint/org/pic文件夹中。
  (defun swint-screenshot (&optional arg)
    "Take a screenshot into a unique-named file in the current buffer file
  directory and insert a link to this file."
    (interactive "P")
    ;; 将截图名字定义为buffer名字加日期。
    (let ((screen-file-path (if arg
                                (concat (getenv "HOME") "/org/pic/")
                              (progn (unless (file-exists-p "./pic")
                                       ;; 建立pic文件夹。
                                       (dired-create-directory "./pic"))
                                     "./pic/")))
          screen-file)
      (cond (is-lin
             (setq screen-file (concat (make-temp-name
                                        (concat screen-file-path (file-name-base (or (buffer-file-name) (buffer-name)))
                                                "_" (format-time-string "%Y%m%d_"))) ".png"))
             (suspend-frame)
             (call-process-shell-command "scrot" nil nil nil nil " -s " (concat "\"" screen-file "\"" )))
            (is-win
             (setq screen-file
                   ;; 注释掉原来make-temp-name的方法，因为在win上对于某些prefix无法生成随机名字。
                   ;; (concat (make-temp-name
                   ;;          (concat (getenv "HOME") "/org/pic/" (file-name-base)
                   ;;                  "_"
                   ;;                  (format-time-string "%Y%m%d_"))) ".png")
                   (replace-regexp-in-string "/" "\\" (concat screen-file-path (file-name-base (or (buffer-file-name) (buffer-name)))
                                                              "_" (format-time-string "%Y%m%d_") (make-temp-name "") ".png") t t))
             (call-process "c:\\Program Files (x86)\\IrfanView\\i_view32.exe" nil nil nil
                           (concat "/clippaste /convert=" screen-file))))
      screen-file))
  (global-set-key (kbd "C-x M-p") 'swint-screenshot)
  (global-set-key (kbd "C-x M-P") '(lambda ()
                                     (interactive)
                                     (swint-screenshot t)))
  ;; ===================截图====================
;;;; 插入截图
  ;; =================插入截图==================
  ;; 如果有#+ATTR_ORG: :width 100则设置为图片宽度为100，否则显示原尺寸。
  ;; 设置尺寸之后使用org-redisplay-inline-images(C-c C-x C-M-v)更新图片。
  (setq org-image-actual-width nil)
  (add-hook 'org-mode-hook 'iimage-mode)
  (add-hook 'org-mode-hook 'org-display-inline-images)
  (global-set-key (kbd "M-g v") 'iimage-mode)
  (defun swint-insert-screenshot (&optional arg)
    "Take a screenshot into a unique-named file in the current buffer file
  directory and insert a link to this file."
    (interactive "P")
    (let ((screen-file-name (swint-screenshot arg)))
      (if (eq major-mode 'org-mode)
          (progn (insert (concat "[[" (abbreviate-file-name screen-file-name) "]]"))
                 (org-redisplay-inline-images))
        (insert (abbreviate-file-name screen-file-name)))))
  (global-set-key (kbd "C-x p") 'swint-insert-screenshot)
  (global-set-key (kbd "C-x P") '(lambda ()
                                   (interactive)
                                   (swint-insert-screenshot t)))
  ;; win上跟lin上不同，需要先使用截图工具进行截图并复制，然后调用insert-screenshot。
  ;; org中打开和关闭图片显示(org-display-inline-images)和(org-remove-inline-images)。
  ;; 可以使用(org-toggle-inline-images)快捷键为C-c C-x C-v。
  ;; =================插入截图==================
;;;; swint-open-at-point
  ;; =============swint-open-at-point===========
  (defun org-at-top-heading-p ()
    "Go back to top heading and return that point. If already on top heading, return nil."
    (let ((headline (org-element-at-point)))
      (and (org-at-heading-p)
           (equal (org-element-type headline) 'headline)
           (equal (org-element-property :level headline) 1))))
  (defun swint-get-annotated-file ()
    (if (org-at-top-heading-p)
        (concat "~"
                (if (string-prefix-p "annotated-(" (file-name-nondirectory (buffer-file-name)))
                    (replace-regexp-in-string
                     "_" "/" (substring-no-properties (file-name-nondirectory (buffer-file-name)) 11 -5)))
                (car (last (split-string (substring-no-properties (org-get-heading) nil -2) "\\[file:") 1)))))
  (defun swint-key-at-point ()
    "Find citation key at point."
    (interactive)
    (let ((current-point (point))
          (citation-format (cond
                            ((eq major-mode 'org-mode)
                             "ebib:")
                            ((eq major-mode 'latex-mode)
                             "\\\\citep{")))
          key)
      (save-excursion
        (beginning-of-line)
        (while (re-search-forward
                (concat "\\(" citation-format "\\)" "\\("
                        ;; 匹配Roller_Physiology_International_1999_2014-02-27T02:02:21Z形式的key。
                        "\\([^^\"@\\&$#%',={} \t\n\f]*_\\)\\{4\\}\\(19\\|20\\)[[:digit:]]\\{2\\}\\(-[[:digit:]]\\{2\\}\\)\\{2\\}T\\([[:digit:]]\\{2\\}:\\)\\{2\\}[[:digit:]]\\{2\\}Z"
                        "\\)") nil t)
          (let ((beg (match-beginning 0))
                (end (match-end 0)))
            (if (and (>= current-point beg) (<= current-point end))
                (setq key (match-string 2))))))
      key))
  (defun swint-open-at-point (&optional in-emacs)
    "Open annotated file if annotation storage file exists."
    (interactive)
    (let ((annotated-file (swint-get-annotated-file))
          (key (swint-key-at-point)))
      (cond
       ((and annotated-file (file-exists-p annotated-file))
        (org-open-file annotated-file in-emacs))
       (key (let ((pdf-file (save-excursion (car (bibtex-completion-find-pdf-in-field key)))))
              (if pdf-file
                  (org-open-file pdf-file in-emacs)
                (message "No available pdf file for this citation."))))
       (t
        (org-open-at-point in-emacs)))))
  (defun swint-open-at-point-with-apps ()
    (interactive)
    (let ((org-file-apps
           (cond (is-lin '(("\\.pdf\\'" . "llpp %s")
                           ("\\.djvu\\'" . "llpp %s")
                           ("\\.png\\'" . "feh.sh %s")
                           ("\\.jpg\\'" . "feh.sh %s")
                           ("\\.bmp\\'" . "feh.sh %s")
                           ("\\.jpeg\\'" . "feh.sh %s")
                           ("\\.eps\\'" . "gv %s")
                           ("\\.ps\\'" . "gv %s")
                           ("\\.rmvb\\'" . "mplayer %s")
                           ("\\.rm\\'" . "mplayer %s")
                           ("\\.mp4\\'" . "mplayer %s")
                           ("\\.avi\\'" . "mplayer %s")
                           ("\\.flv\\'" . "mplayer %s")
                           ("\\.f4v\\'" . "mplayer %s")
                           ("\\.mpg\\'" . "mplayer %s")
                           ("\\.mkv\\'" . "mplayer %s")
                           ("\\.3gp\\'" . "mplayer %s")
                           ("\\.wmv\\'" . "mplayer %s")
                           ("\\.mov\\'" . "mplayer %s")
                           ("\\.dat\\'" . "mplayer %s")
                           ("\\.asf\\'" . "mplayer %s")
                           ("\\.mpeg\\'" . "mplayer %s")
                           ("\\.wma\\'" . "mplayer %s")
                           ("\\.gif\\'" . "mplayer %s")
                           ("\\.doc\\'" . "libreoffice %s")
                           ("\\.ppt\\'" . "libreoffice %s")
                           ("\\.xls\\'" . "libreoffice %s")
                           ("\\.ods\\'" . "libreoffice %s")
                           ("\\.odt\\'" . "libreoffice %s")
                           ("\\.docx\\'" . "libreoffice %s")
                           ("\\.pptx\\'" . "libreoffice %s")
                           ("\\.xlsx\\'" . "libreoffice %s")
                           ("\\.dxf\\'" . "librecad %s")
                           ("\\.html\\'" . "firefox %s")
                           ("\\.htm\\'" . "firefox %s")
                           ))
                 (is-win '((w32-browser))))))
      (swint-open-at-point)))
  ;; =============swint-open-at-point===========
;;;; mobileorg
  ;; =================mobileorg=================
  ;; Set to the location of your Org files on your local system.
  (setq org-directory "~/org")
  ;; Set to the name of the file where new notes will be stored.
  (setq org-mobile-inbox-for-pull "~/org/task-from-mobile.org")
  ;; Set to <your Dropbox root directory>/MobileOrg.
  (setq org-mobile-directory "~/Nutstore-mobileorg")
  ;; 加密。
  (setq org-mobile-encryption-tempfile "~/org/orgtmpcrypt")
  (unless (file-exists-p org-mobile-encryption-tempfile)
    (shell-command (concat "touch " (expand-file-name org-mobile-encryption-tempfile))))
  (unless (file-exists-p org-mobile-inbox-for-pull)
    (shell-command (concat "touch " (expand-file-name org-mobile-inbox-for-pull))))
  ;; 设置需要同步的文件。
  (setq org-agenda-files (list "~/org/task.org"))
  (setq org-mobile-files org-agenda-files)
  (defun swint-org-mobile-sync (arg)
    "Synchronization of org mobile."
    (interactive)
    (cond ((equal arg "down")
           ;; Webdav会造成文件conflict，在pull之前先删除本地mobileorg文件。
           (mapcar 'delete-file (directory-files org-mobile-directory t ".+\\.\\(org\\|dat\\)")))
          ((equal arg "up")
           (with-current-buffer "task.org" (org-mobile-push))))
    (let ((process
           (start-process-shell-command
            "webdav_sync" "*webdav_sync*"
            (concat "java -Dderby.system.home="  (expand-file-name "~/.webdav_sync/")
                    " -Dbe.re.http.no-compress -jar " (expand-file-name "~/.webdav_sync/webdav_sync1_1_6.jar")
                    " -r -" arg " -u https://wgq_713%40163.com:arxg55upvg9urwus@dav.jianguoyun.com/dav/Nutstore-mobileorg/ -d "
                    (expand-file-name "~/Nutstore-mobileorg/")))))
      (lexical-let ((pos (memq 'mode-line-modes mode-line-format))
                    (sync (cond ((equal arg "down") "pull")
                                ((equal arg "up") "push"))))
        (setcdr pos (cons (concat "Org-mobile " sync " ") (cdr pos)))
        (set-process-sentinel
         process
         (lambda (process signal)
           (when (memq (process-status process) '(exit signal))
             (let ((webdav_sync-process-output (with-current-buffer "*webdav_sync*"
                                                 (buffer-substring-no-properties (- (point-max) 6) (point-max))))
                   (pos (memq 'mode-line-modes mode-line-format)))
               (if (string-equal webdav_sync-process-output "Done.\n")
                   (with-current-buffer "task.org"
                     (when (equal sync "pull")
                       (insert-file-contents "~/Nutstore-mobileorg/task.org" nil nil nil t)
                       (org-mobile-pull)
                       (org-mobile-push))
                     (message "Org-mobile %s done." sync))
                 (message "Org-mobile %s failed." sync))
               (setcdr pos (remove (concat "Org-mobile " sync " ") (cdr pos))))))))))
  (define-key org-mode-map (kbd "C-c M-,") '(lambda () (interactive) (swint-org-mobile-sync "down")))
  (define-key org-mode-map (kbd "C-c M-.") '(lambda () (interactive) (swint-org-mobile-sync "up")))
  ;; =================mobileorg=================
;;;; org输出doc
  ;; =================org输出doc================
  ;; 先生成odt文件(需要zip支持)，然后使用libreoffice转化成doc文件。
  ;; 在win上转doc格式路径名中不能有中文。
  (setq org-odt-preferred-output-format "doc") ;v8
  (define-key org-mode-map (kbd "C-c C-S-e") 'org-odt-export-to-odt)
  ;; =================org输出doc================
;;;; org-latex-preview
  ;; =============org-latex-preview=============
  ;; org-preview-latex-fragment表示preview当前位置。
  ;; 加C-u表示当前节，两个C-u表示当前head。
  (define-key org-mode-map (kbd "C-c v") 'org-preview-latex-fragment)
  (setf org-highlight-latex-and-related '(latex)) ;高亮显示公式环境。
  ;; =============org-latex-preview=============
;;;; org输出latex
  ;; ==============org输出latex=================
  ;; (require 'org-install)
  ;; (require 'org-latex)
  ;; 使用上面的两个命令会导致输出成beamer的选项出不来。
  ;; 使用xelatex一步生成PDF。
  (setq org-latex-pdf-process
        '("xelatex -interaction nonstopmode %f"
          "xelatex -interaction nonstopmode %f"))
  (setq org-latex-to-pdf-process
        '("xelatex -interaction nonstopmode %f"
          "xelatex -interaction nonstopmode %f"))
  (when is-lin
    ;; 生成pdf自动用llpp打开。
    (add-hook 'org-mode-hook
              '(lambda ()
                 (delete '("\\.pdf\\'" . default) org-file-apps)
                 (add-to-list 'org-file-apps '("\\.pdf\\'" . "llpp %s")))))
  (defun swint-org-open-export-pdf ()
    "Start a viewer without confirmation.
The viewer is started either on region or master file,
depending on the last command issued."
    (interactive)
    (let ((output-file (cond
                        (is-lin (concat (file-name-base) ".pdf"))
                        (is-win (concat (file-name-directory buffer-file-name) (file-name-base) ".pdf")))))
      (if (file-exists-p output-file)
          (cond
           (is-lin (async-shell-command-no-output-buffer-from-file output-file))
           (is-win (w32-browser output-file)))
        (message "Warning: No export pdf."))))
  ;; 原来的好像有问题。
  ;; code执行免应答（Eval code without confirm）
  (setq org-confirm-babel-evaluate nil)
  (defun org-mode-article-modes ()
    (reftex-mode t)
    (and (buffer-file-name)
         (file-exists-p (buffer-file-name))
         (reftex-parse-all)))
  (add-hook 'org-mode-hook
            (lambda ()
              (if (member "REFTEX" org-todo-keywords-1)
                  (org-mode-article-modes))))
  ;; 崩溃啊，version 8的版本居然把org-export-latex-classes函数改名为org-latex-classes了。所以在新版本里面使用org-latex-classes。
  ;; (unless (boundp 'org-export-latex-classes)
  ;;   (setq org-export-latex-classes nil))
  (unless (boundp 'org-latex-classes)
    (setq org-latex-classes nil))
  ;; 定义org markup(*_+/=~)等的转换。
  (setq org-latex-text-markup-alist '((bold . "\\textbf{%s}")
                                      (code . verb)
                                      (italic . "\\emph{%s}")
                                      (strike-through . "\\sout{%s}")
                                      (underline . "\\underline{%s}")
                                      (verbatim . protectedtexttt)))
  ;; ==============org输出latex=================
;;;; article设置
  ;; ==============article设置==================
  (add-to-list 'org-latex-classes
               '("org-article"
                 "\\documentclass[11pt]{ctexart}
\\usepackage[top=1in,bottom=1in,left=0.8in,right=0.8in]{geometry}
\\usepackage{graphicx,amsmath,amssymb,subfigure,url,xspace,booktabs,tikz,float}
\\usepackage[autoplay,loop]{animate}
\\usepackage[absolute,overlay]{textpos}
\\usetikzlibrary{arrows,shapes,chains,calc,positioning,decorations.markings}
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
% \\usefonttheme[onlymath]{serif}
% \\setCJKmainfont{SimSun}
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
;; 使用Listings宏包格式化源代码(只是把代码框用listing环境框起来，还需要额外的设置。
(setq org-export-latex-listings t)
;; ================article设置==================
;;;; beamer设置
;; =================beamer设置==================
;; Beamer默认采用sansfont(无袖衬)，而不是mainfont(有袖衬)。
;; 设定mainfont会导致公式环境中变量变成正体。
;; 设定setsansfont使用Times New Roman无法使用英文斜体和粗体。
;; 使用某些字体可以实现粗斜体，例如DejaVu Sans/DejaVu Sans Mono/DejaVu Serif等。
(add-to-list 'org-latex-classes
             '("org-beamer"
               "\\documentclass[11pt]{beamer}
% [xcolor=dvipsnames]
\\usepackage{graphicx,subfigure,url,booktabs,tikz,float,fontspec}
\\usepackage{amsmath,amssymb}
\\DeclareGraphicsRule{*}{mps}{*}{}
\\usepackage{xmpmulti}
\\usepackage{colortbl,dcolumn}
\\usepackage[autoplay,loop]{animate}
\\usepackage[absolute,overlay]{textpos}
\\usetikzlibrary{arrows,shapes,chains,calc,positioning,decorations.markings}
\\usepackage{thumbpdf}
\\usepackage{wasysym}
\\usepackage{ucs}
% \\usepackage[utf8]{inputenc}
\\usepackage{pgf,pgfarrows,pgfnodes,pgfautomata,pgfheaps,pgfshade}
\\usepackage{verbatim}
\\usepackage[BoldFont,SlantFont,CJKnumber,CJKchecksingle]{xeCJK}
% \\usepackage{times}
% \\setmainfont{Times New Roman}
\\setsansfont{Times New Roman}
% {DejaVu Sans}{DejaVu Sans Mono}{DejaVu Serif}
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
\\usepackage{comment}
\\subtitle{}
\\subject{}
\\institute{沈阳建筑大学}
[NO-DEFAULT-PACKAGES]
[NO-PACKAGES]"
               ("\\section{%s}" . "\\section*{%s}")
               ;; ("\\begin{frame}[fragile]\\frametitle{%s}"
               ;;  "\\end{frame}"
               ;;  "\\begin{frame}[fragile]\\frametitle{%s}"
               ;;  "\\end{frame}")
               ))
(setq org-beamer-outline-frame-title "目录")
(use-package ox-beamer
  :config
  (add-to-list 'org-beamer-environments-extra
               '("onlyenv" "O" "\\begin{onlyenv}%a" "\\end{onlyenv}"))
  (add-to-list 'org-beamer-environments-extra
               '("uncoverenv" "U" "\\begin{uncoverenv}%a" "\\end{uncoverenv}")))
;; =================beamer设置==================
)
;; =================org-mode====================
;;; org-annotate
;; ===============org-annotate==================
;; Display annotated files with mark.
(use-package dired-x-highlight
  ;; Enabled after features.
  :load-path "site-lisp/org-annotate-file/"
  :defer t
  :after dired
  :config
  (defun swint-org-annotation-storage-file ()
    "Modified from var to function."
    (concat "~/org/annotated/annotated-("
            (replace-regexp-in-string
             "/" "_" (substring-no-properties (abbreviate-file-name default-directory) 1))
            ").org"))
  ;; Sync annotated status as operating.
  (use-package dired-sync-highlight
    :load-path "site-lisp/org-annotate-file/"))
;; 原有org-annotate-file用于全局注释。
(use-package org-annotate-file
  ;; Enabled at commands.
  :load-path "site-lisp/org-annotate-file/"
  :defer t
  :commands org-annotate-file
  :bind ("C-x L" . org-annotate-file-current)
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
(use-package swint-org-annotate-file
  ;; Enabled at commands.
  :load-path "site-lisp/org-annotate-file/"
  :defer t
  :commands swint-org-annotate-file
  :bind ("C-x l" . swint-org-annotate-file-current)
  :config
  (defun swint-org-annotate-file-current ()
    (interactive)
    (cond
     ((equal major-mode 'dired-mode)
      (swint-org-annotate-file (abbreviate-file-name (dired-get-filename))))
     ((equal major-mode 'pdf-view-mode)
      (dired-jump-other-window)
      (swint-org-annotate-file (abbreviate-file-name (dired-get-filename))))
     (t
      (switch-to-buffer-other-window (current-buffer))
      (swint-org-annotate-file (abbreviate-file-name (buffer-file-name)))))))
;; ===============org-annotate==================
;;; org-speed-commands
;; =============org-speed-commands==============
;; Activate single letter commands at beginning of a headline.
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
;; =============org-speed-commands==============
;;; outline
;; ==================outline====================
(use-package outline-magic
  ;; Enabled at commands.
  :defer t
  :commands outline-cycle)
(use-package outline
  ;; Enabled in modes.
  :defer t
  :commands outline-minor-mode
  :init
  (add-hook 'prog-mode-hook 'outline-minor-mode)
  (add-hook 'TeX-mode-hook 'outline-minor-mode)
  (add-hook 'message-mode-hook 'outline-minor-mode)
  (add-hook 'lisp-interaction-mode-hook
            (lambda () (outline-minor-mode -1)))
  (defvar outline-minor-mode-prefix "\M-O")
  :config
  (add-hook 'outline-minor-mode-hook
            (lambda ()
              ;; 在latex-mode和lisp-interaction-mode中不开启outshine。
              (unless (memq major-mode '(latex-mode eshell-mode))
                (outshine-hook-function))
              (smartrep-define-key outline-minor-mode-map "M-s"
                '(("p" . outline-previous-visible-heading)
                  ("n" . outline-next-visible-heading)
                  ("u" . outline-up-heading)
                  ("b" . outline-backward-same-level)
                  ("f" . outline-forward-same-level)))
              (define-key outline-minor-mode-map (kbd "<M-S-return>") 'outline-insert-heading)
              (define-key outline-minor-mode-map (kbd "<backtab>") 'outshine-cycle-buffer)
              (define-key outline-minor-mode-map (kbd "C-M-i") nil)))
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
    "List of regexps which define what a section can be.Ordered from deepest to highest level.")
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
  ;; Enabled at commands.
  :defer t
  :commands (outshine-hook-function outshine-cycle-buffer outshine-calc-outline-regexp)
  :config
  ;; Heading格式随mode不同，通常是M-;加*加空格。
  (setq outshine-use-speed-commands t)
  (setq outshine-imenu-show-headlines-p nil))
(use-package outorg
  ;; Enabled after features.
  ;; M-O # current heading.
  ;; C-u M-O # current buffer.
  ;; M-# outorg-copy-edits-and-exit.
  :defer t
  :after outshine)
(use-package navi-mode
  ;; Enabled after features.
  :defer t
  :after outshine
  :config
  (define-key outline-mode-prefix-map (kbd "i") 'navi-search-and-switch)
  (define-key outline-mode-prefix-map (kbd "o") 'navi-switch-to-twin-buffer)
  (global-set-key (kbd "M-s n") nil)
  (global-set-key (kbd "M-s s") 'swint-swiper)
  (global-set-key (kbd "M-s M-s") 'helm-swoop))
;; ==================outshine===================
;;; interleave
;; =================interleave==================
;; 安装和升级interleave时应确保pdf-tools已经加载，否则无法识别pdf-view中定义的函数。
(use-package interleave
  ;; Enabled at commands.
  :defer t
  :commands (swint-dired-interleave swint-interleave-open-notes-file-for-pdf)
  :init
  (add-hook 'org-mode-hook '(lambda ()
                              (define-key org-mode-map (kbd "C-c l") 'swint-interleave)
                              (define-key org-mode-map (kbd "C-c L") 'interleave-mode)))
  ;; 在加载interleave包之前必须先加载pdf-tools。
  (defun swint-interleave ()
    (interactive)
    (when is-lin
      (use-package pdf-tools))
    (let ((note-page (ignore-errors
                       (string-to-number (org-entry-get nil "interleave_page_note" t)))))
      (unless (org-at-top-heading-p)
        (re-search-backward "^\* " nil t))
      (let* ((annotated-file (swint-get-annotated-file))
             (key (org-entry-get nil "Custom_ID"))
             (pdf-file (save-excursion (car (bibtex-completion-find-pdf-in-field key)))))
        (unless (org-entry-get nil "interleave_pdf")
          (cond
           ((and annotated-file (file-exists-p annotated-file))
            (org-set-property "INTERLEAVE_PDF" annotated-file))
           (pdf-file
            (org-set-property "INTERLEAVE_PDF" (file-relative-name pdf-file))))))
      (call-interactively 'interleave-mode)
      (if note-page
          (progn
            (interleave--go-to-page-note note-page)
            (interleave-sync-pdf-page-current)))))
  :config
  (defun swint-dired-interleave ()
    (interactive)
    (let* ((pdf-file (dired-get-file-for-visit))
           (note-file (concat "~/org/interleave_notes/" (file-name-base pdf-file) ".org")))
      (if (file-exists-p note-file)
          (find-file note-file)
        (find-file pdf-file)
        (interleave-open-notes-file-for-pdf))))
  (defun swint-interleave-open-notes-file-for-pdf ()
    (interactive)
    (if (file-in-directory-p (buffer-file-name) (helm-get-firefox-user-init-dir))
        (when (or (derived-mode-p 'doc-view-mode)
                  (derived-mode-p 'pdf-view-mode))
          (let* ((current-pdf buffer-file-name)
                 (entry-for-pdf (bibtex-completion-get-entry-for-pdf current-pdf))
                 (key-for-pdf (list (bibtex-completion-get-value "=key=" entry-for-pdf))))
            (if entry-for-pdf
                (progn (bibtex-completion-edit-notes key-for-pdf)
                       (org-back-to-heading)
                       (swint-interleave)
                       (interleave-add-note))
              (message "Current pdf file is not in bibliography."))))
      (interleave-open-notes-file-for-pdf)))
  (smartrep-define-key interleave-mode-map "C-c"
    '(("c" . interleave-sync-pdf-page-current)
      ("p" . interleave-sync-pdf-page-previous)
      ("n" . interleave-sync-pdf-page-next)
      ("l" . interleave-mode)))
  (smartrep-define-key interleave-pdf-mode-map "C-c"
    '(("c" . interleave-sync-pdf-page-current)
      ("p" . interleave-sync-pdf-page-previous)
      ("n" . interleave-sync-pdf-page-next)
      ("l" . interleave-add-note)))
  (define-key interleave-mode-map (kbd "M-.") nil)
  (define-key interleave-mode-map (kbd "M-p") nil)
  (define-key interleave-mode-map (kbd "M-n") nil)
  (define-key interleave-pdf-mode-map (kbd "M-.") nil)
  (define-key interleave-pdf-mode-map (kbd "M-p") nil)
  (define-key interleave-pdf-mode-map (kbd "M-n") nil))
;; =================interleave==================
(provide 'setup_org_mode)
