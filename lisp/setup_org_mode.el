;; ================================org-mode====================================
;; (add-to-list 'load-path "~/.emacs.d/org-8.2.1/lisp")
;; (setq load-path (cons "~/.emacs.d/org-8.2.1/lisp" load-path))
(require 'org)
(add-hook 'org-mode-hook 'turn-on-font-lock)
(setq org-hide-leading-stars t)
;; (setq org-startup-indented t)
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c b") 'org-iswitchb)
(add-hook 'org-mode-hook
          (lambda ()
            (setq truncate-lines nil)
            ))
(setq org-capture-templates
      '(
        ("i" "Idea" entry (file+headline "~/org/task.org" "Idea List")
         "* TODO %? %^g")
        ("w" "Work" entry (file+headline "~/org/notes-work.org" "Work")
         "* %? %U %^g")
        ("c" "Computer" entry (file+headline "~/org/notes-computer.org" "Computer")
         "* %? %U %^g")
        ("o" "Others" entry (file+headline "~/org/notes-others.org" "Others")
         "* %? %U %^g")
        ("j" "Journal" entry (file+datetree "~/org/journal.org.gpg")
         "* %? %U")
        ))
;; %^{Description}
;; 禁用org-mode本身定义得C-tab快捷键，使全局快捷键生效
(add-hook 'org-mode-hook
          '(lambda ()
             (define-key org-mode-map (kbd "<C-M-return>") 'org-insert-todo-heading)
             (define-key org-mode-map (kbd "C-c C-b") 'org-beamer-select-environment)
             (define-key org-mode-map (kbd "C-c C-v") 'swint-org-open-export-pdf)
             (define-key org-mode-map (kbd "C-c i") 'my-org-open-at-point)
             (define-key org-mode-map (kbd "C-c o") 'org-open-at-point)
             (define-key org-mode-map (kbd "C-c C-x C-p") 'org-preview-latex-fragment)
             (define-key org-mode-map (kbd "C-c C-i") nil)
             (define-key org-mode-map (kbd "C-c C-o") nil)
             (define-key org-mode-map (kbd "C-c C-j") nil)
             (define-key org-mode-map (kbd "C-c RET") nil)
             (define-key org-mode-map (kbd "C-o") nil)
             (define-key org-mode-map (kbd "M-a") nil)
             (define-key org-mode-map (kbd "M-e") nil)
             (define-key org-mode-map (kbd "C-M-i") 'org-cycle)
             (define-key org-mode-map (kbd "C-c C-p") 'outline-previous-visible-heading)
             (define-key org-mode-map (kbd "C-c C-n") 'outline-next-visible-heading)
             (define-key org-mode-map (kbd "C-c C-u") 'outline-up-heading)
             (define-key org-mode-map (kbd "C-x C-p") 'outline-backward-same-level)
             (define-key org-mode-map (kbd "C-x C-n") 'outline-forward-same-level)
             (define-key org-mode-map (kbd "C-a") nil)
             (define-key org-mode-map (kbd "C-e") 'end-of-line)
             (define-key org-mode-map (kbd "C-j") nil)
             (define-key org-mode-map (kbd "C-c j") nil)
             (define-key org-mode-map (kbd "C-c =") nil)
             (define-key org-mode-map (kbd "C-c C-f") nil)
             (define-key org-mode-map [(control \,)] nil)
             (define-key org-mode-map [(control \.)] nil)
             (define-key org-mode-map [(control \#)] nil)
             (define-key org-mode-map [(control tab)] nil)
             (define-key org-mode-map [(control \')] nil)))
;;; do not show title of task in mode-line when using org-clock
(setq org-clock-heading-function
      (lambda ()
        (substring (nth 4 (org-heading-components)) 0 0)))
;; ======================org标注工具=============================
;; (add-to-list 'load-path "~/.emacs.d/org-annotate-file")
;; 原有org-annotate-file用于全局注释
(require 'org-annotate-file)
(setq org-annotate-file-storage-file "~/org/annotated.org")
(global-set-key (kbd "C-x C-l") 'org-annotate-file)
(define-key dired-mode-map (kbd "C-x C-l") '(lambda () (interactive)
                                              (org-annotate-file (abbreviate-file-name (dired-get-filename)))))
;; 新建swint-org-annotate-file.el用于局部注释
(require 'swint-org-annotate-file)
(setq swint-org-annotate-file-storage-file "./annotated.org")
(global-set-key (kbd "C-c C-l") 'swint-org-annotate-file)
(define-key dired-mode-map (kbd "C-c C-l") '(lambda () (interactive)
                                              (swint-org-annotate-file (abbreviate-file-name (dired-get-filename)))))
;; Display annotated files with mark
(require 'dired-x-annotated)
;; ======================org标注工具=============================
;; ==============显示两周的agenda==================
(setq org-agenda-span 14)
;; ==============显示两周的agenda==================
;; =======设定todo的子项完成后主项自动完成==========
(defun my-org-autodone (n-done n-not-done)
  (let (org-log-done org-log-states)
    (org-todo (if (= n-not-done 0) "DONE" "TODO"))))
(add-hook 'org-after-todo-statistics-hook 'my-org-autodone)
;; =======设定todo的子项完成后主项自动完成==========
;; =======设定todo关键词==========
(setq org-todo-keywords
      '((sequence "TODO(t)" "Waiting(w)" "Started(s)" "|" "DONE(d)" "Aborted(a)")
        ))
;; |后面的项以绿颜色的字出现，(a!/@)：()中出现!和@分别代表记录状态改变的时间以及需要输入备注，多个状态时使用/分隔
;; =======设定todo关键词==========
;; =======================org输出latex=============================
(add-hook 'org-mode-hook
          (lambda () (setq truncate-lines nil)))
;; (require 'org-install)
;; (require 'org-latex)
;; 使用上面的两个命令会导致输出成beamer的选项出不来
;; 使用xelatex一步生成PDF
(setq org-latex-pdf-process
      '("xelatex -interaction nonstopmode %f"
        "xelatex -interaction nonstopmode %f"))
(setq org-latex-to-pdf-process
      '("xelatex -interaction nonstopmode %f"
        "xelatex -interaction nonstopmode %f"))
;; 生成pdf自动用llpp打开
(add-hook 'org-mode-hook
          '(lambda ()
             (delete '("\\.pdf\\'" . default) org-file-apps)
             (add-to-list 'org-file-apps '("\\.pdf\\'" . "llpp %s"))))
(defun swint-org-open-export-pdf ()
  "Start a viewer without confirmation.
The viewer is started either on region or master file,
depending on the last command issued."
  (interactive)
  (let ((output-file (concat (file-name-base (buffer-name)) ".pdf")))
    (if (file-exists-p output-file)
        (async-shell-command-no-output-buffer-from-file output-file)
      (message "Warning: No export pdf."))))
;; 原来的好像有问题
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
;; ====================article设置========================
(add-to-list 'org-latex-classes
             '("org-article"
               "\\documentclass[11pt]{ctexart}
\\usepackage[top=1in,bottom=1in,left=0.8in,right=0.8in]{geometry}
\\usepackage{graphicx,amsmath,amssymb,subfigure,url,xspace,booktabs,tikz,float}
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
\\usepackage[]{caption}
\\captionsetup{font={small,it}}
\\usepackage{comment}
[NO-DEFAULT-PACKAGES]
[NO-PACKAGES]"
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
;; 使用Listings宏包格式化源代码(只是把代码框用listing环境框起来，还需要额外的设置)
(setq org-export-latex-listings t)
;; ;; ==========beamer设置===================
(add-to-list 'org-latex-classes
             '("org-beamer"
               "\\documentclass[11pt]{beamer}
% [xcolor=dvipsnames]
\\usepackage{graphicx,subfigure,url,booktabs,tikz,float}
\\usepackage{amsmath,amssymb}
\\usepackage{graphicx}
\\DeclareGraphicsRule{*}{mps}{*}{}
\\usepackage{subfigure}
\\usepackage{xmpmulti}
\\usepackage{colortbl,dcolumn}
\\usepackage{thumbpdf}
\\usepackage{wasysym}
\\usepackage{ucs}
\\usepackage[utf8]{inputenc}
\\usepackage{pgf,pgfarrows,pgfnodes,pgfautomata,pgfheaps,pgfshade}
\\usepackage{verbatim}
\\usepackage{xeCJK}
% \\setmainfont{Times New Roman}
\\setsansfont{Times New Roman}
\\setCJKmainfont{SimSun}
\\usefonttheme[onlymath]{serif}
% \\usepackage{times}
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
;; =======================org输出latex=============================
;; =======================org使用ditaa输出ascii图片==========================
(setq org-ditaa-jar-path "~/.emacs.d/org-8.2.1/contrib/scripts/ditaa.jar")
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
;; =======================org使用ditaa输出ascii图片==========================
;; ==============org中输入公式======================
;; (add-to-list 'load-path "~/.emacs.d/org-cdlatex-mode")
(add-hook 'org-mode-hook 'turn-on-org-cdlatex)
;; Environment templates can be inserted with C-c {.
;; The <TAB> key will do template expansion if the cursor is inside a LaTeX fragment1. For example, <TAB> will expand fr to \frac{}{} and position the cursor correctly inside the first brace. Another <TAB> will get you into the second brace. Even outside fragments, <TAB> will expand environment abbreviations at the beginning of a line. For example, if you write ‘equ’ at the beginning of a line and press <TAB>, this abbreviation will be expanded to an equation environment. To get a list of all abbreviations, type M-x cdlatex-command-help.
;; Pressing _ and ^ inside a LaTeX fragment will insert these characters together with a pair of braces. If you use <TAB> to move out of the braces, and if the braces surround only a single character or macro, they are removed again (depending on the variable cdlatex-simplify-sub-super-scripts).
;; Pressing the backquote ` followed by a character inserts math macros, also outside LaTeX fragments. If you wait more than 1.5 seconds after the backquote, a help window will pop up.
;; Pressing the single-quote ' followed by another character modifies the symbol before point with an accent or a font. If you wait more than 1.5 seconds after the single-quote, a help window will pop up. Character modification will work only inside LaTeX fragments; outside the quote is normal.
;; ==============org中输入公式======================
;; ==============截图================
;; 只截图，而不在文件中插入
;; screenshot-local截图到./pic文件夹中，screenshot截图到home/swint/org/annotated文件夹中。
(defun my-screenshot ()
  "Take a screenshot into a unique-named file in the current buffer file
  directory and insert a link to this file."
  (interactive)
  ;; 将截图名字定义为buffer名字加日期
  (setq filename
        (concat (make-temp-name
                 (concat (getenv "HOME") "/org/annotated/" (file-name-base (buffer-name))
                         "_"
                         (format-time-string "%Y%m%d_"))) ".png"))
  (suspend-frame)
  (call-process-shell-command "scrot" nil nil nil nil " -s " (concat
                                                              "\"" filename "\"" ))
  )
(defun my-screenshot-local ()
  "Take a screenshot into a unique-named file in the current buffer file
  directory and insert a link to this file."
  (interactive)
  ;; 将截图名字定义为buffer名字加日期
  (if (file-exists-p "./pic")
      ()
    ;; 建立pic文件夹
    (dired-create-directory "./pic"))
  (setq filename
        (concat (make-temp-name
                 (concat "./pic/" (file-name-base (buffer-name))
                         "_"
                         (format-time-string "%Y%m%d_"))) ".png"))
  (suspend-frame)
  (call-process-shell-command "scrot" nil nil nil nil " -s " (concat
                                                              "\"" filename "\"" ))
  )
(global-set-key (kbd "C-c M-p") 'my-screenshot-local)
(global-set-key (kbd "C-x M-p") 'my-screenshot)
;; ==============截图================
;; =================org插入截图====================
;;1. suspend current emacs window
;;2. call scrot to capture the screen and save as a file in $HOME/.emacs.img/
;;3. put the png file reference in current buffer, like this:  [[/home/path/.emacs.img/1q2w3e.png]]
(add-hook 'org-mode-hook 'iimage-mode) ; enable iimage-mode for org-mode
(add-hook 'org-mode-hook 'org-display-inline-images)
(define-key org-mode-map (kbd "C-c C-x C-v") '(lambda ()
                                                (interactive)
                                                (if (iimage-mode)
                                                    (turn-off-iimage-mode)
                                                  (turn-on-iimage-mode))
                                                (org-toggle-inline-images)))
;; 图片显示受到两个因素的影响，只有iimage-mode和org-display-inline-images都打开才能显示图片。
(defun my-screenshot-org ()
  "Take a screenshot into a unique-named file in the current buffer file
  directory and insert a link to this file."
  (interactive)
  ;; 将截图名字定义为buffer名字加日期
  (setq filename
        (concat (make-temp-name
                 (concat (getenv "HOME") "/org/annotated/" (file-name-base (buffer-name))
                         "_"
                         (format-time-string "%Y%m%d_"))) ".png"))
  (suspend-frame)
  (call-process-shell-command "scrot" nil nil nil nil " -s " (concat
                                                              "\"" filename "\"" ))
  (insert (concat "[[" filename "]]"))
  (org-display-inline-images)
  )
(defun my-screenshot-org-local ()
  "Take a screenshot into a unique-named file in the current buffer file
  directory and insert a link to this file."
  (interactive)
  ;; 将截图名字定义为buffer名字加日期
  (if (file-exists-p "./pic")
      ()
    ;; 建立pic文件夹
    (dired-create-directory "./pic"))
  (setq filename
        (concat (make-temp-name
                 (concat "./pic/" (file-name-base (buffer-name))
                         "_"
                         (format-time-string "%Y%m%d_"))) ".png"))
  (suspend-frame)
  (call-process-shell-command "scrot" nil nil nil nil " -s " (concat
                                                              "\"" filename "\"" ))
  (insert (concat "[[" filename "]]"))
  (org-display-inline-images)
  )
(add-hook 'org-mode-hook
          '(lambda ()
             (define-key org-mode-map (kbd "C-c p") 'my-screenshot-org-local)
             (define-key org-mode-map (kbd "C-x p") 'my-screenshot-org)
             ))
;; org中打开和关闭图片显示(org-display-inline-images)和(org-remove-inline-images)，可以使用(org-toggle-inline-images)快捷键为C-c C-x C-v。
;; =================org插入截图====================
;; =================org中使用外部程序打开文件=================
(defun my-org-open-at-point ()
  (interactive)
  (let ((org-file-apps '(("\\.pdf\\'" . "llpp %s")
                         ("\\.djvu\\'" . "llpp %s")
                         ("\\.png\\'" . "~/feh.sh %s")
                         ("\\.jpg\\'" . "~/feh.sh %s")
                         ("\\.bmp\\'" . "~/feh.sh %s")
                         ("\\.jpeg\\'" . "~/feh.sh %s")
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
                         )))
    (org-open-at-point)
    ))
;; =================org中使用外部程序打开文件=================
;; =================mobileorg===============
;; Set to the location of your Org files on your local system
(setq org-directory "~/org")
;; Set to the name of the file where new notes will be stored
(setq org-mobile-inbox-for-pull "~/org/task-from-mobile.org")
;; Set to <your Dropbox root directory>/MobileOrg.
(setq org-mobile-directory "~/Nutstore-mobileorg")
;; 加密
(setq org-mobile-encryption-tempfile "~/org/orgtmpcrypt")
(unless (file-exists-p org-mobile-encryption-tempfile)
  (shell-command (concat "touch " org-mobile-encryption-tempfile)))
(unless (file-exists-p org-mobile-inbox-for-pull)
  (shell-command (concat "touch " org-mobile-inbox-for-pull)))
;; 设置需要同步的文件
(setq org-agenda-files (list "~/org/task.org"))
(setq org-mobile-files org-agenda-files)
;; 在不同gtd文件之间refile
;; (custom-set-variables
;;  '(org-refile-targets
;;    (quote
;;     (("Gtd-task.org" :maxlevel . 1)("Gtd-project.org" :maxlevel . 1) ("Gtd-maybe.org":maxlevel . 1) ("Gtd-done-aborted.org":maxlevel . 1)))))
(defun swint-org-mobile-pull ()
  "Operate on multiple PCs"
  (interactive)
  (insert-file-contents "~/Nutstore-mobileorg/task.org" nil nil nil t)
  (org-mobile-pull)
  (org-mobile-push))
(global-set-key (kbd "C-c M-,") 'swint-org-mobile-pull)
(global-set-key (kbd "C-c M-.") 'org-mobile-push)
;; =================mobileorg===============
;; =======================org输出doc=============================
;; 先生成odt文件(需要zip支持)，然后使用libreoffice转化成doc文件
(setq org-odt-preferred-output-format "doc") ;v8
(define-key org-mode-map (kbd "C-c C-S-e") 'org-odt-export-to-odt)
;; =======================org输出doc=============================
;; =======================org-latex-preview======================
;; C-c C-x C-l org-preview-latex-fragment表示preview当前位置
;; 加C-u表示当前节，两个C-u表示当前head
(eval-after-load 'org
  '(setf org-highlight-latex-and-related '(latex))) ;高亮显示公式环境
;; =======================org-latex-preview======================
;; ================================org-mode====================================
(provide 'setup_org_mode)
