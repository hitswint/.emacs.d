;; ==================org-mode==========================
(use-package org
  ;; Enabled in org-mode.
  :defer t
  :config
  (set-face-attribute 'org-level-5 nil :bold nil :foreground "cyan" :height 1.0)
  (set-face-attribute 'org-level-6 nil :bold nil :foreground "magenta" :height 1.0)
  (set-face-attribute 'org-level-7 nil :bold nil :foreground "purple" :height 1.0)
  (set-face-attribute 'org-level-8 nil :bold nil :foreground "gray" :height 1.0)
  (global-set-key (kbd "M-s o") nil)
  (global-set-key (kbd "M-s o l") 'org-store-link)
  (global-set-key (kbd "M-s o c") 'org-capture)
  (global-set-key (kbd "M-s o a") 'org-agenda)
  (setq org-capture-templates
        '(("i" "Idea" entry (file+headline "~/org/task.org" "Idea List") "* TODO %? %U %^g")
          ("w" "Work" entry (file+headline "~/org/notes-work.org" "Work") "* %? %U %^g")
          ("c" "Computer" entry (file+headline "~/org/notes-computer.org" "Computer") "* %? %U %^g")
          ("o" "Others" entry (file+headline "~/org/notes-others.org" "Others") "* %? %U %^g")
          ("j" "Journal" entry (file+datetree "~/org/journal.org.gpg") "* %? %U")))
  ;; %^{Description}
  (add-hook 'org-mode-hook
            '(lambda ()
               ;; 插入source code时高亮，C-c ' 打开相应major-mode编辑窗口。
               (setq org-src-fontify-natively t)
               ;; (setq org-startup-indented t)
               (setq truncate-lines nil)
               (setq org-hide-leading-stars t)
               (setq org-imenu-depth 8)
               (turn-on-font-lock)
               (define-key org-mode-map (kbd "<C-M-return>") 'org-insert-todo-heading)
               (define-key org-mode-map (kbd "C-c e") 'org-beamer-select-environment)
               (define-key org-mode-map (kbd "C-c C-v") 'swint-org-open-export-pdf)
               (define-key org-mode-map (kbd "C-c i") 'org-open-at-point-with-apps)
               (define-key org-mode-map (kbd "C-c o") '(lambda () (interactive) (swint-org-open-at-point t)))
               (define-key org-mode-map (kbd "C-c C-x p") 'org-preview-latex-fragment)
               (define-key org-mode-map (kbd "C-M-i") 'org-cycle)
               (define-key org-mode-map (kbd "C-c C-p") 'outline-previous-visible-heading)
               (define-key org-mode-map (kbd "C-c C-n") 'outline-next-visible-heading)
               (smartrep-define-key org-mode-map "C-c"
                 '(("C-u" . outline-up-heading)
                   ("C-b" . org-backward-heading-same-level)
                   ("C-f" . org-forward-heading-same-level)))
               (define-key org-mode-map (kbd "C-a") nil)
               (define-key org-mode-map (kbd "C-e") 'end-of-line)
               (define-key org-mode-map (kbd "C-j") nil)
               (define-key org-mode-map (kbd "C-o") nil)
               (define-key org-mode-map (kbd "M-a") nil)
               (define-key org-mode-map (kbd "M-e") nil)
               (define-key org-mode-map (kbd "RET") nil)
               (define-key org-mode-map [(control \,)] nil)
               (define-key org-mode-map [(control \.)] nil)
               (define-key org-mode-map [(control \#)] nil)
               (define-key org-mode-map [(control tab)] nil)
               (define-key org-mode-map [(control \')] nil)))
  ;; ==============GTD==================
  ;; Do not show title of task in mode-line when using org-clock.
  (setq org-clock-heading-function
        (lambda ()
          (substring (nth 4 (org-heading-components)) 0 0)))
  ;; 显示两周的agenda。
  (setq org-agenda-span 14)
  ;; 设定todo的子项完成后主项自动完成。
  (defun my-org-autodone (n-done n-not-done)
    (let (org-log-done org-log-states)
      (org-todo (if (= n-not-done 0) "DONE" "TODO"))))
  (add-hook 'org-after-todo-statistics-hook 'my-org-autodone)
  ;; 设定todo关键词。
  (setq org-todo-keywords
        '((sequence "TODO(t)" "Waiting(w)" "Started(s)" "|" "DONE(d)" "Aborted(a)")))
  ;; |后面的项以绿颜色的字出现，(a!/@)：()中出现!和@分别代表记录状态改变的时间以及需要输入备注，多个状态时使用/分隔。
  ;; ==============GTD==================
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
  ;; win中似乎不好使，应该是没装java。
  ;; =======================org使用ditaa输出ascii图片==========================
  ;; ==============org中输入公式======================
  (use-package cdlatex
    :defer t
    :init
    (add-hook 'org-mode-hook 'turn-on-org-cdlatex))
  ;; Environment templates can be inserted with C-c {.
  ;; The <TAB> key will do template expansion if the cursor is inside a LaTeX fragment1. For example, <TAB> will expand fr to \frac{}{} and position the cursor correctly inside the first brace. Another <TAB> will get you into the second brace. Even outside fragments, <TAB> will expand environment abbreviations at the beginning of a line. For example, if you write ‘equ’ at the beginning of a line and press <TAB>, this abbreviation will be expanded to an equation environment. To get a list of all abbreviations, type M-x cdlatex-command-help.
  ;; Pressing _ and ^ inside a LaTeX fragment will insert these characters together with a pair of braces. If you use <TAB> to move out of the braces, and if the braces surround only a single character or macro, they are removed again (depending on the variable cdlatex-simplify-sub-super-scripts).
  ;; Pressing the backquote ` followed by a character inserts math macros, also outside LaTeX fragments. If you wait more than 1.5 seconds after the backquote, a help window will pop up.
  ;; Pressing the single-quote ' followed by another character modifies the symbol before point with an accent or a font. If you wait more than 1.5 seconds after the single-quote, a help window will pop up. Character modification will work only inside LaTeX fragments; outside the quote is normal.
  ;; ==============org中输入公式======================
  ;; ==============截图================
  ;; screenshot-local截图到./pic文件夹中，screenshot截图到home/swint/org/pic文件夹中。
  (defun my-screenshot ()
    "Take a screenshot into a unique-named file in the current buffer file
  directory and insert a link to this file."
    (interactive)
    ;; 将截图名字定义为buffer名字加日期。
    (cond
     (is-lin
      (setq filename
            (concat (make-temp-name
                     (concat (getenv "HOME") "/org/pic/" (file-name-base (buffer-name))
                             "_"
                             (format-time-string "%Y%m%d_"))) ".png"))
      (suspend-frame)
      (call-process-shell-command "scrot" nil nil nil nil " -s " (concat
                                                                  "\"" filename "\"" )))
     (is-win
      (setq filename
            ;; 注释掉原来make-temp-name的方法，因为在win上对于某些prefix无法生成随机名字。
            ;; (concat (make-temp-name
            ;;          (concat (getenv "HOME") "/org/pic/" (file-name-base (buffer-name))
            ;;                  "_"
            ;;                  (format-time-string "%Y%m%d_"))) ".png")
            (concat (getenv "HOME") "/org/pic/" (file-name-base (buffer-name))
                    "_"
                    (format-time-string "%Y%m%d_") (make-temp-name "") ".png"))
      ;; Turn into path in windows type.
      (setq windows-filename
            (replace-regexp-in-string "/" "\\" filename t t))
      (call-process "c:\\Program Files (x86)\\IrfanView\\i_view32.exe" nil nil nil
                    (concat "/clippaste /convert=" windows-filename)))))
  (defun my-screenshot-local ()
    "Take a screenshot into a unique-named file in the current buffer file
  directory and insert a link to this file."
    (interactive)
    ;; 将截图名字定义为buffer名字加日期。
    (unless (file-exists-p "./pic")
      ;; 建立pic文件夹。
      (dired-create-directory "./pic"))
    (cond
     (is-lin
      (setq filename
            (concat (make-temp-name
                     (concat "./pic/" (file-name-base (buffer-name))
                             "_"
                             (format-time-string "%Y%m%d_"))) ".png"))
      (suspend-frame)
      (call-process-shell-command "scrot" nil nil nil nil " -s " (concat
                                                                  "\"" filename "\"" )))
     (is-win
      (setq filename
            (concat "./pic/" (file-name-base (buffer-name))
                    "_"
                    (format-time-string "%Y%m%d_") (make-temp-name "") ".png"))
      ;; Turn into path in windows type.
      (setq windows-filename
            (replace-regexp-in-string "/" "\\" filename t t))
      (call-process "c:\\Program Files (x86)\\IrfanView\\i_view32.exe" nil nil nil
                    (concat "/clippaste /convert=" windows-filename)))))
  (global-set-key (kbd "C-x p") 'my-screenshot-local)
  (global-set-key (kbd "C-x P") 'my-screenshot)
  ;; ==============截图================
  ;; =================org插入截图====================
  ;; 1. suspend current emacs window
  ;; 2. call scrot to capture the screen and save as a file in $HOME/.emacs.img/
  ;; 3. put the png file reference in current buffer, like this:  [[/home/path/.emacs.img/1q2w3e.png]]
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
    ;; 将截图名字定义为buffer名字加日期。
    (cond
     (is-lin
      (setq filename
            (concat (make-temp-name
                     (concat (getenv "HOME") "/org/pic/" (file-name-base (buffer-name))
                             "_"
                             (format-time-string "%Y%m%d_"))) ".png"))
      (suspend-frame)
      (call-process-shell-command "scrot" nil nil nil nil " -s " (concat
                                                                  "\"" filename "\"" )))
     (is-win
      (setq filename
            ;; 在org文件中显示图片只需要/Users/...，而不需要前面的c:。
            (concat (getenv "HOME") "/org/pic/" (file-name-base (buffer-name))
                    "_"
                    (format-time-string "%Y%m%d_") (make-temp-name "") ".png"))
      ;; Turn into path in windows type
      (setq windows-filename
            (replace-regexp-in-string "/" "\\" filename t t))
      (call-process "c:\\Program Files (x86)\\IrfanView\\i_view32.exe" nil nil nil (concat
                                                                                    "/clippaste /convert=" windows-filename))))
    (insert (concat "[[" (abbreviate-file-name filename) "]]"))
    (org-display-inline-images))
  (defun my-screenshot-org-local ()
    "Take a screenshot into a unique-named file in the current buffer file
  directory and insert a link to this file."
    (interactive)
    ;; 将截图名字定义为buffer名字加日期。
    (if (file-exists-p "./pic")
        ()
      ;; 建立pic文件夹。
      (dired-create-directory "./pic"))
    (cond
     (is-lin
      (setq filename
            (concat (make-temp-name
                     (concat "./pic/" (file-name-base (buffer-name))
                             "_"
                             (format-time-string "%Y%m%d_"))) ".png"))
      (suspend-frame)
      (call-process-shell-command "scrot" nil nil nil nil " -s " (concat
                                                                  "\"" filename "\"" )))
     (is-win
      (setq filename
            (concat "./pic/" (file-name-base (buffer-name))
                    "_"
                    (format-time-string "%Y%m%d_") (make-temp-name "") ".png"))
      ;; Turn into path in windows type.
      (setq windows-filename
            (replace-regexp-in-string "/" "\\" filename t t))
      (call-process "c:\\Program Files (x86)\\IrfanView\\i_view32.exe" nil nil nil (concat
                                                                                    "/clippaste /convert=" windows-filename))))
    (insert (concat "[[" filename "]]"))
    (org-display-inline-images))
  (add-hook 'org-mode-hook
            '(lambda ()
               (define-key org-mode-map (kbd "C-c p") 'my-screenshot-org-local)
               (define-key org-mode-map (kbd "C-c P") 'my-screenshot-org)))
  ;; win上跟lin上不同，需要先使用截图工具进行截图并复制，然后C-c p。
  ;; org中打开和关闭图片显示(org-display-inline-images)和(org-remove-inline-images)。
  ;; 可以使用(org-toggle-inline-images)快捷键为C-c C-x C-v。
  ;; =================org插入截图====================
  ;; =================org中使用外部程序打开文件=================
  (defun swint-org-open-at-point (&optional in-emacs)
    "Open annotated file if annotation storage file exists."
    (interactive)
    (let* ((annotated-file-link (org-get-heading))
           (annotated-file (concat "~"
                                   (if (string-prefix-p "annotated-(" (file-name-nondirectory (buffer-file-name)))
                                       (replace-regexp-in-string
                                        "_" "/" (substring-no-properties (file-name-nondirectory (buffer-file-name)) 11 -5)))
                                   (car (last (split-string (substring-no-properties annotated-file-link nil -2) "\\[file:") 1)))))
      (if (and (org-at-heading-p)
               (file-exists-p annotated-file))
          (org-open-file annotated-file in-emacs)
        (org-open-at-point in-emacs))))
  (cond
   (is-lin
    (defun org-open-at-point-with-apps ()
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
        (swint-org-open-at-point))))
   (is-win
    (defun org-open-at-point-with-apps ()
      (interactive)
      (let (org-file-apps w32-browser)
        (swint-org-open-at-point)))))
  ;; =================org中使用外部程序打开文件=================
  ;; =================mobileorg===============
  ;; Set to the location of your Org files on your local system.
  (setq org-directory "~/org")
  ;; Set to the name of the file where new notes will be stored.
  (setq org-mobile-inbox-for-pull "~/org/task-from-mobile.org")
  ;; Set to <your Dropbox root directory>/MobileOrg.
  (setq org-mobile-directory "~/Nutstore-mobileorg")
  ;; 加密
  (setq org-mobile-encryption-tempfile "~/org/orgtmpcrypt")
  (unless (file-exists-p org-mobile-encryption-tempfile)
    (shell-command (concat "touch " (expand-file-name org-mobile-encryption-tempfile))))
  (unless (file-exists-p org-mobile-inbox-for-pull)
    (shell-command (concat "touch " (expand-file-name org-mobile-inbox-for-pull))))
  ;; 设置需要同步的文件。
  (setq org-agenda-files (list "~/org/task.org"))
  (setq org-mobile-files org-agenda-files)
  ;; 在不同gtd文件之间refile。
  ;; (custom-set-variables
  ;;  '(org-refile-targets
  ;;    (quote
  ;;     (("Gtd-task.org" :maxlevel . 1)("Gtd-project.org" :maxlevel . 1) ("Gtd-maybe.org":maxlevel . 1) ("Gtd-done-aborted.org":maxlevel . 1)))))
  (defun swint-org-mobile-pull ()
    "Operate on multiple PCs."
    (interactive)
    ;; webdav会造成文件conflict，在pull之前先删除本地mobileorg文件。
    (mapcar 'delete-file (directory-files org-mobile-directory t
                                          ".+\\.\\(org\\|dat\\)"))
    (let ((process
           (start-process-shell-command
            "webdav_sync" "*webdav_sync*"
            (concat "java -Dderby.system.home="  (expand-file-name "~/.webdav_sync/")
                    " -Dbe.re.http.no-compress -jar " (expand-file-name "~/.webdav_sync/webdav_sync1_1_4.jar")
                    " -r -down -u https://wgq_713%40163.com:arxg55upvg9urwus@dav.jianguoyun.com/dav/Nutstore-mobileorg/ -d "
                    (expand-file-name "~/Nutstore-mobileorg/"))))
          (pos (memq 'mode-line-modes mode-line-format)))
      (setcdr pos (cons "org-mobile-pull " (cdr pos)))
      (set-process-sentinel
       process
       (lambda (process signal)
         (when (memq (process-status process) '(exit signal))
           (let ((webdav_sync-process-output (with-current-buffer "*webdav_sync*"
                                               (buffer-substring-no-properties (- (point-max) 6) (point-max))))
                 (pos (memq 'mode-line-modes mode-line-format)))
             (if (string-equal webdav_sync-process-output "Done.\n")
                 (with-current-buffer "task.org"
                   (insert-file-contents "~/Nutstore-mobileorg/task.org" nil nil nil t)
                   (org-mobile-pull)
                   (org-mobile-push)
                   (message "swint-org-mobile-pull done."))
               (message "swint-org-mobile-pull failed"))
             (setcdr pos (remove "org-mobile-pull " (cdr pos)))))))))
  (defun swint-org-mobile-push ()
    "Operate on multiple PCs"
    (interactive)
    (with-current-buffer "task.org"
      (org-mobile-push))
    (let ((process
           (start-process-shell-command
            "webdav_sync" "*webdav_sync*"
            (concat "java -Dderby.system.home="  (expand-file-name "~/.webdav_sync/")
                    " -Dbe.re.http.no-compress -jar " (expand-file-name "~/.webdav_sync/webdav_sync1_1_4.jar")
                    " -r -up -u https://wgq_713%40163.com:arxg55upvg9urwus@dav.jianguoyun.com/dav/Nutstore-mobileorg/ -d "
                    (expand-file-name "~/Nutstore-mobileorg/"))))
          (pos (memq 'mode-line-modes mode-line-format)))
      (setcdr pos (cons "org-mobile-push " (cdr pos)))
      (set-process-sentinel
       process
       (lambda (process signal)
         (when (memq (process-status process) '(exit signal))
           (let ((webdav_sync-process-output (with-current-buffer "*webdav_sync*"
                                               (buffer-substring-no-properties (- (point-max) 6) (point-max))))
                 (pos (memq 'mode-line-modes mode-line-format)))
             (if (string-equal webdav_sync-process-output "Done.\n")
                 (message "swint-org-mobile-push done.")
               (message "swint-org-mobile-push failed"))
             (setcdr pos (remove "org-mobile-push " (cdr pos)))))))))
  (define-key org-mode-map (kbd "C-c M-,") 'swint-org-mobile-pull)
  (define-key org-mode-map (kbd "C-c M-.") 'swint-org-mobile-push)
  ;; =================mobileorg===============
  ;; =======================org输出doc=============================
  ;; 先生成odt文件(需要zip支持)，然后使用libreoffice转化成doc文件。
  ;; 在win上转doc格式路径名中不能有中文。
  (setq org-odt-preferred-output-format "doc") ;v8
  (define-key org-mode-map (kbd "C-c C-S-e") 'org-odt-export-to-odt)
  ;; =======================org输出doc=============================
  ;; =======================org-latex-preview======================
  ;; C-c C-x C-l org-preview-latex-fragment表示preview当前位置。
  ;; 加C-u表示当前节，两个C-u表示当前head。
  (setf org-highlight-latex-and-related '(latex)) ;高亮显示公式环境
  ;; =======================org-latex-preview======================
  ;; 定义org markup(*_+/=~)等的转换。
  (setq org-latex-text-markup-alist '((bold . "\\textbf{%s}")
                                      (code . verb)
                                      (italic . "\\emph{%s}")
                                      (strike-through . "\\sout{%s}")
                                      (underline . "\\underline{%s}")
                                      (verbatim . protectedtexttt)))
  ;; =======================org输出latex=============================
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
                        (is-lin (concat (file-name-base (buffer-name)) ".pdf"))
                        (is-win (concat (file-name-directory buffer-file-name) (file-name-base (buffer-name)) ".pdf")))))
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
  ;; ====================article设置========================
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
;; ==========beamer设置===================
;; beamer默认采用sansfont(无袖衬)，而不是mainfont(有袖衬)。
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
               '("uncoverenv" "U" "\\begin{uncoverenv}%a" "\\end{uncoverenv}"))))
;; ======================org标注工具=============================
;; Display annotated files with mark.
(use-package dired-x-annotated
  ;; Enabled automatically.
  :load-path "site-lisp/org-annotate-file/"
  :init
  (defun swint-org-annotate-file-storage-file ()
    "Modified from var to function"
    (concat "~/org/annotated/annotated-("
            (replace-regexp-in-string
             "/" "_" (substring-no-properties (abbreviate-file-name default-directory) 1))
            ").org"))
  :config
  ;; Sync annotated status as operating.
  (use-package dired-sync-annotated
    :load-path "site-lisp/org-annotate-file/"))
;; 原有org-annotate-file用于全局注释。
(use-package org-annotate-file
  ;; Enabled at commands.
  :load-path "site-lisp/org-annotate-file/"
  :defer t
  :commands org-annotate-file
  :init
  (bind-key "C-x L" 'org-annotate-file-current)
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
  :init
  (bind-key "C-x l" 'swint-org-annotate-file-current)
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
;; ======================org标注工具=============================
;; ==================org-mode==========================
(provide 'setup_org_mode)
