;;; auctex
;; =====================auctex=====================
;; 手动编译的auctex需要加下面两句，package安装不需要。
;; (load "auctex.el" nil t t)
;; (load "preview-latex.el" nil t t)
(use-package tex
  ;; Enabled in latex-mode.
  ;; 使用package原设定。
  :defer t
  :config
;;;; reftex
  ;; ===================reftex=====================
  (setq reftex-plug-into-AUCTeX t
        reftex-toc-split-windows-horizontally t
        reftex-tocc-split-windows-fraction 0.2)
  (setq reftex-format-cite-function
        '(lambda (key fmt)
           (let ((cite (replace-regexp-in-string "%l" key fmt)))
             (if (or (= ?~ (string-to-char fmt))
                     (member (preceding-char) '(?\ ?\t ?\n ?~)))
                 cite
               (concat "~" cite)))))
  ;; ===================reftex=====================
;;;; preview
  ;; ==================preview=====================
  (set-default 'preview-scale-function 1.5)
  (setq preview-auto-cache-preamble t)
  (when is-win
    (setq preview-image-type 'pnm)
    (setq preview-gs-command "c:/Program Files (x86)/gs/gs9.09/bin/gswin32c.exe"))
  ;; C-c C-x p preview选中或者全部。
  ;; C-c C-x P 取消buffer的preview。
  ;; ==================preview=====================
;;;; setup-and-keybindings
  ;; ============setup-and-keybindings=============
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq-default TeX-master nil)
  ;; Auctex在打开tex文件时加载，LaTeX-insert-left-brace会覆盖全局的括号定义。
  ;; 打开auctex自带的默认输入右括号的选项。
  (setq LaTeX-electric-left-right-brace t)
  ;; 为 LaTeX 模式 hook 自动换行，数学公式，reftex 和显示行号的功能。
  (mapc (lambda (mode)
          (add-hook 'TeX-mode-hook mode))
        (list 'auto-fill-mode
              'turn-on-auto-fill
              'LaTeX-math-mode
              'turn-on-reftex
              'turn-on-orgtbl
              'LaTeX-install-toolbar))
  ;; 在 LaTeX mode 中，默认开启 PDF mode，即默认使用 xelatex 直接生成 pdf 文件，而不用每次用 'C-c C-t C-p' 进行切换。设置 'Tex-show-compilation' 为t，在另一个窗口显示编译信息，对于错误的排除很方便。另外，编译时默认直接保存文件，绑定补全符号到 TAB 键。
  (add-hook 'LaTeX-mode-hook
            (lambda ()
              (setq TeX-auto-untabify t  ; remove all tabs before saving
                    TeX-engine 'xetex    ; use xelatex default
                    TeX-show-compilation t) ; display compilation windows
              (TeX-global-PDF-mode t)    ; PDF mode enable, not plain
              (setq TeX-save-query nil)
              (imenu-add-menubar-index)
              (define-key LaTeX-mode-map (kbd "C-c r") 'reftex-parse-all)
              (define-key LaTeX-mode-map (kbd "C-c f") 'TeX-font)
              (define-key LaTeX-mode-map (kbd "C-q") 'swint-kill-tex-buffer)
              (define-key LaTeX-mode-map (kbd "C-c C-x C-v") 'iimage-mode)
              (define-key LaTeX-mode-map (kbd "C-c C-x p") 'preview-at-point)
              (define-key LaTeX-mode-map (kbd "C-c C-x P") 'preview-clearout-buffer)
              (define-key LaTeX-mode-map (kbd "\"") nil)))
  (setq TeX-view-program-list
        '(("Llpp" "llpp %o")
          ("Firefox" "firefox %o")))
  ;; 使用imagemagick中convert转换为图片。win中默认使用imgconvert，可以将cygwin中convert改名为imgconvert。
  (add-to-list 'TeX-command-list '("LaTeX-standalone" "%`xelatex -shell-escape%(mode)%' %t" TeX-run-TeX nil t))
  (cond
   ((eq system-type 'gnu/linux)
    (add-hook 'LaTeX-mode-hook
              (lambda ()
                (setq TeX-view-program-selection '((output-pdf "Llpp")
                                                   (output-dvi "Llpp")))))))
  ;; ============setup-and-keybindings=============
;;;; 自动fold
  ;; ===================自动fold===================
  (add-hook 'TeX-mode-hook
            (lambda () (TeX-fold-mode 1))) ; Automatically activate TeX-fold-mode.
  ;; C-c C-o C-b打开fold，C-c C-o b关闭fold。
  ;; ===================自动fold===================
;;;; 关闭tex buffer
  ;; ================关闭tex buffer================
  ;; 关闭tex的同时关闭latexmk编译进程。
  (defun swint-kill-tex-buffer ()
    "kill tex buffer with active process"
    (interactive)
    (if (TeX-active-process)
        (kill-process (TeX-active-process)))
    (dirtree-kill-this-buffer))
  ;; ================关闭tex buffer================
;;;; latex插入截图
  ;; ================latex插入截图=================
  ;; 1. suspend current emacs window
  ;; 2. call scrot to capture the screen and save as a file in $HOME/.emacs.img/
  ;; 3. put the png file reference in current buffer, like this:  [[/home/path/.emacs.img/1q2w3e.png]]
  (defun my-screenshot-tex ()
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
      ;; turn into path in windows type
      (setq filename
            (concat (getenv "HOME") "/org/pic/" (file-name-base (buffer-name))
                    "_"
                    (format-time-string "%Y%m%d_") (make-temp-name "") ".png"))
      (setq windows-filename
            (replace-regexp-in-string "/" "\\" filename t t))
      (call-process "c:\\Program Files (x86)\\IrfanView\\i_view32.exe" nil nil nil (concat
                                                                                    "/clippaste /convert=" windows-filename))))
    (insert filename))
  (defun my-screenshot-tex-local ()
    "Take a screenshot into a unique-named file in the current buffer file
  directory and insert a link to this file."
    (interactive)
    ;; 将截图名字定义为buffer名字加日期
    (unless (file-exists-p "./pic")
      ;; 建立pic文件夹
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
            (concat  "./pic/" (file-name-base (buffer-name))
                     "_"
                     (format-time-string "%Y%m%d_") (make-temp-name "") ".png"))
      ;; turn into path in windows type
      (setq windows-filename
            (replace-regexp-in-string "/" "\\" filename t t))
      (call-process "c:\\Program Files (x86)\\IrfanView\\i_view32.exe" nil nil nil (concat
                                                                                    "/clippaste /convert=" windows-filename))))
    (insert filename))
  (add-hook 'LaTeX-mode-hook
            '(lambda ()
               (define-key LaTeX-mode-map (kbd "C-c p") 'my-screenshot-tex-local)
               (define-key LaTeX-mode-map (kbd "C-c P") 'my-screenshot-tex)))
  ;; win上跟lin上不同，需要先使用截图工具进行截图并复制，然后C-c p。
  ;; ================latex插入截图=================
;;;; pandoc
  ;; ===================pandoc=====================
  (defun pandoc-latex-to-doc ()
    (interactive)
    (shell-command (concat "pandoc -o " (file-name-base (buffer-name)) ".docx " (buffer-name))))
  (add-hook 'LaTeX-mode-hook
            '(lambda ()
               (define-key LaTeX-mode-map (kbd "C-c C-S-e") 'pandoc-latex-to-doc)))
  ;; ===================pandoc=====================
  )
;; =====================auctex=====================
;;; auctex-latexmk
;; =================auctex-latexmk=================
;; 安装了texlive2009及更高的版本之后，默认就有latexmk，不用做任何改变。只需要加入.latexmkrc的配置文件和这个auctex-latexmk。
(use-package auctex-latexmk
  ;; Enabled after features.
  :defer t
  :after tex
  :config
  (auctex-latexmk-setup))
;; =================auctex-latexmk=================
;;; zotelo
;; ====================zotelo======================
(use-package zotelo
  ;; Enabled in latex-mode.
  :defer t
  :commands zotelo-minor-mode
  :init
  (add-hook 'TeX-mode-hook 'zotelo-minor-mode))
;; 使用方法，C-c z c建立bib文件，C-c z u更新bib文件，C-c [引用。
;; 刚添加bib时会显示没有有效的bib文件，需要重新打开或者重启emacs。
;; ====================zotelo======================
;;; latex-preview-pane
;; ==============latex-preview-pane================
(use-package latex-preview-pane
  ;; Enabled at commands.
  :defer t
  :commands latex-preview-pane-mode
  :config
  ;; latex-preview-pane-enable绑定latex-mode-hook，无效。
  ;; (add-hook 'TeX-mode-hook (lambda () (latex-preview-pane-mode 1)))
  (define-key latex-preview-pane-mode-map (kbd "M-p") nil)
  (define-key latex-preview-pane-mode-map (kbd "M-P") nil)
  (setq pdf-latex-command "xelatex")
  ;; 使auctex编译支持synctex。
  (setq TeX-source-correlate-mode t)
  (setq TeX-source-correlate-method 'synctex)
  ;; 使latex-preview-pane编译支持synctex。
  (setq shell-escape-mode "--synctex=1"))
;; 原函数使用call-process同步编译，会导致hang。
;; 改用start-process异步编译会造成auto-revert-buffers错误。
;; 因为异步编译时，pdf-view仍然在更新，显示pdf文件被损坏。
;; ==============latex-preview-pane================
;;; magic-latex-buffer
;; ==============magic-latex-buffer================
(use-package magic-latex-buffer
  ;; Enabled in latex-mode.
  :defer t
  :commands magic-latex-buffer
  :init
  (add-hook 'TeX-mode-hook 'magic-latex-buffer)
  :config
  ;; Bug：Error running timer `font-latex-jit-lock-force-redisplay': (wrong-number-of-arguments (2 . 2) 3) [N times]
  ;; Redefine font-latex-jit-lock-force-redisplay to fix aboved bug.
  (defun font-latex-jit-lock-force-redisplay (buf start end)
    "Compatibility for Emacsen not offering `jit-lock-force-redisplay'."
    ;; The following block is an expansion of `jit-lock-force-redisplay'
    ;; and involved macros taken from CVS Emacs on 2007-04-28.
    (with-current-buffer buf
      (let ((modified (buffer-modified-p)))
        (unwind-protect
            (let ((buffer-undo-list t)
                  (inhibit-read-only t)
                  (inhibit-point-motion-hooks t)
                  (inhibit-modification-hooks t)
                  deactivate-mark
                  buffer-file-name
                  buffer-file-truename)
              (put-text-property start end 'fontified t))
          (unless modified
            (restore-buffer-modified-p nil)))))))
;; ==============magic-latex-buffer================
(provide 'setup_latex)
