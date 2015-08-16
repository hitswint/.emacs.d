;; ==========================auctex==============================
(load "auctex.el" nil t t)
(load "preview-latex.el" nil t t)
;; 有些设置使用LaTeX-mode-hook无效，可以使用TeX-mode-hook
;; ===========outline===============
(defun turn-on-outline-minor-mode ()
  (outline-minor-mode 1))
(add-hook 'TeX-mode-hook 'turn-on-outline-minor-mode)
;; (setq outline-minor-mode-prefix "\C-o") ;必须放在setup_anything_lacarte.el之前
;; (add-to-list 'load-path "~/.emacs.d/outline-magic/")
(add-hook 'outline-minor-mode-hook
          (lambda ()
            (require 'outline-magic)
            (define-key outline-minor-mode-map (kbd "C-M-i") 'outline-cycle)
            (define-key outline-minor-mode-map (kbd "C-c C-p") 'outline-previous-visible-heading)
            (define-key outline-minor-mode-map (kbd "C-c C-n") 'outline-next-visible-heading)
            (define-key outline-minor-mode-map (kbd "C-c C-u") 'outline-up-heading)
            (define-key outline-minor-mode-map (kbd "C-x C-p") 'outline-backward-same-level)
            (define-key outline-minor-mode-map (kbd "C-x C-n") 'outline-forward-same-level)
            ))
;; ===========outline===============
;; ===========preview===============
(set-default 'preview-scale-function 1.5)
(setq preview-auto-cache-preamble t)
(when is-win
  (setq preview-image-type 'pnm)
  (setq preview-gs-command "c:/Program Files (x86)/gs/gs9.09/bin/gswin32c.exe"))
;; C-c C-p C-p 如果有选中，则preview选中，如果无选中，则preview全部buffer
;; C-c C-p C-c C-b 取消buffer的preview
;; ===========preview===============
;; =============自动fold===========
(add-hook 'TeX-mode-hook
          (lambda () (TeX-fold-mode 1))) ; Automatically activate TeX-fold-mode.
;; C-c C-o C-b打开fold，C-c C-o b关闭fold
;; =============自动fold===========
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)
(add-hook 'TeX-mode-hook 'LaTeX-install-toolbar)
(add-hook 'TeX-mode-hook 'turn-on-auto-fill)
(add-hook 'TeX-mode-hook 'turn-on-reftex)
(add-hook 'TeX-mode-hook 'turn-on-orgtbl)
;;为 LaTeX 模式 hook 自动换行，数学公式，reftex 和显示行号的功能。
(mapc (lambda (mode)
        (add-hook 'TeX-mode-hook mode))
      (list 'auto-fill-mode
            'LaTeX-math-mode
            'turn-on-reftex
            ;; 'linum-mode
            ))
;;在 LaTeX mode 中，默认开启 PDF mode，即默认使用 xelatex 直接生成 pdf 文件，而不用每次用 'C-c C-t C-p' 进行切换。设置 'Tex-show-compilation' 为t，在另一个窗口显示编译信息，对于错误的排除很方便。另外，编译时默认直接保存文件，绑定补全符号到 TAB 键。
(add-hook 'LaTeX-mode-hook
          (lambda ()
            (setq TeX-auto-untabify t     ; remove all tabs before saving
                  TeX-engine 'xetex       ; use xelatex default
                  ;; TeX-show-compilation t
                  ) ; display compilation windows
            (TeX-global-PDF-mode t)       ; PDF mode enable, not plain
            (setq TeX-save-query nil)
            (imenu-add-menubar-index)
            (define-key LaTeX-mode-map (kbd "C-c r") 'reftex-parse-all)
            (define-key LaTeX-mode-map (kbd "M-s M-m") 'TeX-insert-macro)
            (define-key LaTeX-mode-map (kbd "TAB") 'TeX-complete-symbol)
            (define-key LaTeX-mode-map (kbd "C-q") 'swint-kill-tex-buffer)
            (define-key orgtbl-mode-map (kbd "C-c |") 'org-table-create-or-convert-from-region)
            (define-key LaTeX-mode-map (kbd "C-c C-x C-p") 'preview-at-point)
            (define-key LaTeX-mode-map (kbd "C-c C-x p") 'preview-clearout-buffer)
            (define-key LaTeX-mode-map (kbd "C-c C-x b") 'helm-bibtex)
            (define-key LaTeX-mode-map (kbd "C-c RET") nil)
            (define-key LaTeX-mode-map (kbd "C-c C-j") nil)
            (define-key LaTeX-mode-map (kbd "C-c C-i") nil)
            (define-key LaTeX-mode-map (kbd "C-c C-o") nil)
            (define-key LaTeX-mode-map (kbd "C-c C-f") nil)
            (define-key LaTeX-mode-map (kbd "\"") nil)
            ))
;; 关闭tex的同时关闭latexmk编译进程
(defun swint-kill-tex-buffer ()
  "kill tex buffer with active process"
  (interactive)
  (if (TeX-active-process)
      (kill-process (TeX-active-process))
    )
  (dirtree-exist-kill-this-buffer))
(setq TeX-view-program-list
      '(("Llpp" "llpp %o")
        ("Firefox" "firefox %o")))
(cond
 ((eq system-type 'gnu/linux)
  (add-hook 'LaTeX-mode-hook
            (lambda ()
              (setq TeX-view-program-selection '((output-pdf "Llpp")
                                                 (output-dvi "Llpp")))))))
;; ==========================auctex==============================
;; ==========================reftex==============================
(setq reftex-plug-into-AUCTeX t)
(setq reftex-format-cite-function
      '(lambda (key fmt)
         (let ((cite (replace-regexp-in-string "%l" key fmt)))
           (if (or (= ?~ (string-to-char fmt))
                   (member (preceding-char) '(?\ ?\t ?\n ?~)))
               cite
             (concat "~" cite)))))
;; toc buffer在左侧
(setq reftex-toc-split-windows-horizontally t)
;; toc buffer使用这个frame的比例
(setq reftex-tocc-split-windows-fraction 0.2)
;; ==========================reftex==============================
;; ===================auctex-latexmk=============================
;; 安装了texlive2009及更高的版本之后，默认就有latexmk，不用做任何改变。只需要加入.latexmkrc的配置文件和这个auctex-latexmk。
;; (add-to-list 'load-path "~/.emacs.d/auctex-latexmk")
(require 'auctex-latexmk)
(auctex-latexmk-setup)
;; ===================auctex-latexmk=============================
;; =================latex插入截图====================
;;1. suspend current emacs window
;;2. call scrot to capture the screen and save as a file in $HOME/.emacs.img/
;;3. put the png file reference in current buffer, like this:  [[/home/path/.emacs.img/1q2w3e.png]]
(defun my-screenshot-tex ()
  "Take a screenshot into a unique-named file in the current buffer file
  directory and insert a link to this file."
  (interactive)
  ;; 将截图名字定义为buffer名字加日期
  (cond
   (is-lin
    (setq filename
          (concat (make-temp-name
                   (concat (getenv "HOME") "/org/annotated/" (file-name-base (buffer-name))
                           "_"
                           (format-time-string "%Y%m%d_"))) ".png"))
    (suspend-frame)
    (call-process-shell-command "scrot" nil nil nil nil " -s " (concat
                                                                "\"" filename "\"" )))
   (is-win
    ;; turn into path in windows type
    (setq filename
          (concat (getenv "HOME") "/org/annotated/" (file-name-base (buffer-name))
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
  (if (file-exists-p "./pic")
      ()
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
             (define-key LaTeX-mode-map (kbd "C-c P") 'my-screenshot-tex)
             ))
;; win上跟lin上不同，需要先使用截图工具进行截图并复制，然后C-c p
;; =================latex插入截图====================
;; ======================zotelo=============================
;; (add-to-list 'load-path "~/.emacs.d/zotelo")
(require 'zotelo)
(add-hook 'TeX-mode-hook 'zotelo-minor-mode)
;; 使用方法，C-c z c建立bib文件，C-c z u更新bib文件，C-c [引用
;; 刚添加bib时会显示没有有效的bib文件，需要重新打开或者重启emacs。
;; ======================zotelo=============================
;; ======================pandoc=============================
(defun pandoc-latex-to-doc ()
  (interactive)
  (shell-command (concat "pandoc -o " (file-name-base (buffer-name)) ".docx " (buffer-name))))
(global-set-key (kbd "C-c C-S-e") 'pandoc-latex-to-doc)
;; ======================pandoc=============================
(provide 'setup_latex)
