;;; auctex
;; =====================auctex=====================
(use-package tex
  :mode ("\\.[tT][eE][xX]\\'" . latex-mode)
  :commands LaTeX-math-mode
  :config
;;;; setup-and-keybindings
  ;; ============setup-and-keybindings=============
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq-default TeX-master nil)
  ;; Auctex在打开tex文件时加载，LaTeX-insert-left-brace会覆盖全局的括号定义。
  ;; 打开auctex自带的默认输入右括号的选项。
  (setq LaTeX-electric-left-right-brace t)
  (mapc (lambda (mode)
          (add-hook 'TeX-mode-hook mode))
        (list 'LaTeX-math-mode
              'turn-on-orgtbl
              'TeX-fold-mode ; C-c C-o C-b打开fold，C-c C-o b关闭fold。
              'LaTeX-install-toolbar))
  ;; 在LaTeX-mode中，默认开启PDF-mode，即默认使用xelatex直接生成pdf文件，而不用每次用'C-c C-t C-p'进行切换。设置'Tex-show-compilation'为t，在另一个窗口显示编译信息，对于错误的排除很方便。另外，编译时默认直接保存文件，绑定补全符号到TAB键。
  (add-hook 'LaTeX-mode-hook
            (lambda ()
              (setq TeX-auto-untabify t ; Remove all tabs before saving.
                    TeX-engine 'xetex   ; Use xelatex default.
                    TeX-show-compilation t) ; Display compilation windows.
              (TeX-global-PDF-mode t)   ; PDF mode enable, not plain.
              (setq TeX-save-query nil)
              (imenu-add-menubar-index)
              (define-key LaTeX-mode-map (kbd "C-c f") 'TeX-font)
              (define-key LaTeX-mode-map (kbd "C-c v") 'preview-at-point)
              (define-key LaTeX-mode-map (kbd "C-c V") 'preview-clearout-buffer)
              (define-key LaTeX-mode-map (kbd "C-q") ' (lambda () (interactive)
                                                         (ignore-errors (kill-process (TeX-active-process)))
                                                         (swint-kill-this-buffer)))
              (define-key LaTeX-mode-map (kbd "C-c r") 'reftex-mode)
              (define-key LaTeX-mode-map (kbd "C-c z") 'zotelo-minor-mode)
              (define-key LaTeX-mode-map (kbd "C-c m") 'helm-insert-latex-math)
              (define-key LaTeX-mode-map (kbd "C-c b") 'helm-bibtex-with-local-bibliography)))
  (setq TeX-view-program-list '(("Llpp" "llpp %o") ("Firefox" "firefox %o")))
  ;; 使用imagemagick中convert转换为图片。win中默认使用imgconvert，可以将cygwin中convert改名为imgconvert。
  (add-to-list 'TeX-command-list '("LaTeX-standalone" "%`xelatex -shell-escape%(mode)%' %t" TeX-run-TeX nil t))
  (when is-lin (setq TeX-view-program-selection '((output-pdf "Llpp") (output-dvi "Llpp"))))
  ;; ============setup-and-keybindings=============
;;;; reftex
  ;; ===================reftex=====================
  (use-package reftex
    :commands reftex-mode
    ;; C-c [ reftex-citation，C-c C-x [ org-reftex-citation。
    :config
    (define-key reftex-mode-map (kbd "C-c r") 'reftex-parse-all)
    (setq reftex-plug-into-AUCTeX t
          reftex-toc-split-windows-horizontally t
          reftex-toc-split-windows-fraction 0.2))
  ;; ===================reftex=====================
;;;; preview
  ;; ==================preview=====================
  (use-package preview
    :after tex
    :config
    (setq preview-auto-cache-preamble t)
    (setq preview-gs-options '("-q" "-dNOPAUSE" "-DNOPLATFONTS" "-dPrinted" "-dTextAlphaBits=4" "-dGraphicsAlphaBits=4"))
    (set-face-attribute 'preview-reference-face nil :background "white"))
  ;; ==================preview=====================
  )
;; =====================auctex=====================
;;; auctex-latexmk
;; =================auctex-latexmk=================
;; texlive默认包含latexmk，只需加入.latexmkrc配置文件。
(use-package auctex-latexmk
  :after tex
  :config
  (auctex-latexmk-setup))
;; =================auctex-latexmk=================
;;; zotelo
;; ====================zotelo======================
;; C-c z c建立bib文件，C-c z u更新bib文件。
(use-package zotelo
  :commands (zotelo-minor-mode swint-zotelo-update-database zotelo--locate-bibliography-files zotelo-set-collection)
  :config
  (define-key zotelo-minor-mode-map "\C-czU" 'swint-zotelo-update-database)
  ;; 设置.bib文件的编码格式，否则出现乱码。
  (setq zotelo-translator-charsets '((BibTeX . "Unicode") (Default . "Unicode")))
  ;; 手动更新~/.bib/zotelo/下文件。
  (defun swint-zotelo-update-database ()
    (interactive)
    (zotero-update-collection-hash)
    (maphash #'(lambda (key value)
                 (zotelo-update-database nil (concat "~/.bib/zotelo/" value) key))
             zotero-collection-hash))
  (defvar zotero-collection-hash nil)
  (defun zotero-update-collection-hash ()
    (let ((buf (get-buffer-create "*moz-command-output*"))
          colls name id)
      ;; Set up the collection list.
      (moz-command (format zotelo--render-collection-js
                           (process-get (zotelo--moz-process) 'moz-prompt)))
      (moz-command "zotelo_render_collection()" buf)
      (setq zotero-collection-hash (make-hash-table :test 'equal))
      (with-current-buffer buf
        (goto-char (point-min))
        (zotelo--message (format "Collections:\n %s"
                                 (buffer-substring-no-properties (point-min) (min 500 (point-max)))))
        (while (re-search-forward "^\\([0-9]+\\) /\\(.*\\)$" nil t)
          (setq id (match-string-no-properties 1)
                name (replace-regexp-in-string "/" "_" (match-string-no-properties 2) t t))
          (puthash id name zotero-collection-hash)))
      (puthash "0" "ALL" zotero-collection-hash))))
;; ====================zotelo======================
;;; latex-preview-pane
;; ==============latex-preview-pane================
(use-package latex-preview-pane
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
