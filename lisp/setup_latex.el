;;; auctex
;; =====================auctex=====================
(use-package tex
  :commands LaTeX-math-mode
  :mode ("\\.[tT][eE][xX]\\'" . LaTeX-mode)
  :config
;;;; setup-and-keybindings
  ;; ============setup-and-keybindings=============
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq-default TeX-master nil)
  (setq LaTeX-electric-left-right-brace t)
  (setq LaTeX-math-abbrev-prefix "M-s `") ;LaTeX-math-mode与cdlatex的prefix冲突
  (mapc (lambda (mode)
          (add-hook 'LaTeX-mode-hook mode))
        (list 'LaTeX-math-mode
              'turn-on-orgtbl
              'TeX-fold-mode ; C-c C-o C-b打开fold，C-c C-o b关闭fold
              'LaTeX-install-toolbar))
  (add-hook 'LaTeX-mode-hook
            (lambda ()
              (setq TeX-auto-untabify t ; Remove all tabs before saving.
                    TeX-engine 'xetex   ; Use xelatex default.
                    TeX-show-compilation t) ; Display compilation windows.
              (TeX-global-PDF-mode t)   ; PDF mode enable, not plain.
              (setq TeX-save-query nil)
              (setq-local TeX-base-mode-name "TeX")
              (imenu-add-menubar-index)
              (define-key LaTeX-mode-map (kbd "C-q") #'(lambda () (interactive)
                                                         (ignore-errors (kill-process (TeX-active-process)))
                                                         (swint-kill-buffer)))
              (define-key LaTeX-mode-map (kbd "C-c m") 'helm-insert-latex-math)
              (define-key LaTeX-mode-map (kbd "C-c l") #'(lambda () (interactive)
                                                           (insert (swint-cursor-localtion))))))
  (setq TeX-view-program-list '(("Llpp" "llpp %o") ("Firefox" "firefox %o")))
  ;; 使用imagemagick中convert转换为图片。win中默认使用imgconvert，可以将cygwin中convert改名为imgconvert
  (add-to-list 'TeX-command-list '("LaTeX-standalone" "%`xelatex -shell-escape%(mode)%' %t" TeX-run-TeX nil t))
  ;; 使用beamer-preview多线程快速更新，需pip安装pypdf2/colorlog/watchdog
  ;; 指定--run 2是因为只编译一次时会出现临时页：Temporary page! LATEX was unable to guess the total number of pages
  ;; 在导出模板cn-beamer中禁用animate包之后不生成临时页
  (add-to-list 'TeX-command-list '("beamer-preview" "python ~/.emacs.d/repos/beamer-preview/beamer-preview.py --compiler xelatex --compiler-option=\"-interaction=nonstopmode\" --run 1 --watch %(t-filename-only)" TeX-run-command nil t))
  (setq TeX-view-program-selection '((output-pdf "Llpp") (output-dvi "Llpp")))
  ;; ============setup-and-keybindings=============
;;;; preview
  ;; ==================preview=====================
  (use-package preview
    :commands (preview-at-point
               preview-clearout-buffer)
    :init
    (add-hook 'LaTeX-mode-hook (lambda ()
                                 (bind-key "C-c v" 'preview-at-point LaTeX-mode-map)
                                 (bind-key "C-c V" 'preview-clearout-buffer LaTeX-mode-map)))
    :config
    (setq preview-auto-cache-preamble t)
    (set-face-attribute 'preview-reference-face nil :background "grey" :height 1.5))
  ;; ==================preview=====================
  )
;; =====================auctex=====================
;;; auctex-latexmk
;; =================auctex-latexmk=================
;; texlive默认包含latexmk，只需加入.latexmkrc配置文件
(use-package auctex-latexmk
  :after tex
  :config
  (auctex-latexmk-setup)
  (advice-add 'TeX-engine-set :after #'(lambda (type) (auctex-latexmk-setup))))
;; =================auctex-latexmk=================
;;; magic-latex-buffer
;; ==============magic-latex-buffer================
(use-package magic-latex-buffer
  :diminish magic-latex-buffer
  :commands magic-latex-buffer
  :init
  (add-hook 'LaTeX-mode-hook 'magic-latex-buffer)
  :config
  (setq magic-latex-enable-block-align nil))
;; ==============magic-latex-buffer================
(provide 'setup_latex)
