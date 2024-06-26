;;; auctex
;; =====================auctex=====================
(use-package latex
  :commands LaTeX-math-mode
  :mode ("\\.[tT][eE][xX]\\'" . LaTeX-mode)
  :init
  (add-hook 'LaTeX-mode-hook
            (lambda ()
              (LaTeX-math-mode 1)
              (TeX-fold-mode 1) ;C-c C-o C-b打开fold，C-c C-o b关闭fold
              (TeX-PDF-mode 1)
              (turn-on-orgtbl)
              (setq-local TeX-base-mode-name "TeX")
              (define-key LaTeX-mode-map (kbd "C-c m") 'helm-insert-latex-math)
              (define-key LaTeX-mode-map (kbd "C-c l") #'(lambda () (interactive) (insert (swint-cursor-localtion))))))
  (setq LaTeX-math-abbrev-prefix "M-s `") ;LaTeX-math-mode与cdlatex的prefix冲突
  :config
  (setq TeX-auto-save t
        TeX-parse-self t
        TeX-auto-untabify t
        TeX-engine 'xetex
        TeX-save-query nil
        TeX-show-compilation t
        TeX-view-program-list '(("Llpp" "llpp %o") ("Firefox" "firefox %o"))
        TeX-source-correlate-mode t
        TeX-source-correlate-method 'synctex
        TeX-source-correlate-start-server nil
        TeX-electric-sub-and-superscript t
        LaTeX-electric-left-right-brace t)
  ;; 使用imagemagick中convert转换为图片。win中默认使用imgconvert，可以将cygwin中convert改名为imgconvert
  (add-to-list 'TeX-command-list '("LaTeX-standalone" "%`xelatex -shell-escape%(mode)%' %t" TeX-run-TeX nil t))
  ;; 使用beamer-preview多线程快速更新，需pip安装pypdf2/colorlog/watchdog
  ;; 指定--run 2是因为只编译一次时会出现临时页：Temporary page! LATEX was unable to guess the total number of pages
  ;; 在导出模板cn-beamer中禁用animate包之后不生成临时页
  (add-to-list 'TeX-command-list '("beamer-preview" "python ~/.emacs.d/repos/beamer-preview/beamer-preview.py --compiler xelatex --compiler-option=\"-interaction=nonstopmode\" --run 1 --watch %(t-filename-only)" TeX-run-command nil t))
  (setq TeX-view-program-selection '((output-pdf "PDF Tools") (output-dvi "Llpp") (output-html "Firefox"))))
;; =====================auctex=====================
;;; preview
;; ====================preview=====================
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
;; ====================preview=====================
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
