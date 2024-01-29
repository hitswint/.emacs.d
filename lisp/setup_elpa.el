;;; package
;; ====================package======================
(require 'package)
;;;; 官方源
;; (setq package-archives '(("melpa" . "https://melpa.org/packages/")
;;                          ;; ("melpa-stable" . "https://stable.melpa.org/packages/")
;;                          ("gnu" . "https://elpa.gnu.org/packages/")
;;                          ("cselpa" . "https://elpa.thecybershadow.net/packages/") ;term-keys
;;                          ;; ("org" . "https://orgmode.org/elpa/") ;已失效
;;                          ;; ("elpa" . "http://tromey.com/elpa/") ;版本过老
;;                          ;; ("marmalade" . "http://marmalade-repo.org/packages/")
;;                          ))
;;;; emacs-china
(setq package-archives '(("melpa" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
                         ("gnu" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
                         ("cselpa" . "https://elpa.thecybershadow.net/packages/")))
;; Optimization, no need to activate all the packages so early.
(setq package-enable-at-startup nil
      package--init-file-ensured t
      byte-compile-warnings '(not free-vars unresolved noruntime lexical make-local))
;; 激活所有packages，也可以使用package-activate单独激活
(package-initialize)
(defvar prelude-packages '(ac-html-bootstrap
                           ac-ispell
                           ac-math
                           academic-phrases
                           ace-jump-helm-line
                           ace-link
                           ace-pinyin
                           aggressive-indent
                           all-the-icons
                           all-the-icons-dired
                           anzu
                           async
                           auctex
                           auctex-latexmk
                           auto-complete
                           auto-complete-auctex
                           auto-complete-c-headers
                           auto-complete-clang
                           auto-highlight-symbol
                           auto-yasnippet
                           avy
                           avy-zap
                           backup-walker
                           baidu-translate
                           benchmark-init
                           bibtex-completion
                           bing-dict
                           bm
                           buttercup
                           cdlatex
                           clean-aindent-mode
                           clipmon
                           company
                           company-c-headers
                           company-quickhelp
                           company-try-hard
                           company-web
                           counsel
                           csv-mode
                           dash
                           delight
                           diff-hl
                           diminish
                           dired-du
                           dired-filetype-face
                           dired-narrow
                           dired-ranger
                           dired-subtree
                           disaster
                           drag-stuff
                           dumb-jump
                           easy-kill
                           ebib
                           elisp-slime-nav
                           elmacro
                           elpy
                           emmet-mode
                           emms
                           epl
                           eshell-prompt-extras
                           evil-nerd-commenter
                           exec-path-from-shell
                           expand-region
                           f
                           fcitx
                           find-file-in-project
                           flycheck
                           arduino-mode
                           flycheck-pos-tip
                           function-args
                           gcmh
                           git-commit
                           git-timemachine
                           gnuplot-mode
                           gnu-elpa-keyring-update
                           god-mode
                           google-translate
                           goto-chg
                           graphviz-dot-mode
                           helm
                           helm-ag
                           helm-bibtex
                           helm-bm
                           helm-core
                           helm-descbinds
                           helm-firefox
                           helm-flycheck
                           helm-flyspell
                           helm-gtags
                           helm-mu
                           helm-pass
                           helm-projectile
                           helm-swoop
                           helm-unicode
                           highlight-indentation
                           highlight-parentheses
                           highlight-symbol
                           hungry-delete
                           hydra
                           imenu-anywhere
                           interleave
                           ivy
                           ivy-hydra
                           jedi
                           js2-mode
                           jupyter
                           key-chord
                           lacarte
                           let-alist
                           lingva
                           magic-latex-buffer
                           magit
                           markdown-mode
                           math-symbol-lists
                           matlab-mode
                           meghanada
                           mu4e-alert
                           mu4e-views
                           multifiles
                           multiple-cursors
                           names
                           neotree
                           operate-on-number
                           org-appear
                           org-brain
                           org-noter
                           djvu
                           nov
                           org-pdftools
                           org-ref
                           outline-magic
                           outorg
                           outshine
                           ox-pandoc
                           paredit
                           paredit-everywhere
                           parsebib
                           pass
                           pdfgrep
                           pdf-tools
                           peep-dired
                           pinyin-search
                           pinyinlib
                           pkg-info
                           popup
                           popup-kill-ring
                           pos-tip
                           posframe
                           projectile
                           pyim
                           pyim-basedict
                           pyvenv
                           quickrun
                           rainbow-delimiters
                           rainbow-mode
                           readline-complete
                           recentf-ext
                           restclient
                           rg
                           s
                           skewer-mode
                           smartrep
                           sudo-edit
                           swiper
                           tangotango-theme
                           term-keys
                           unicode-escape
                           vimish-fold
                           visible-mark
                           visual-regexp
                           vlf
                           vundo
                           volatile-highlights
                           w3m
                           web-mode
                           which-key
                           window-numbering
                           with-editor
                           wrap-region
                           yaml-mode
                           yasnippet
                           yasnippet-snippets
                           youdao-dictionary
                           ztree)
  "A list of packages to ensure are installed at launch.")
(defun prelude-packages-installed-p ()
  "Check if all packages in `prelude-packages' are installed."
  (cl-every #'package-installed-p prelude-packages))
(defun prelude-require-package (package)
  "Install PACKAGE unless already installed."
  (unless (memq package prelude-packages)
    (add-to-list 'prelude-packages package))
  (unless (package-installed-p package)
    (package-install package)))
(defun prelude-require-packages (packages)
  "Ensure PACKAGES are installed.
Missing packages are installed automatically."
  (mapc #'prelude-require-package packages))
(defun prelude-install-packages ()
  "Install all packages listed in `prelude-packages'."
  (unless (prelude-packages-installed-p)
    ;; Check for new packages (package versions).
    (message "%s" "Emacs Prelude is now refreshing its package database...")
    (package-refresh-contents)
    (message "%s" " done.")
    ;; Install the missing packages.
    (prelude-require-packages prelude-packages)))
;; Run package installation.
(prelude-install-packages)
;; ====================package======================
;;; benchmark-init
;; ================benchmark-init===================
;; (require 'benchmark-init)
;; (benchmark-init/show-durations-tree)
;; (benchmark-init/show-durations-tabulated)
;; ================benchmark-init===================
;;; use-package
;; =================use-package=====================
;; :bind或:commands中需使用package或:config中的函数
(require 'use-package)
(require 'delight)
(require 'bind-key)
(require 'diminish)
(delight 'fundamental-mode "Fund")
;; =================use-package=====================
;;; exec-path-from-shell
;; =============exec-path-from-shell================
(use-package exec-path-from-shell
  :config
  (setq exec-path-from-shell-variables '("PATH" "MANPATH" "XAPIAN_CJK_NGRAM"))
  (setq exec-path-from-shell-arguments '("-l"))
  (exec-path-from-shell-initialize))
;; =============exec-path-from-shell================
;;; smartrep
;; ===================smartrep======================
(use-package smartrep
  :config
  (setq smartrep-mode-line-string-activated nil
        smartrep-mode-line-active-bg "black")
  (smartrep-define-key global-map "<escape>"
    '(("i" . tab-to-tab-stop)
      ("u" . upcase-word)
      ("l" . downcase-word)
      ("c" . capitalize-word)
      ("q" . fill-paragraph)
      ("h" . mark-paragraph)
      ("k" . kill-sentence)))
  (smartrep-define-key global-map "M-g"
    '(("d" . duplicate-dwim))))
;; ===================smartrep======================
;;; gcmh
;; =====================gcmh========================
(use-package gcmh
  :diminish gcmh-mode
  :init
  (setq gcmh-high-cons-threshold 33554432)
  :config
  (gcmh-mode 1))
;; =====================gcmh========================
;;; misc
;; =====================misc========================
(require 'subr-x)
(require 'bookmark)
;; =====================misc========================
(provide 'setup_elpa)
