;;; package
;; ====================package======================
(require 'package)
;;;; 源
;;;;; 官方源
;; (setq package-archives '(("melpa" . "https://melpa.org/packages/")
;;                          ("melpa-stable" . "https://stable.melpa.org/packages/")
;;                          ("gnu" . "https://elpa.gnu.org/packages/")
;;                          ("org" . "https://orgmode.org/elpa/")
;;                          ;; ("elpa" . "http://tromey.com/elpa/") ;版本过老
;;                          ;; ("marmalade" . "http://marmalade-repo.org/packages/")
;;                          ))
;;;;; emacs-china
;; (setq package-archives '(("melpa" . "http://elpa.emacs-china.org/melpa/")
;;                          ("melpa-stable" . "http://elpa.emacs-china.org/stable-melpa/")
;;                          ("gnu" . "http://elpa.emacs-china.org/gnu/")
;;                          ("org" . "http://elpa.emacs-china.org/org/")
;;                          ("marmalade" . "http://elpa.emacs-china.org/marmalade/")))
(setq package-archives '(("melpa" . "http://elpa.zilongshanren.com/melpa/")
                         ("melpa-stable" . "http://elpa.zilongshanren.com/stable-melpa/")
                         ("gnu" . "http://elpa.zilongshanren.com/gnu/")
                         ("org" . "http://elpa.zilongshanren.com/org/")))
;;;;; https://github.com/d12frosted/elpa-mirror
;; (setq package-archives '(("melpa" . "https://gitlab.com/d12frosted/elpa-mirror/raw/master/melpa/")
;;                          ("melpa-stable" . "https://gitlab.com/d12frosted/elpa-mirror/raw/master/stable-melpa/")
;;                          ("gnu" . "https://gitlab.com/d12frosted/elpa-mirror/raw/master/gnu/")
;;                          ("org" . "https://gitlab.com/d12frosted/elpa-mirror/raw/master/org/")))
;;;;; 163
;; (setq package-archives '(("gnu" . "http://mirrors.163.com/elpa/gnu/")
;;                          ("melpa" . "http://mirrors.163.com/elpa/melpa/")
;;                          ("melpa-stable" . "http://mirrors.163.com/elpa/melpa-stable/")
;;                          ("org" . "http://mirrors.163.com/elpa/org/")
;;                          ("marmalade" . "http://mirrors.163.com/elpa/marmalade/")))
;;;;; tsinghua
;; (setq package-archives '(("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
;;                          ("melpa-stable" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa-stable/")
;;                          ("gnu" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
;;                          ("org" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/org/")
;;                          ("marmalade" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/marmalade/")))
;;;;; ustc
;; (setq package-archives '(("melpa" . "http://mirrors.ustc.edu.cn/elpa/melpa/")
;;                          ("melpa-stable" . "http://mirrors.ustc.edu.cn/elpa/melpa-stable/")
;;                          ("gnu" . "http://mirrors.ustc.edu.cn/elpa/gnu/")
;;                          ("org" . "http://mirrors.ustc.edu.cn/elpa/org/")
;;                          ("marmalade" . "http://mirrors.ustc.edu.cn/elpa/marmalade/")))
;;;; ELPA for term-keys
(add-to-list 'package-archives '("cselpa" . "https://elpa.thecybershadow.net/packages/") t)
;; Optimization, no need to activate all the packages so early.
(setq package-enable-at-startup nil
      package--init-file-ensured t
      byte-compile-warnings '(not free-vars unresolved noruntime lexical make-local))
;; 激活所有packages，也可以使用package-activate单独激活。
(package-initialize)
;; 借自prelude。
(defvar prelude-packages '(ac-html-bootstrap
                           ac-ispell
                           ac-math
                           academic-phrases
                           ace-jump-helm-line
                           ace-link
                           ace-pinyin
                           aggressive-indent
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
                           unicode-escape
                           baidu-translate
                           bbyac
                           benchmark-init
                           bind-key
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
                           company-ycmd
                           counsel
                           dash
                           diff-hl
                           diminish
                           dired-details
                           dired-du
                           dired-filetype-face
                           dired-narrow
                           dired-ranger
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
                           flycheck-ycmd
                           function-args
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
                           latex-preview-pane
                           let-alist
                           magic-latex-buffer
                           magit
                           markdown-mode
                           math-symbol-lists
                           matlab-mode
                           meghanada
                           mu4e-alert
                           multifiles
                           multiple-cursors
                           names
                           navi-mode
                           neotree
                           nyan-mode
                           operate-on-number
                           org
                           org-appear
                           org-brain
                           org-noter
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
                           rich-minority
                           s
                           skewer-mode
                           smart-mode-line
                           smartrep
                           smex
                           sudo-edit
                           swiper
                           tangotango-theme
                           term-keys
                           undo-fu
                           use-package
                           vimish-fold
                           visible-mark
                           visual-regexp
                           vlf
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
                           ycmd
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
;;; use-package
;; =================use-package=====================
;; :bind或:commands中需使用package或:config中的函数。
(eval-when-compile
  (require 'use-package))
(require 'diminish)
(require 'bind-key)
(defmacro def-package! (name &rest plist)
  (unless (and (bound-and-true-p byte-compile-current-file)
               (or (and (plist-member plist :if)     (not (eval (plist-get plist :if))))
                   (and (plist-member plist :when)   (not (eval (plist-get plist :when))))
                   (and (plist-member plist :unless) (eval (plist-get plist :unless)))))
    `(use-package ,name ,@plist)))
;; =================use-package=====================
;;; benchmark-init
;; ================benchmark-init===================
(def-package! benchmark-init
  ;; :disabled
  :config
  ;; benchmark-init/show-durations-tree / benchmark-init/show-durations-tabulated
  (add-hook 'after-init-hook 'benchmark-init/deactivate))
;; ================benchmark-init===================
;;; exec-path-from-shell
;; =============exec-path-from-shell================
(def-package! exec-path-from-shell
  :config
  (setq exec-path-from-shell-variables '("PATH" "MANPATH" "XAPIAN_CJK_NGRAM"))
  (setq exec-path-from-shell-arguments '("-l"))
  (exec-path-from-shell-initialize))
;; =============exec-path-from-shell================
;;; smartrep
;; ===================smartrep======================
(def-package! smartrep
  :config
  (setq smartrep-mode-line-string-activated nil)
  (smartrep-define-key global-map "<escape>"
    '(("i" . tab-to-tab-stop)
      ("u" . upcase-word)
      ("l" . downcase-word)
      ("c" . capitalize-word)
      ("q" . fill-paragraph)
      ("h" . mark-paragraph)
      ("k" . kill-sentence))))
;; ===================smartrep======================
;; =====================misc========================
(require 'subr-x)
(require 'bookmark)
;; =====================misc========================
(provide 'setup_elpa)
