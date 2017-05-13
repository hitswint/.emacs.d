;;; elpa
;; ====================elpa=========================
(require 'package)
(unless package--initialized
  ;; 使用http://elpa.emacs-china.org镜像源。
  (setq package-archives '(("melpa" . "http://elpa.emacs-china.org/melpa/")
                           ("gnu" . "http://elpa.emacs-china.org/gnu/")
                           ("org" . "http://elpa.emacs-china.org/org/")
                           ("marmalade" . "http://elpa.emacs-china.org/marmalade/")))
  ;; 使用官方源。
  ;; (setq package-archives '(("melpa" . "http://melpa.org/packages/")
  ;;                          ("gnu" . "http://elpa.gnu.org/packages/")
  ;;                          ("org" . "http://orgmode.org/elpa/")
  ;;                          ("elpa" . "http://tromey.com/elpa/")
  ;;                          ("marmalade" . "http://marmalade-repo.org/packages/")))
  ;; Optimization, no need to activate all the packages so early.
  (setq package-enable-at-startup nil)
  ;; 激活所有packages，也可以使用package-activate单独激活。
  (package-initialize)
  ;; win中出现Failed to download `gnu' archive错误。
  (when is-win
    (setq package-check-signature nil)))
;; 借自prelude。
(defvar prelude-packages
  (append '(ace-jump-mode
            ace-link
            ace-pinyin
            ace-popup-menu
            ac-html-bootstrap
            ac-ispell
            ac-math
            aggressive-indent
            anchored-transpose
            anzu
            arduino-mode
            async
            auctex
            auctex-latexmk
            auto-complete
            auto-complete-auctex
            auto-complete-c-headers
            auto-complete-clang
            auto-highlight-symbol
            avy
            avy-menu
            avy-zap
            backup-walker
            bbyac
            bind-key
            bing-dict
            bm
            buttercup
            cdlatex
            char-menu
            chinese-pyim
            chinese-pyim-basedict
            chinese-pyim-greatdict
            clean-aindent-mode
            clipmon
            company
            company-c-headers
            company-quickhelp
            company-try-hard
            company-web
            counsel
            dash
            diff-hl
            diminish
            dired-details
            dired-narrow
            dired-ranger
            diredful
            disaster
            drag-stuff
            dumb-jump
            firefox-controller
            function-args
            easy-kill
            ein
            elisp-slime-nav
            elmacro
            elpy
            emmet-mode
            emms
            epl
            eshell-prompt-extras
            evil-nerd-commenter
            expand-region
            f
            find-file-in-project
            flx
            flx-ido
            flycheck
            git-commit
            git-timemachine
            gnuplot-mode
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
            helm-projectile
            helm-swoop
            helm-unicode
            highlight-indentation
            highlight-parentheses
            highlight-symbol
            hungry-delete
            hydra
            ido-at-point
            ido-hacks
            ido-ubiquitous
            ido-vertical-mode
            imenu-anywhere
            interleave
            ivy
            ivy-hydra
            js2-mode
            lacarte
            latex-preview-pane
            let-alist
            magic-latex-buffer
            magit
            magit-popup
            markdown-mode
            math-symbol-lists
            matlab-mode
            mew
            multifiles
            multiple-cursors
            names
            navi-mode
            neotree
            nyan-mode
            operate-on-number
            outline-magic
            outorg
            outshine
            paredit
            paredit-everywhere
            parsebib
            peep-dired
            perspective
            persp-projectile
            pinyin-search
            pkg-info
            popup
            popup-kill-ring
            popwin
            pos-tip
            projectile
            pyvenv
            quickrun
            rainbow-delimiters
            rainbow-mode
            readline-complete
            recentf-ext
            rich-minority
            s
            session
            skewer-mode
            smart-mode-line
            smartrep
            smex
            swiper
            tangotango-theme
            undo-tree
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
            yasnippet
            zotelo
            ztree)
          (cond
           (is-lin '(fcitx
                     pdf-tools
                     tablist
                     ycmd
                     company-ycmd
                     flycheck-ycmd))
           (is-win '(w32-browser
                     mingus))))
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
;; ====================elpa=========================
;;; USE-PACKAGE
;; =================USE-PACKAGE=====================
;; :bind或:commands中需使用package或:config中的函数。
(eval-when-compile
  (require 'use-package))
(require 'diminish)
(require 'bind-key)
;; =================USE-PACKAGE=====================
;;; smartrep
;; ===================smartrep======================
(use-package smartrep
  ;; Enabled automatically.
  :config
  (setq smartrep-mode-line-string-activated nil))
;; ===================smartrep======================
;; =====================misc========================
(require 'subr-x)
;; =====================misc========================
(provide 'setup_elpa)
