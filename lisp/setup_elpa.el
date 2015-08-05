;; (when
;;     (load
;;      (expand-file-name "~/.emacs.d/elpa/package.el"))
;;   (package-initialize))
;; 强制使用自己手动下载的低版本的package.el，在emacs23时使用。
;; (add-to-list 'load-path "~/.emacs.d/elpa")
(require 'package)
(setq package-archives '(("melpa" . "http://melpa.org/packages/")
                         ("gnu" . "http://elpa.gnu.org/packages/")
                         ("org" . "http://orgmode.org/elpa/")
                         ("elpa" . "http://tromey.com/elpa/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")))
(package-initialize)
(when (< emacs-major-version 24)
  ;; Help package.el work in older Emacsen, where there's no TRASH arg
  ;; for 'delete-directory
  (message "Warning: overriding delete-directory to support TRASH argument.")
  (fset 'smp--delete-directory (symbol-function 'delete-directory))
  (defun delete-directory (directory &optional recursive trash)
    "Overridden: see `smp--delete-directory' for the wrapped function"
    (smp--delete-directory directory recursive)))
;; 借自prelude。
(defvar prelude-packages
  '(ac-math
    ace-jump-buffer
    ace-jump-mode
    anchored-transpose
    anzu
    async
    auto-complete
    auto-complete-auctex
    bing-dict
    dash
    dired-details
    dirtree
    drag-stuff
    elisp-slime-nav
    elmacro
    emms
    epl
    expand-region
    f
    fcitx
    flx
    flx-ido
    flycheck
    git-commit-mode
    git-rebase-mode
    git-timemachine
    gnuplot-mode
    god-mode
    graphviz-dot-mode
    helm
    helm-bibtex
    helm-projectile
    helm-swoop
    helm-unicode
    highlight-symbol
    hungry-delete
    ido-at-point
    ido-hacks
    ido-vertical-mode
    imenu-anywhere
    lacarte
    let-alist
    magit
    math-symbol-lists
    multiple-cursors
    nyan-mode
    outline-magic
    paredit
    paredit-everywhere
    parsebib
    pinyin-search
    pkg-info
    popup
    popup-kill-ring
    popwin
    pos-tip
    projectile
    rainbow-delimiters
    readline-complete
    recentf-ext
    rich-minority
    s
    smart-mode-line
    smex
    smooth-scrolling
    switch-window
    tree-mode
    undo-tree
    visible-mark
    w3m
    windata
    window-numbering
    yasnippet
    zotelo)
  "A list of packages to ensure are installed at launch.")
(defun prelude-packages-installed-p ()
  "Check if all packages in `prelude-packages' are installed."
  (every #'package-installed-p prelude-packages))
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
(define-obsolete-function-alias 'prelude-ensure-module-deps 'prelude-require-packages)
(defun prelude-install-packages ()
  "Install all packages listed in `prelude-packages'."
  (unless (prelude-packages-installed-p)
    ;; check for new packages (package versions)
    (message "%s" "Emacs Prelude is now refreshing its package database...")
    (package-refresh-contents)
    (message "%s" " done.")
    ;; install the missing packages
    (prelude-require-packages prelude-packages)))
;; run package installation
(prelude-install-packages)
(provide 'setup_elpa)
