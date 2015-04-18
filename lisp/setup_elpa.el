;; (when
;;     (load
;;      (expand-file-name "~/.emacs.d/elpa/package.el"))
;;   (package-initialize))
;; 强制使用自己手动下载的低版本的package.el，在emacs23时使用。
;; (add-to-list 'load-path "~/.emacs.d/elpa")
(require 'package)
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("melpa-old" . "http://melpa.milkbox.net/packages/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(package-initialize)
(when (< emacs-major-version 24)
  ;; Help package.el work in older Emacsen, where there's no TRASH arg
  ;; for 'delete-directory
  (message "Warning: overriding delete-directory to support TRASH argument.")
  (fset 'smp--delete-directory (symbol-function 'delete-directory))
  (defun delete-directory (directory &optional recursive trash)
    "Overridden: see `smp--delete-directory' for the wrapped function"
    (smp--delete-directory directory recursive)))
;; (defun init--install-packages ()
;;   (packages-install
;;    '(magit
;;      paredit
;;      move-text
;;      gist
;;      htmlize
;;      visual-regexp
;;      flycheck
;;      flx
;;      flx-ido
;;      css-eldoc
;;      yasnippet
;;      smartparens
;;      ido-vertical-mode
;;      ido-at-point
;;      simple-httpd
;;      guide-key
;;      nodejs-repl
;;      restclient
;;      highlight-escape-sequences
;;      whitespace-cleanup-mode
;;      elisp-slime-nav
;;      git-commit-mode
;;      gitconfig-mode
;;      gitignore-mode
;;      clojure-mode
;;      groovy-mode
;;      prodigy
;;      cider
;;      cider-tracing)))
;; (condition-case nil
;;     (init--install-packages)
;;   (error
;;    (package-refresh-contents)
;;    (init--install-packages)))
(provide 'setup_elpa)
