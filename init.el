;; -*- lexical-binding: t; -*-
;;; OS
;; ==================OS====================
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
;; (package-initialize)
(setq is-mac (equal system-type 'darwin))
(setq is-win (equal system-type 'windows-nt))
(setq is-lin (equal system-type 'gnu/linux))
;; ==================OS====================
;;; PATH
;; ==================PATH==================
(defvar site-lisp-dir (expand-file-name "site-lisp" user-emacs-directory))
(defvar lisp-dir (expand-file-name "lisp" user-emacs-directory))
(defvar perspective-dir (expand-file-name "perspective-20191127.1849" site-lisp-dir))
(defvar doom-autoload-file (expand-file-name "autoloads.el" lisp-dir))
(add-to-list 'load-path lisp-dir)
(setq load-path (append load-path (directory-files site-lisp-dir t "^[^.]" t)))
;; ==================PATH==================
;;; SETUP
;; =================SETUP==================
(eval-and-compile
  (defvar doom--file-name-handler-alist file-name-handler-alist)
  (unless (or after-init-time noninteractive)
    (setq gc-cons-threshold most-positive-fixnum
          gc-cons-percentage 0.6
          file-name-handler-alist nil))
  ;; cl-lib(introduced in 24.3) provides cl-xxx while cl provides xxx, some packages still use xxx.
  ;; Use cl-xxx function directly or load cl library which aliases xxx to cl-xxx.
  (require 'cl)
  (require 'setup_default)
  (require 'setup_elpa)
  (require 'setup_desktop)
  (require 'setup_avy)
  (require 'setup_appearance)
  (require 'setup_bibtex)
  (require 'setup_browser)
  (require 'setup_ccmode)
  (require 'setup_chinese)
  (require 'setup_dired)
  (require 'setup_backup)
  (require 'setup_eaf)
  (require 'setup_email)
  (require 'setup_emms)
  (require 'setup_flycheck)
  (require 'setup_flyspell)
  (require 'setup_helm)
  (require 'setup_ibuffer)
  (require 'setup_isearch)
  (require 'setup_ivy)
  (require 'setup_latex)
  (require 'setup_lsp)
  (require 'setup_magit)
  (require 'setup_matlab)
  (require 'setup_org)
  (require 'setup_pdf)
  (require 'setup_perspective)
  (require 'setup_projectile)
  (require 'setup_parenthesis)
  (require 'setup_python)
  (require 'setup_dict)
  (require 'setup_web)
  (require 'setup_windows)
  (require 'setup_completion)
  (require 'setup_shell)
  (require 'setup_packages)
  (condition-case-unless-debug ex
      (if (file-exists-p doom-autoload-file)
          (require 'autoloads doom-autoload-file t)
        (dolist (file (directory-files (expand-file-name "autoload" lisp-dir) t "\\w+"))
          (when (file-regular-p file) (load file))))
    ('error
     (lwarn 'autoloads :warning
            "%s in autoloads.el -> %s"
            (car ex) (error-message-string ex))))
  (add-hook 'emacs-startup-hook
            (lambda ()
              (setq gc-cons-threshold 33554432
                    gc-cons-percentage 0.1
                    file-name-handler-alist doom--file-name-handler-alist))))
;; =================SETUP==================
;;; CUSTOM
;; =================CUSTOM=================
(setq custom-file (expand-file-name "custom-lin.el" user-emacs-directory))
(load custom-file t t)
;; =================CUSTOM=================
