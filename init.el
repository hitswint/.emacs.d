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
(defvar doom-autoload-file (expand-file-name "autoloads.el" lisp-dir))
(add-to-list 'load-path lisp-dir)
(setq load-path (append load-path (directory-files site-lisp-dir t "^[^.]" t)))
;; ==================PATH==================
;;; SETUP
;; =================SETUP==================
(eval-and-compile
  (defvar doom--file-name-handler-alist file-name-handler-alist)
  (unless (or after-init-time noninteractive)
    (setq gc-cons-threshold 402653184
          gc-cons-percentage 0.6
          file-name-handler-alist nil))
  ;; cl-lib(introduced in 24.3) provides cl-xxx while cl provides xxx, some packages still use xxx.
  ;; Use cl-xxx function directly or load cl library which aliases xxx to cl-xxx.
  (require 'cl)
  (require 'setup_default)
  (require 'setup_elpa)
  (require 'setup_desktop_session)
  (require 'setup_perspective)
  (require 'setup_abandoned)
  (require 'setup_avy)
  (require 'setup_appearance)
  (require 'setup_ccmode)
  (require 'setup_chinese)
  (require 'setup_dired)
  (require 'setup_backup_autosave)
  (require 'setup_email)
  (require 'setup_emms)
  (require 'setup_flycheck)
  (require 'setup_flyspell)
  (require 'setup_ibuffer)
  (require 'setup_isearch)
  (require 'setup_ivy)
  (require 'setup_latex)
  (require 'setup_magit)
  (require 'setup_octave)
  (require 'setup_mode_line)
  (require 'setup_org_mode)
  (require 'setup_helm)
  (require 'setup_projectile)
  (require 'setup_paredit)
  (require 'setup_parenthesis)
  (require 'setup_python)
  (require 'setup_dict)
  (require 'setup_w3m)
  (require 'setup_web)
  (require 'setup_wicd)
  (require 'setup_windows)
  (require 'setup_completion)
  (require 'setup_shell)
  (require 'setup_packages)
  (condition-case-unless-debug ex
      (require 'autoloads doom-autoload-file t)
    ('error
     (lwarn 'autoloads :warning
            "%s in autoloads.el -> %s"
            (car ex) (error-message-string ex))))
  (add-hook 'emacs-startup-hook
            (lambda ()
              (setq gc-cons-threshold 16777216
                    gc-cons-percentage 0.1
                    file-name-handler-alist doom--file-name-handler-alist))))
;; =================SETUP==================
;;; CUSTOM
;; =================CUSTOM=================
(setq custom-file (expand-file-name "custom-lin.el" user-emacs-directory))
(load custom-file t t)
;; =================CUSTOM=================
