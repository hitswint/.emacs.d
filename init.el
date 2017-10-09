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
(when is-win
  ;; Set the default file path.
  (setq default-directory "~/")
  ;; 使emacs可以使用win键。
  (setq w32-lwindow-modifier 'super)
  (run-with-idle-timer 0.0 nil 'w32-send-sys-command 61488))
;; Set path to dependencies.
(setq site-lisp-dir (expand-file-name "site-lisp" user-emacs-directory))
(setq lisp-dir (expand-file-name "lisp" user-emacs-directory))
(setq defuns-dir (expand-file-name "defuns" user-emacs-directory))
(setq user-settings-dir (concat user-emacs-directory "users/" user-login-name))
;; Set up load path.
(add-to-list 'load-path site-lisp-dir)
(add-to-list 'load-path lisp-dir)
(add-to-list 'load-path defuns-dir)
(add-to-list 'load-path user-settings-dir)
;; Add external projects to load path.
(dolist (project (directory-files site-lisp-dir t "\\w+"))
  (when (file-directory-p project) (add-to-list 'load-path project)))
;; Functions (load all files in defuns-dir).
(dolist (file (directory-files defuns-dir t "\\w+"))
  (when (file-regular-p file) (load file)))
;; ==================PATH==================
;;; TIME
;; ==================TIME===================
;; ;; Function to collect information of packages.
;; (defvar missing-packages-list nil
;;   "List of packages that `try-require' can't find.")
;; (defvar package-init-statistic nil "Package loading statistics")
;; ;; attempt to load a feature/library, failing silently
;; (defun try-require (feature &optional click)
;;   "Attempt to load a library or module. Return true if the
;; library given as argument is successfully loaded. If not, instead
;; of an error, just add the package to a list of missing packages."
;;   (condition-case err
;;       ;; protected form
;;       (let ((timestamp (current-time))
;;             (package (if (stringp feature) feature (symbol-name feature))))
;;         (if (stringp feature)
;;             (load-library feature)
;;           (require feature))
;;         (if click
;;             (add-to-list 'package-init-statistic
;;                          (cons (if (stringp feature) feature (symbol-name feature))
;;                                (float-time (time-since timestamp)))))
;;         (message "Checking for library `%s'... Found, cost %.2f seconds"
;;                  feature (float-time (time-since timestamp))))
;;     ;; error handler
;;     (file-error  ; condition
;;      (progn
;;        (message "Checking for library `%s'... Missing" feature)
;;        (add-to-list 'missing-packages-list feature 'append))
;;      nil)))
;; ;; Load all configuration and packages.
;; (let ((ts-init (current-time)))
;;   (setq missing-packages-list nil
;;         package-init-statistic nil)
;;   (try-require 'x t)
;;   (try-require 'x t)
;;   (try-require 'x t)
;;   ;; Report package statistics.
;;   (message "\n\nShowing package initialization statistics:\n%s"
;;            (mapconcat (lambda (x)
;;                         (format "package %s cost %.2f seconds" (car x) (cdr x)))
;;                       (reverse package-init-statistic)
;;                       "\n"
;;                       ))
;;   (message "Finished startup in %.2f seconds,  %d packages missing%s\n\n"
;;            (float-time (time-since ts-init)) (length missing-packages-list)
;;            (if missing-packages-list
;;                ". Refer to `missing-packages-list` for missing packages."
;;              ".")))
;; ==================TIME===================
;;; SETUP
;; =================SETUP==================
;; cl-lib(introduced in 24.3) provides cl-xxx while cl provides xxx, some packages still use xxx.
;; Use cl-xxx function directly or load cl library which aliases xxx to cl-xxx.
(require 'cl)
(require 'setup_default)
(require 'setup_elpa)
(require 'setup_desktop_session)
(require 'setup_abandoned)
(require 'setup_avy)
(require 'setup_appearance)
(require 'setup_ccmode)
(require 'setup_chinese)
(require 'setup_dired)
(require 'setup_backup_autosave)
(require 'setup_email)
(require 'setup_emms_mingus)
(require 'setup_flycheck)
(require 'setup_flyspell)
(require 'setup_ibuffer)
(require 'setup_isearch)
(require 'setup_ivy)
(require 'setup_latex)
(require 'setup_magit)
(require 'setup_octave)
(require 'setup_minibuffer)
(require 'setup_mode_line)
(require 'setup_org_mode)
(require 'setup_helm)
(require 'setup_projectile)
(require 'setup_paredit)
(require 'setup_parenthesis)
(require 'setup_perspective)
(require 'setup_python)
(require 'setup_dict)
(require 'setup_w3m)
(require 'setup_web)
(require 'setup_wicd)
(require 'setup_windows)
(require 'setup_completion)
(require 'setup_shell)
(require 'setup_packages)
;; =================SETUP==================
;;; CUSTOM
;; =================CUSTOM=================
(cond
 (is-lin (setq custom-file (expand-file-name "custom-lin.el" user-emacs-directory)))
 (is-win (setq custom-file (expand-file-name "custom-win.el" user-emacs-directory))))
(load custom-file)
;; =================CUSTOM=================
