;; ===================OS====================
(setq is-mac (equal system-type 'darwin))
(setq is-win (equal system-type 'windows-nt))
(setq is-lin (equal system-type 'gnu/linux))
(setq is-T510 (file-exists-p "~/is-T510.org"))
(setq is-X201 (file-exists-p "~/is-X201.org"))
;; ===================PATH==================
(when is-win
  (setenv "HOME" "c:/Users/swint")
  ;; (setenv "PATH" "c:/Users/swint/")
  ;; 必须注销这行，因为会让latex失效。
  ;; set the default file path
  (setq default-directory "~/")
  ;; 使emacs可以使用win键
  (setq w32-lwindow-modifier 'super)
  (run-with-idle-timer 0.0 nil 'w32-send-sys-command 61488))
;; Set path to dependencies
(setq site-lisp-dir
      (expand-file-name "site-lisp" user-emacs-directory))
(setq lisp-dir
      (expand-file-name "lisp" user-emacs-directory))
(setq defuns-dir
      (expand-file-name "defuns" user-emacs-directory))
(setq user-settings-dir                 ;Settings for currently logged in user
      (concat user-emacs-directory "users/" user-login-name))
;; Set up load path
(add-to-list 'load-path site-lisp-dir)
(add-to-list 'load-path lisp-dir)
(add-to-list 'load-path defuns-dir)
(add-to-list 'load-path user-settings-dir)
;; Add external projects to load path
(dolist (project (directory-files site-lisp-dir t "\\w+"))
  (when (file-directory-p project)
    (add-to-list 'load-path project)))
;; Functions (load all files in defuns-dir)
(dolist (file (directory-files defuns-dir t "\\w+"))
  (when (file-regular-p file)
    (load file)))
;; =================CUSTOM=================
(cond
 (is-lin (setq custom-file (expand-file-name "custom-lin.el" user-emacs-directory)))
 (is-win (setq custom-file (expand-file-name "custom-win.el" user-emacs-directory))))
(load custom-file)
;; =================TIME===================
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
;;   (try-require 'setup_elpa t)
;;   (try-require 'setup_desktop_session t)
;;   (try-require 'setup_backup_autosave t)
;;   (try-require 'setup_default t)
;;   (try-require 'setup_abandoned t)
;;   (try-require 'setup_abbrev t)
;;   (try-require 'setup_avy t)
;;   (try-require 'setup_appearance t)
;;   (try-require 'setup_ccmode t)
;;   (try-require 'setup_fonts t)
;;   (try-require 'setup_dired t)
;;   (try-require 'setup_dirtree t)
;;   (try-require 'setup_emms_mingus t)
;;   (try-require 'setup_flycheck t)
;;   (try-require 'setup_flyspell t)
;;   (try-require 'setup_gnuplot t)
;;   (try-require 'setup_graphviz t)
;;   (try-require 'setup_ibuffer t)
;;   (try-require 'setup_ido t)
;;   (try-require 'setup_isearch t)
;;   (try-require 'setup_latex t)
;;   (try-require 'setup_magit t)
;;   (try-require 'setup_octave t)
;;   (try-require 'setup_mew t)
;;   (try-require 'setup_minibuffer t)
;;   (try-require 'setup_mode_line t)
;;   (try-require 'setup_org_mode t)
;;   (try-require 'setup_helm t)
;;   (try-require 'setup_projectile t)
;;   (try-require 'setup_paredit t)
;;   (try-require 'setup_parenthesis t)
;;   (try-require 'setup_perspective t)
;;   (try-require 'setup_recentf t)
;;   (try-require 'setup_yasnippet t)
;;   (try-require 'setup_dict t)
;;   (try-require 'setup_w3m t)
;;   (try-require 'setup_wicd t)
;;   (try-require 'setup_windows t)
;;   (try-require 'setup_completion t)
;;   (try-require 'setup_eshell t)
;;   (try-require 'setup_packages t)
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
;; =================SETUP==================
;; cl-lib(introduced in 24.3) provides cl-xxx while cl provides xxx, some packages still use xxx.
;; Use cl-xxx function directly or load cl library which aliases xxx to cl-xxx.
(require 'cl)
(require 'setup_elpa)
(require 'setup_desktop_session)
(require 'setup_backup_autosave)
(require 'setup_default)
(require 'setup_abandoned)
(require 'setup_abbrev)
(require 'setup_avy)
(require 'setup_appearance)
(require 'setup_ccmode)
(require 'setup_fonts)
(require 'setup_dired)
(require 'setup_dirtree)
(require 'setup_emms_mingus)
(require 'setup_flycheck)
(require 'setup_flyspell)
(require 'setup_gnuplot)
(require 'setup_graphviz)
(require 'setup_ibuffer)
(require 'setup_ido)
(require 'setup_isearch)
(require 'setup_latex)
(require 'setup_magit)
(require 'setup_octave)
(require 'setup_mew)
(require 'setup_minibuffer)
(require 'setup_mode_line)
(require 'setup_org_mode)
(require 'setup_helm)
(require 'setup_projectile)
(require 'setup_paredit)
(require 'setup_parenthesis)
(require 'setup_perspective)
(require 'setup_recentf)
(require 'setup_yasnippet)
(require 'setup_dict)
(require 'setup_w3m)
(require 'setup_wicd)
(require 'setup_windows)
(require 'setup_completion)
(require 'setup_eshell)
(require 'setup_packages)
;; =================END====================
