;;; autoload/byte-compile
;; =================autoload/byte-compile=================
(defun doom-initialize-autoloads ()
  "Ensures that `doom-autoload-file' exists and is loaded. Otherwise run
`doom/reload-autoloads' to generate it."
  (unless (file-exists-p doom-autoload-file)
    (doom//reload-autoloads)))
(defun doom-packages--read-if-cookies (file)
  "Returns the value of the ;;;###if predicate form in FILE."
  (with-temp-buffer
    (insert-file-contents-literally file nil 0 256)
    (if (and (re-search-forward "^;;;###if " nil t)
             (<= (line-number-at-pos) 3))
        (let ((load-file-name file))
          (eval (sexp-at-point)))
      t)))
(defun doom-packages--async-run (fn &optional arg)
  (let ((default-directory (file-truename user-emacs-directory))
        (compilation-filter-hook
         (list (lambda () (ansi-color-apply-on-region compilation-filter-start (point))))))
    (compile (format "%s --quick --batch -l init.el -f %s %s"
                     (executable-find "emacs")
                     (symbol-name fn)
                     arg))
    (while compilation-in-progress
      (sit-for 1))))
;;;###autoload
(defun doom//reload-autoloads ()
  "Refreshes the autoloads.el file, specified by `doom-autoload-file'."
  (interactive)
  (if (not noninteractive)
      (and (doom-packages--async-run 'doom//reload-autoloads)
           (load doom-autoload-file))
    ;; (doom-initialize-packages t)
    (let ((targets
           (file-expand-wildcards
            (expand-file-name "autoload/*.el" lisp-dir))))
      (when (file-exists-p doom-autoload-file)
        (delete-file doom-autoload-file)
        (message "Deleted old autoloads.el"))
      (dolist (file (reverse targets))
        (message
         (cond ((not (doom-packages--read-if-cookies file))
                "⚠ Ignoring %s")
               ((update-file-autoloads file nil doom-autoload-file)
                "✕ Nothing in %s")
               (t
                "✓ Scanned %s"))
         (file-relative-name file (file-truename user-emacs-directory))))
      ;; (make-directory (file-name-directory doom-autoload-file) t)
      (let ((buf (get-file-buffer doom-autoload-file))
            current-sexp)
        (unwind-protect
            (condition-case-unless-debug ex
                (with-current-buffer buf
                  (save-buffer)
                  (goto-char (point-min))
                  (while (re-search-forward "^(" nil t)
                    (save-excursion
                      (backward-char)
                      (setq current-sexp (read (thing-at-point 'sexp t)))
                      (eval current-sexp t))
                    (forward-char))
                  (message "Finished generating autoloads.el!"))
              ('error
               (delete-file doom-autoload-file)
               (error "Error in autoloads.el: (%s %s ...) %s -- %s"
                      (nth 0 current-sexp)
                      (nth 1 current-sexp)
                      (car ex) (error-message-string ex))))
          (swint-kill-buffer buf))))))
;;;###autoload
(defun doom//byte-compile (&optional recompile-p)
  "Byte compiles your emacs configuration."
  (interactive (list current-prefix-arg))
  (let ((default-directory (file-truename user-emacs-directory))
        (recompile-p (or recompile-p
                         (and (member "-r" (cdr argv)) t))))
    (if (not noninteractive)
        (doom-packages--async-run 'doom//byte-compile (and recompile-p "-- -r"))
      ;; (doom-initialize-packages t)
      (let ((total-ok   0)
            (total-fail 0)
            (total-noop 0)
            compile-targets)
        (setq compile-targets
              (cl-loop for target
                       in (list lisp-dir perspective-dir) ;; site-lisp-dir
                       if (file-directory-p target)
                       nconc (nreverse (directory-files-recursively target "\\.el$"))
                       else if (file-exists-p target)
                       collect target))
        (unless compile-targets
          (error "No targets to compile"))
        (push (expand-file-name "init.el" (file-truename user-emacs-directory)) compile-targets)
        (condition-case ex
            (progn
              (dolist (target compile-targets)
                (when (or recompile-p
                          (let ((elc-file (byte-compile-dest-file target)))
                            (and (file-exists-p elc-file)
                                 (file-newer-than-file-p target elc-file))))
                  (let ((result (if (doom-packages--read-if-cookies target)
                                    (byte-compile-file target)
                                  'no-byte-compile))
                        (short-name (file-relative-name target (file-truename user-emacs-directory))))
                    (cl-incf
                     (cond ((eq result 'no-byte-compile)
                            (message "⚠ Ignored %s" short-name)
                            total-noop)
                           ((null result)
                            (message "✕ Failed to compile %s" short-name)
                            total-fail)
                           (t
                            (message "✓ Compiled %s" short-name)
                            (load target t t)
                            total-ok))))))
              (message "%s %s file(s) %s"
                       (if recompile-p "Recompiled" "Compiled")
                       (format "%d/%d" total-ok (- (length compile-targets) total-noop))
                       (format "(%s ignored)" total-noop)))
          (error
           (message "\n%s\n\n%s\n\n%s"
                    "There were breaking errors."
                    (error-message-string ex)
                    "Reverting changes...")
           (doom//clean-byte-compiled-files)
           (message "Finished (nothing was byte-compiled)")))))))
;;;###autoload
(defun doom//clean-byte-compiled-files ()
  "Delete all the compiled elc files in your Emacs configuration."
  (interactive)
  (let ((targets (append (list (expand-file-name "init.elc" (file-truename user-emacs-directory)))
                         (directory-files-recursively perspective-dir "\\.elc$")
                         (directory-files-recursively lisp-dir "\\.elc$")
                         ;; (directory-files-recursively site-lisp-dir "\\.elc$")
                         ))
        (default-directory (file-truename user-emacs-directory)))
    (unless (cl-loop for path in targets
                     if (file-exists-p path)
                     collect path
                     and do (delete-file path)
                     and do (message "✓ Deleted %s" (file-relative-name path)))
      (message "Everything is clean"))))
;; =================autoload/byte-compile=================
;;; package-initialize
;; ==================package-initialize===================
;; Without package.el initialization, you mainly lose two things:
;; 1. Emacs won’t load your packages’ autoloads files; 2. Your load-path will be empty.
;; 下面函数计算load-path，使用use-package代替autoloads，但某些包仍然存在问题
;; 若有未定义函数，使用(autoload 'xxx-fun "org")或(load "xxx-autoloads" nil t)
;; 使用locate-library返回require加载的文件路径
;; (defvar doom-init-p nil)
;; (defvar doom--refreshed-p nil)
;; (defvar doom--site-load-path load-path)
;; (defvar doom--base-load-path (append (list lisp-dir) doom--site-load-path))
;; (defun doom-initialize (&optional force-p)
;;   "Initialize installed packages (using package.el)."
;;   (when (or force-p (not doom-init-p))
;;     (let ((load-path doom--base-load-path))
;;       (setq package-activated-list nil)
;;       (condition-case _ (package-initialize t)
;;         ('error (package-refresh-contents)
;;                 (setq doom--refreshed-p t)
;;                 (package-initialize t)))
;;       (let ((packages-required (cl-remove-if #'package-installed-p prelude-packages)))
;;         (when packages-required
;;           (message "Installing packages")
;;           (unless doom--refreshed-p
;;             (package-refresh-contents))
;;           (dolist (package packages-required)
;;             (let ((inhibit-message t))
;;               (package-install package))
;;             (if (package-installed-p package)
;;                 (message "✓ Installed %s" package)
;;               (error "✕ Couldn't install %s" package)))
;;           (message "Installing core packages...done")))
;;       (setq doom-init-p t))))
;; (defun doom-initialize-load-path ()
;;   (setq load-path (append doom--base-load-path
;;                           (directory-files site-lisp-dir t "^[^.]" t)
;;                           (directory-files package-user-dir t "^[^.]" t))))
;; (defun doom-initialize-packages (&optional force-p)
;;   "Crawls across your emacs.d."
;;   (doom-initialize-load-path)
;;   (with-temp-buffer
;;     (cl-flet
;;         ((_load
;;           (file &optional noerror interactive)
;;           (condition-case-unless-debug ex
;;               (let ((load-prefer-newer t)
;;                     (noninteractive (not interactive)))
;;                 (load file noerror :nomessage :nosuffix))
;;             ('error
;;              (lwarn 'doom :warning
;;                     "%s in %s: %s"
;;                     (car ex)
;;                     (file-relative-name file (file-truename user-emacs-directory))
;;                     (error-message-string ex))))))
;;       (_load (expand-file-name "init.el" (file-truename user-emacs-directory)))
;;       (mapc #'_load (file-expand-wildcards (expand-file-name "autoload/*.el" lisp-dir))))))
;; (setq load-path (eval-when-compile (doom-initialize t)
;;                                    (doom-initialize-load-path)))
;; ==================package-initialize===================
