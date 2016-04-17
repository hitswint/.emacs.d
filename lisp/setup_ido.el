;;; ido
;; =========================ido========================
(use-package ido
  ;; Enabled automatically.
  :config
  (ido-mode t)
  ;; (global-set-key (kbd "C-x f") 'ido-find-file)
;;;; ido-back-to-home
  ;; =================ido-back-to-home=================
  (add-hook 'ido-setup-hook
            (lambda ()
              ;; Go straight home
              (define-key ido-file-completion-map
                (kbd "/")
                (lambda ()
                  (interactive)
                  (if (looking-back "/")
                      (insert "~/")
                    (call-interactively 'self-insert-command))))))
  ;; =================ido-back-to-home=================
;;;; ido-find-file:open-file-with-external-app
  ;; ====ido-find-file:open-file-with-external-app=====
  (defvar ido-fallback-function nil "The fallback function that will be explicitly check and can be externally modified
this variable is introduced to enhance ido-find-file functionality
search (cond ...  ((eq ido-exit 'fallback) ... )) to see where it's used.
2013-08-04 Sunday 19:35:45 by Scinart")
  (defun ido-file-internal (method &optional fallback default prompt item initial switch-cmd)
    ;; Internal function for ido-find-file and friends
    (unless item
      (setq item 'file))
    (let ((ido-current-directory (ido-expand-directory default))
          (ido-context-switch-command switch-cmd)
          ido-directory-nonreadable ido-directory-too-big
          filename)
      (if (or (not ido-mode) (ido-is-slow-ftp-host))
          (setq filename t
                ido-exit 'fallback)
        (setq ido-directory-nonreadable
              (ido-nonreadable-directory-p ido-current-directory)
              ido-directory-too-big
              (and (not ido-directory-nonreadable)
                   (ido-directory-too-big-p ido-current-directory))))
      (when (and (eq item 'file)
                 (or ido-use-url-at-point ido-use-filename-at-point))
        (let (fn d)
          (require 'ffap)
          ;; Duplicate code from ffap-guesser as we want different
          ;; behavior for files and URLs.
          (cond
           ((with-no-warnings
              (and ido-use-url-at-point
                   ffap-url-regexp
                   (ffap-fixup-url (or (ffap-url-at-point)
                                       (ffap-gopher-at-point)))))
            (setq ido-exit 'ffap
                  filename t))

           ((and ido-use-filename-at-point
                 (setq fn (with-no-warnings
                            (if (eq ido-use-filename-at-point 'guess)
                                (ffap-guesser)
                              (ffap-string-at-point))))
                 (not (string-match "^http:/" fn))
                 (let ((absolute-fn (expand-file-name fn)))
                   (setq d (if (file-directory-p absolute-fn)
                               (file-name-as-directory absolute-fn)
                             (file-name-directory absolute-fn))))
                 (file-directory-p d))
            (setq ido-current-directory d)
            (setq initial (file-name-nondirectory fn))))))
      (let (ido-saved-vc-hb
            (vc-handled-backends (and (boundp 'vc-handled-backends) vc-handled-backends))
            (ido-work-directory-index -1)
            (ido-work-file-index -1)
            (ido-find-literal nil))
        (unless filename
          (setq ido-saved-vc-hb vc-handled-backends)
          (let ((minibuffer-completing-file-name t))
            (setq filename (ido-read-internal item
                                              (or prompt "Find file: ")
                                              'ido-file-history
                                              (and (eq method 'alt-file) buffer-file-name)
                                              (confirm-nonexistent-file-or-buffer) initial))))
        ;; Choose the file name: either the text typed in, or the head
        ;; of the list of matches
        (cond
         ((eq ido-exit 'fallback)
          ;; Need to guard setting of default-directory here, since
          ;; we don't want to change directory of current buffer.
          (let ((default-directory ido-current-directory)
                (read-file-name-function nil))
            (setq this-command (or fallback ido-fallback-function 'find-file))
            (run-hook-with-args 'ido-before-fallback-functions this-command)
            (call-interactively this-command)
            (setq ido-fallback-function nil) ))
         ((eq ido-exit 'switch-to-buffer)
          (ido-buffer-internal
           (if (memq method '(other-window other-frame)) method ido-default-buffer-method)
           nil nil nil ido-text))
         ((eq ido-exit 'insert-buffer)
          (ido-buffer-internal 'insert 'insert-buffer "Insert buffer: " nil ido-text 'ido-enter-insert-file))
         ((eq ido-exit 'dired)
          (dired (concat ido-current-directory (or ido-text ""))))
         ((eq ido-exit 'ffap)
          (find-file-at-point))
         ((eq method 'alt-file)
          (ido-record-work-file filename)
          (setq default-directory ido-current-directory)
          (ido-record-work-directory)
          (find-alternate-file filename))
         ((memq method '(dired list-directory))
          (if (equal filename ".")
              (setq filename ""))
          (let* ((dirname (ido-final-slash (concat ido-current-directory filename) t))
                 (file (substring dirname 0 -1)))
            (cond
             ((file-directory-p dirname)
              (ido-record-command method dirname)
              (ido-record-work-directory dirname)
              (funcall method dirname))
             ((file-directory-p ido-current-directory)
              (cond
               ((file-exists-p file)
                (ido-record-command method ido-current-directory)
                (ido-record-work-directory)
                (funcall method ido-current-directory)
                (if (eq method 'dired)
                    (with-no-warnings
                      (dired-goto-file (expand-file-name file)))))
               ((string-match "[[*?]" filename)
                (setq dirname (concat ido-current-directory filename))
                (ido-record-command method dirname)
                (ido-record-work-directory)
                (funcall method dirname))
               ((y-or-n-p (format "Directory %s does not exist.  Create it? " filename))
                (ido-record-command method dirname)
                (ido-record-work-directory dirname)
                (make-directory-internal dirname)
                (funcall method dirname))
               (t
                ;; put make-directory command on history
                (ido-record-command 'make-directory dirname))))
             (t (error "No such directory")))))
         ((eq method 'write)
          (ido-record-work-file filename)
          (setq default-directory ido-current-directory)
          (setq filename (concat ido-current-directory filename))
          (ido-record-command 'write-file filename)
          (add-to-history 'file-name-history filename)
          (ido-record-work-directory)
          (write-file filename t))
         ((eq method 'read-only)
          (ido-record-work-file filename)
          (setq filename (concat ido-current-directory filename))
          (ido-record-command fallback filename)
          (ido-record-work-directory)
          (run-hook-with-args 'ido-before-fallback-functions fallback)
          (funcall fallback filename))
         ((eq method 'insert)
          (ido-record-work-file filename)
          (setq filename (concat ido-current-directory filename))
          (ido-record-command
           (if ido-find-literal 'insert-file-literally 'insert-file)
           filename)
          (add-to-history 'file-name-history filename)
          (ido-record-work-directory)
          (insert-file-1 filename
                         (if ido-find-literal
                             #'insert-file-contents-literally
                           #'insert-file-contents)))
         (filename
          (ido-record-work-file filename)
          (setq filename (concat ido-current-directory filename))
          (ido-record-command 'find-file filename)
          (add-to-history 'file-name-history filename)
          (ido-record-work-directory)
          (ido-visit-buffer (find-file-noselect filename nil ido-find-literal) method))))))
  (add-hook 'ido-minibuffer-setup-hook 'ido-my-keys)
  (cond
   (is-lin
    (defun ido-my-keys ()
      "My Keybindings for ido
especially for extending ido-find-file functionality
2013-08-04 Sunday 17:25:03"
      (define-key ido-completion-map (kbd "RET") 'ido-exit-minibuffer) ;; for find-file
      (define-key ido-completion-map (kbd "C-j") 'ido-magic-open-using-external-app))
    (defun ido-magic-open-using-external-app ()
      "This should be used when ido-minibuffer is active"
      (interactive)
      ;;   (let ((i (length ido-text)))
      ;;     (while (> i 0)
      ;;       (push (aref ido-text (setq i (1- i))) unread-command-events)))
      (setq ido-exit 'fallback)
      (setq ido-fallback-function
            '(lambda () (interactive)
               (let ( (dir ido-current-directory)
                      (file (car ido-matches) ))
                 (async-shell-command-no-output-buffer-from-file (concat dir file)))))
      (exit-minibuffer)))
   (is-win
    (defun ido-my-keys ()
      "My Keybindings for ido
especially for extending ido-find-file functionality
2013-08-04 Sunday 17:25:03"
      (define-key ido-completion-map (kbd "RET") 'ido-exit-minibuffer) ;; for find-file
      (define-key ido-completion-map (kbd "C-j") 'ido-magic-open-using-w32))
    (defun ido-magic-open-using-w32 ()
      "This should be used when ido-minibuffer is active"
      (interactive)
      ;;   (let ((i (length ido-text)))
      ;;     (while (> i 0)
      ;;       (push (aref ido-text (setq i (1- i))) unread-command-events)))
      (setq ido-exit 'fallback)
      (setq ido-fallback-function
            '(lambda () (interactive)
               (let ( (dir ido-current-directory)
                      (file (car ido-matches) ))
                 (w32-shell-execute 1 (concat dir file)))))
      (exit-minibuffer))))
  ;; ====ido-find-file:open-file-with-external-app=====
  )
;; =========================ido========================
;;; ido-tips
;; ======================ido-tips======================
(use-package ido-ubiquitous
  ;; Enabled after features.
  :defer t
  :after ido
  :config
  ;; (setq ido-everywhere t)
  (ido-ubiquitous-mode 1))
(use-package ido-vertical-mode
  ;; Enabled at idle.
  :defer 2
  :config
  (ido-vertical-mode 1)
  (setq ido-vertical-define-keys 'C-n-C-p-up-down-left-right))
(use-package ido-hacks
  ;; Enabled at idle.
  :defer 2)
(use-package flx-ido
  ;; Enabled at idle.
  :defer 2
  :config
  (setq ido-enable-flex-matching t)
  (flx-ido-mode 1))
(use-package ido-at-point
  ;; Enabled at idle.
  :defer 2
  :config
  (ido-at-point-mode))
;; ======================ido-tips======================
(provide 'setup_ido)
