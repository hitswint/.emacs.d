;;; helm
;; ====================helm=====================
(def-package! helm
  :diminish helm-mode
  :commands (helm-find-files-1
             helm-insert-latex-math)
  :bind-keymap ("C-x c" . helm-command-map)
  :bind (("C-M-y" . helm-show-kill-ring)
         ("C-'" . helm-bookmarks)
         ("C-," . swint-helm-file-buffers-list)
         ("C-." . swint-helm-dired-buffers-list)
         ("C-x C-f" . helm-find-files)
         ("C-x f" . helm-find)
         ("M-x" . helm-M-x)
         ("C-x F" . swint-helm-locate)
         ("C-x y" . helm-resume))
  :config
  (def-package! helm-config)
  (def-package! helm-for-files)
  (helm-mode 1)
  (setq helm-completing-read-handlers-alist '((describe-function . helm-completing-read-symbols)
                                              (describe-variable . helm-completing-read-symbols)
                                              (debug-on-entry . helm-completing-read-symbols)
                                              (find-function . helm-completing-read-symbols)
                                              (find-tag . helm-completing-read-with-cands-in-buffer)
                                              (ffap-alternate-file)
                                              (tmm-menubar)
                                              (find-file)
                                              (org-annotate-file)
                                              (swint-org-annotate-file)
                                              (dired-do-copy)
                                              (dired-create-directory)))
  (setq helm-projectile-sources-list '(helm-source-projectile-projects
                                       helm-source-projectile-files-list
                                       helm-source-projectile-buffers-list))
  (setq helm-buffer-details-flag nil)
  (setq helm-ff-newfile-prompt-p nil)
  (setq helm-split-window-default-side 'same)
  (setq helm-kill-ring-threshold 1)
  (setq helm-external-programs-associations file-extension-app-alist)
  (setq helm-pdfgrep-default-read-command "llpp -page %p \"%f\"")
  (setq helm-boring-buffer-regexp-list (append helm-boring-buffer-regexp-list '("\\`Enjoy\\ Music\\'" "\\`\\*Inferior\\ Octave\\*\\'" "\\`\\*Ibuffer\\*\\'" "\\`\\*MATLAB\\*\\'" "\\`\\*shell\\*\\'" "\\`\\*calculator\\*\\'" "\\`\\*Calendar\\*\\'" "\\`\\*Process\\ List\\*\\'" "\\`\\*toc\\*\\'" "\\`\\*buffer-selection\\*\\'" "\\`\\*Disabled\\ Command\\*\\'" "\\`\\*Mingus\\*\\'" "\\`\\*Ido\\ Completions\\*\\'" "\\`.english-words\\'" "\\`\\*Help\\*\\'")))
  (custom-set-faces '(helm-buffer-directory ((t (:foreground "yellow" :weight bold))))
                    '(helm-buffer-file ((t (:inherit font-lock-type-face))))
                    '(helm-ff-directory ((t (:foreground "yellow" :weight bold))))
                    '(helm-ff-dotted-directory ((t (:foreground "yellow" :weight bold))))
                    '(helm-ff-file ((t (:foreground "white"))))
                    '(helm-grep-file ((t (:foreground "cyan"))))
                    '(helm-selection ((t (:background "black" :underline t))))
                    '(helm-visible-mark ((t (:foreground "DeepSkyBlue1")))))
;;;; keybindings
  ;; ===============keybindings=================
  (define-key helm-map (kbd "C-;") 'helm-toggle-visible-mark)
  (define-key helm-map (kbd "C-l") 'helm-execute-persistent-action)
  (define-key helm-map (kbd "C-M-p") 'helm-previous-source)
  (define-key helm-map (kbd "C-M-n") 'helm-next-source)
  (define-key helm-map (kbd "M-U") 'helm-unmark-all)
  (define-key helm-map (kbd "M-T") 'helm-toggle-all-marks)
  (define-key helm-map (kbd "C-,") 'swint-helm-file-buffers-after-quit)
  (define-key helm-map (kbd "C-.") 'swint-helm-dired-buffers-after-quit)
  (define-key helm-map (kbd "C-'") 'swint-helm-bookmarks-after-quit)
  (define-key helm-map (kbd "M-'") 'swint-helm-projectile-after-quit)
  (define-key helm-map (kbd "M-RET") 'helm-quit-and-find-file)
  (define-key helm-map (kbd "<C-tab>") 'helm-resume-previous-session-after-quit)
  (define-key helm-map (kbd "C-x y") 'helm-resume-list-buffers-after-quit)
  (helm-define-key-with-subkeys helm-map (kbd "C-x Y") ?Y 'helm-run-cycle-resume)
  (helm-define-key-with-subkeys global-map (kbd "C-x Y") ?Y 'helm-cycle-resume)
  (define-key helm-find-files-map (kbd "C-l") 'helm-execute-persistent-action)
  (define-key helm-find-files-map (kbd "C-h") 'helm-find-files-up-one-level)
  (define-key helm-find-files-map (kbd "M-U") 'helm-unmark-all)
  (define-key helm-find-files-map (kbd "M-T") 'helm-toggle-all-marks)
  (define-key helm-find-files-map (kbd "C-o") 'helm-ff-run-switch-other-window)
  (define-key helm-find-files-map (kbd "C-M-j") 'helm-ff-run-open-file-with-lister)
  (define-key helm-find-files-map (kbd "C-j") 'helm-ff-run-open-file-externally)
  (define-key helm-generic-files-map (kbd "C-o") 'helm-ff-run-switch-other-window)
  (define-key helm-generic-files-map (kbd "C-M-j") 'helm-ff-run-open-file-with-lister)
  (define-key helm-generic-files-map (kbd "C-j") 'helm-ff-run-open-file-externally)
  (define-key helm-buffer-map (kbd "C-M-j") 'swint-helm-buffer-persp-add-buffers)
  (define-key helm-buffer-map (kbd "C-M-k") 'swint-helm-buffer-persp-remove-buffers)
  (define-key helm-buffer-map (kbd "C-o") 'swint-helm-buffer-switch-persp/other-window)
  (define-key helm-read-file-map (kbd "C-l") 'helm-execute-persistent-action)
  (define-key helm-read-file-map (kbd "C-h") 'helm-find-files-up-one-level)
  (define-key helm-grep-map (kbd "C-o") 'helm-grep-run-other-window-action)
  (define-key helm-command-map (kbd "u") 'helm-unicode)
  ;; ===============keybindings=================
;;;; helm-pinyin
  ;; ================helm-pinyin================
  (require 'iswitchb-pinyin)
  ;; 支持中文拼音首字母匹配，会使helm-find-files匹配过多。
  (cl-defun helm-mm-3-match/around (orig-fn str &rest args)
    (apply orig-fn (concat str "|" (str-unicode-to-pinyin-initial str)) args))
  (advice-add 'helm-mm-3-match :around #'helm-mm-3-match/around)
  ;; 默认在输入前面加空格解决匹配问题。
  (defun helm-find-files-1/around (orig-fn fname &rest args)
    (apply orig-fn (concat fname " ") args))
  (advice-add 'helm-find-files-1 :around #'helm-find-files-1/around)
  ;; ================helm-pinyin================
;;;; helm-file-buffer
  ;; ============helm-file-buffer===============
  (defmacro swint-buffer-dired/persp-boolean (is-dired is-persp-curr)
    (let ((cl-remove-op (if (null is-dired) 'cl-remove-if 'cl-remove-if-not))
          (dired-op (if (null is-dired) 'or 'and))
          (persp-curr-op (if (eq is-dired is-persp-curr) 'or 'not))
          (type (if (null is-dired) "file" "dired")))
      `(or (,cl-remove-op (lambda (x) (,dired-op (equal (buffer-mode x) 'dired-mode)
                                                 (,persp-curr-op (and (bound-and-true-p persp-mode)
                                                                      (member x (remq nil (mapcar 'buffer-name (persp-buffers (persp-curr)))))))))
                          (helm-buffer-list))
           (list (buffer-name (get-buffer-create (format "*helm %s buffers-swint*" ,type)))))))
  (defun swint-helm-file-buffers-list--init/curr-persp ()
    ;; Issue #51 Create the list before `helm-buffer' creation.
    (setq swint-helm-file-buffers-list-cache/curr-persp
          (swint-buffer-dired/persp-boolean nil 1))
    (let ((result (cl-loop for b in swint-helm-file-buffers-list-cache/curr-persp
                           maximize (length b) into len-buf
                           maximize (length (with-current-buffer b
                                              (symbol-name major-mode)))
                           into len-mode
                           finally return (cons len-buf len-mode))))
      (unless (default-value 'helm-buffer-max-length)
        (helm-set-local-variable 'helm-buffer-max-length (car result)))
      (unless (default-value 'helm-buffer-max-len-mode)
        (helm-set-local-variable 'helm-buffer-max-len-mode (cdr result)))))
  (defun swint-helm-file-buffers-list--init/other-persps ()
    ;; Issue #51 Create the list before `helm-buffer' creation.
    (setq swint-helm-file-buffers-list-cache/other-persps
          (swint-buffer-dired/persp-boolean nil nil))
    (let ((result (cl-loop for b in swint-helm-file-buffers-list-cache/other-persps
                           maximize (length b) into len-buf
                           maximize (length (with-current-buffer b
                                              (symbol-name major-mode)))
                           into len-mode
                           finally return (cons len-buf len-mode))))
      (unless (default-value 'helm-buffer-max-length)
        (helm-set-local-variable 'helm-buffer-max-length (car result)))
      (unless (default-value 'helm-buffer-max-len-mode)
        (helm-set-local-variable 'helm-buffer-max-len-mode (cdr result)))))
  (defclass swint-helm-file-buffers-source/curr-persp (helm-source-sync helm-type-buffer)
    ((buffer-list
      :initarg :buffer-list
      :initform #'helm-buffer-list
      :custom function
      :documentation
      "  A function with no arguments to create buffer list.")
     (init :initform 'swint-helm-file-buffers-list--init/curr-persp)
     (candidates :initform swint-helm-file-buffers-list-cache/curr-persp)
     (matchplugin :initform nil)
     (match :initform 'helm-buffers-match-function)
     (persistent-action :initform 'helm-buffers-list-persistent-action)
     (resume :initform (lambda ()
                         (run-with-idle-timer
                          0.1 nil (lambda ()
                                    (with-helm-buffer
                                      (helm-force-update))))))
     (keymap :initform helm-buffer-map)
     (volatile :initform t)
     (help-message :initform 'helm-buffer-help-message)
     (persistent-help
      :initform
      "Show this buffer / C-u \\[helm-execute-persistent-action]: Kill this buffer")))
  (defclass swint-helm-file-buffers-source/other-persps (helm-source-sync helm-type-buffer)
    ((buffer-list
      :initarg :buffer-list
      :initform #'helm-buffer-list
      :custom function
      :documentation
      "  A function with no arguments to create buffer list.")
     (init :initform 'swint-helm-file-buffers-list--init/other-persps)
     (candidates :initform swint-helm-file-buffers-list-cache/other-persps)
     (matchplugin :initform nil)
     (match :initform 'helm-buffers-match-function)
     (persistent-action :initform 'helm-buffers-list-persistent-action)
     (resume :initform (lambda ()
                         (run-with-idle-timer
                          0.1 nil (lambda ()
                                    (with-helm-buffer
                                      (helm-force-update))))))
     (keymap :initform helm-buffer-map)
     (volatile :initform t)
     (help-message :initform 'helm-buffer-help-message)
     (persistent-help
      :initform
      "Show this buffer / C-u \\[helm-execute-persistent-action]: Kill this buffer")))
  (defclass swint-helm-recentf-file-source (helm-source-sync)
    ((init :initform (lambda () (recentf-mode 1)))
     (candidates :initform (lambda () (cl-remove-if (lambda (x)
                                                      (or (file-directory-p x)
                                                          (member x (mapcar 'buffer-file-name (buffer-list)))))
                                                    recentf-list)))
     (pattern-transformer :initform 'helm-recentf-pattern-transformer)
     (match-part :initform (lambda (candidate)
                             (if (or helm-ff-transformer-show-only-basename
                                     helm-recentf--basename-flag)
                                 (helm-basename candidate) candidate)))
     (filter-one-by-one :initform (lambda (c)
                                    (if (and helm-ff-transformer-show-only-basename
                                             (not (consp c)))
                                        (cons (helm-basename c) c)
                                      c)))
     (keymap :initform helm-generic-files-map)
     (help-message :initform helm-generic-file-help-message)
     (action :initform (helm-actions-from-type-file))))
  (defvar swint-helm-file-buffers-source-list/curr-persp nil)
  (defvar swint-helm-file-buffers-source-list/other-persps nil)
  (defvar swint-helm-source-recentf-file nil)
  (defun swint-helm-file-buffers-list ()
    "Preconfigured `helm' lightweight file buffers list."
    (interactive)
    (unless swint-helm-file-buffers-source-list/curr-persp
      (setq swint-helm-file-buffers-source-list/curr-persp
            (helm-make-source "File Buffers in current persp" 'swint-helm-file-buffers-source/curr-persp)))
    (unless swint-helm-file-buffers-source-list/other-persps
      (setq swint-helm-file-buffers-source-list/other-persps
            (helm-make-source "File Buffers in other persps" 'swint-helm-file-buffers-source/other-persps))
      (helm-add-action-to-source "Switch to persp/buffer" 'helm-switch-persp/buffer swint-helm-file-buffers-source-list/other-persps 0))
    (unless swint-helm-source-recentf-file
      (setq swint-helm-source-recentf-file
            (helm-make-source "Recentf File" 'swint-helm-recentf-file-source)))
    (let ((helm-ff-transformer-show-only-basename nil))
      (helm :sources '(swint-helm-file-buffers-source-list/curr-persp
                       swint-helm-file-buffers-source-list/other-persps
                       swint-helm-source-recentf-file
                       helm-source-buffer-not-found)
            :buffer "*helm file buffers-swint*"
            :keymap helm-buffer-map
            :truncate-lines t)))
  ;; ============helm-file-buffer===============
;;;; helm-dired-buffer
  ;; ============helm-dired-buffer==============
  (defun swint-helm-dired-buffers-list--init/curr-persp ()
    ;; Issue #51 Create the list before `helm-buffer' creation.
    (setq swint-helm-dired-buffers-list-cache/curr-persp
          (swint-buffer-dired/persp-boolean 1 1))
    (let ((result (cl-loop for b in swint-helm-dired-buffers-list-cache/curr-persp
                           maximize (length b) into len-buf
                           maximize (length (with-current-buffer b
                                              (symbol-name major-mode)))
                           into len-mode
                           finally return (cons len-buf len-mode))))
      (unless (default-value 'helm-buffer-max-length)
        (helm-set-local-variable 'helm-buffer-max-length (car result)))
      (unless (default-value 'helm-buffer-max-len-mode)
        (helm-set-local-variable 'helm-buffer-max-len-mode (cdr result)))))
  (defun swint-helm-dired-buffers-list--init/other-persps ()
    ;; Issue #51 Create the list before `helm-buffer' creation.
    (setq swint-helm-dired-buffers-list-cache/other-persps
          (swint-buffer-dired/persp-boolean 1 nil))
    (let ((result (cl-loop for b in swint-helm-dired-buffers-list-cache/other-persps
                           maximize (length b) into len-buf
                           maximize (length (with-current-buffer b
                                              (symbol-name major-mode)))
                           into len-mode
                           finally return (cons len-buf len-mode))))
      (unless (default-value 'helm-buffer-max-length)
        (helm-set-local-variable 'helm-buffer-max-length (car result)))
      (unless (default-value 'helm-buffer-max-len-mode)
        (helm-set-local-variable 'helm-buffer-max-len-mode (cdr result)))))
  (defclass swint-helm-dired-buffers-source/curr-persp (helm-source-sync helm-type-buffer)
    ((buffer-list
      :initarg :buffer-list
      :initform #'helm-buffer-list
      :custom function
      :documentation
      "  A function with no arguments to create buffer list.")
     (init :initform 'swint-helm-dired-buffers-list--init/curr-persp)
     (candidates :initform swint-helm-dired-buffers-list-cache/curr-persp)
     (matchplugin :initform nil)
     (match :initform 'helm-buffers-match-function)
     (persistent-action :initform 'helm-buffers-list-persistent-action)
     (resume :initform (lambda ()
                         (run-with-idle-timer
                          0.1 nil (lambda ()
                                    (with-helm-buffer
                                      (helm-force-update))))))
     (keymap :initform helm-buffer-map)
     (volatile :initform t)
     (help-message :initform 'helm-buffer-help-message)
     (persistent-help
      :initform
      "Show this buffer / C-u \\[helm-execute-persistent-action]: Kill this buffer")))
  (defclass swint-helm-dired-buffers-source/other-persps (helm-source-sync helm-type-buffer)
    ((buffer-list
      :initarg :buffer-list
      :initform #'helm-buffer-list
      :custom function
      :documentation
      "  A function with no arguments to create buffer list.")
     (init :initform 'swint-helm-dired-buffers-list--init/other-persps)
     (candidates :initform swint-helm-dired-buffers-list-cache/other-persps)
     (matchplugin :initform nil)
     (match :initform 'helm-buffers-match-function)
     (persistent-action :initform 'helm-buffers-list-persistent-action)
     (resume :initform (lambda ()
                         (run-with-idle-timer
                          0.1 nil (lambda ()
                                    (with-helm-buffer
                                      (helm-force-update))))))
     (keymap :initform helm-buffer-map)
     (volatile :initform t)
     (help-message :initform 'helm-buffer-help-message)
     (persistent-help
      :initform
      "Show this buffer / C-u \\[helm-execute-persistent-action]: Kill this buffer")))
  (defclass swint-helm-recentf-directory-source (helm-source-sync)
    ((init :initform (lambda () (recentf-mode 1)))
     (candidates :initform (lambda () (cl-remove-if (lambda (x)
                                                      (or (not (file-directory-p x))
                                                          (member x (cl-loop for xx in (cl-remove-if-not (lambda (x)
                                                                                                           (equal (buffer-mode x) 'dired-mode))
                                                                                                         (buffer-list))
                                                                             collect (expand-file-name (buffer-local-value 'default-directory xx))))))
                                                    recentf-list)))
     (pattern-transformer :initform 'helm-recentf-pattern-transformer)
     (match-part :initform (lambda (candidate)
                             (if (or helm-ff-transformer-show-only-basename
                                     helm-recentf--basename-flag)
                                 (helm-basename candidate) candidate)))
     (filter-one-by-one :initform (lambda (c)
                                    (if (and helm-ff-transformer-show-only-basename
                                             (not (consp c)))
                                        (cons (helm-basename c) c)
                                      c)))
     (keymap :initform helm-generic-files-map)
     (help-message :initform helm-generic-file-help-message)
     (action :initform (helm-actions-from-type-file))))
  (defvar swint-helm-dired-buffers-source-list/curr-persp nil)
  (defvar swint-helm-dired-buffers-source-list/other-persps nil)
  (defvar swint-helm-source-recentf-directory nil)
  (defun swint-helm-dired-buffers-list ()
    "Preconfigured `helm' lightweight dired buffers list."
    (interactive)
    (unless swint-helm-dired-buffers-source-list/curr-persp
      (setq swint-helm-dired-buffers-source-list/curr-persp
            (helm-make-source "Dired Buffers in current persp" 'swint-helm-dired-buffers-source/curr-persp)))
    (unless swint-helm-dired-buffers-source-list/other-persps
      (setq swint-helm-dired-buffers-source-list/other-persps
            (helm-make-source "Dired Buffers in other persps" 'swint-helm-dired-buffers-source/other-persps))
      (helm-add-action-to-source "Switch to persp/buffer" 'helm-switch-persp/buffer swint-helm-dired-buffers-source-list/other-persps 0))
    (unless swint-helm-source-recentf-directory
      (setq swint-helm-source-recentf-directory
            (helm-make-source "Recentf Directory" 'swint-helm-recentf-directory-source)))
    (let ((helm-ff-transformer-show-only-basename nil))
      (helm :sources '(swint-helm-dired-buffers-source-list/curr-persp
                       swint-helm-dired-buffers-source-list/other-persps
                       swint-helm-source-recentf-directory
                       helm-source-buffer-not-found)
            :buffer "*helm dired buffers-swint*"
            :keymap helm-buffer-map
            :truncate-lines t)))
  ;; ============helm-dired-buffer==============
;;;; helm-related-to-persp
  ;; =========helm-related-to-persp=============
  (defun helm-switch-persp/buffer (BUFFER-OR-NAME)
    "Helm-switch to persp/buffer simultaneously."
    (let ((buffer (get-buffer BUFFER-OR-NAME)))
      (if (bound-and-true-p persp-mode)
          (cl-loop for persp in (nreverse (cons  "i" (nreverse (delete "i" (persp-names)))))
                   when (or (memq buffer (persp-buffers (gethash persp (perspectives-hash))))
                            (string-equal persp "i"))
                   do (if (memq buffer (persp-buffers (persp-curr)))
                          (switch-to-buffer buffer)
                        (swint-persp-switch persp)
                        (if (window-live-p (get-buffer-window buffer))
                            (select-window (get-buffer-window buffer))
                          (switch-to-buffer buffer))))
        (switch-to-buffer buffer))))
  (defun swint-helm-buffer-switch-persp/other-window ()
    "Run switch-persp/other-window action from `helm-source-buffers-list'."
    (interactive)
    (with-helm-alive-p
      (helm-exit-and-execute-action 'swint-helm-switch-persp/other-window)))
  (defun swint-helm-buffer-persp-add-buffers ()
    "Run persp-add-buffer action from `helm-source-buffers-list'."
    (interactive)
    (with-helm-alive-p
      (helm-exit-and-execute-action 'swint-helm-persp-add-buffers)))
  (defun swint-helm-buffer-persp-remove-buffers ()
    "Run persp-remove-buffer action from `helm-source-buffers-list'."
    (interactive)
    (with-helm-alive-p
      (helm-exit-and-execute-action 'swint-helm-persp-remove-buffers)))
  (defun swint-helm-switch-persp/other-window (buffer)
    "Helm-switch to persp/other-window simultaneously."
    (if (or (not (bound-and-true-p persp-mode))
            (memq buffer (persp-buffers (persp-curr))))
        (switch-to-buffer-other-window buffer)
      (let ((curr-buf (current-buffer)))
        (swint-persp-switch "i")
        (switch-to-buffer curr-buf)
        (switch-to-buffer-other-window buffer))))
  (defun swint-helm-persp-add-buffers (_ignore)
    (let* ((bufs (helm-marked-candidates))
           (added-bufs (cl-count-if 'persp-add-buffer bufs)))
      (when (buffer-live-p helm-buffer)
        (with-helm-buffer
          (setq helm-marked-candidates nil
                helm-visible-mark-overlays nil)))
      (message "Addded %s buffer(s)" added-bufs)))
  (defun swint-helm-persp-remove-buffers (_ignore)
    (let* ((bufs (helm-marked-candidates))
           (removed-bufs (cl-count-if 'persp-remove-buffer bufs)))
      (when (buffer-live-p helm-buffer)
        (with-helm-buffer
          (setq helm-marked-candidates nil
                helm-visible-mark-overlays nil)))
      (message "Removed %s buffer(s)" removed-bufs)))
  ;; =========helm-related-to-persp=============
;;;; 在其他helm-buffer中运行helm命令
  ;; ======在其他helm-buffer中运行helm命令======
  (defun swint-helm-file-buffers-after-quit ()
    (interactive)
    (helm-run-after-exit #'(lambda () (swint-helm-file-buffers-list))))
  (defun swint-helm-dired-buffers-after-quit ()
    (interactive)
    (helm-run-after-exit #'(lambda () (swint-helm-dired-buffers-list))))
  (defun swint-helm-bookmarks-after-quit ()
    (interactive)
    (helm-run-after-exit #'(lambda () (helm-bookmarks))))
  (defun swint-helm-projectile-after-quit ()
    (interactive)
    (helm-run-after-exit #'(lambda () (helm-projectile))))
  ;; ======在其他helm-buffer中运行helm命令======
;;;; helm-open-file-with-lister
  ;; ========helm-open-file-with-lister=========
  (defun helm-open-file-with-lister (_candidate)
    "Opens a file with lister of total commander."
    (start-process-shell-command
     "tc" "*tc*"
     (concat "wine "
             "~/.wine/drive_c/totalcmd/TOTALCMD.EXE /O /T /S=L z:"
             (replace-regexp-in-string " " "\\\\ "
                                       (expand-file-name _candidate)))))
  (defun helm-ff-run-open-file-with-lister ()
    "Run Rename file action from `helm-source-find-files'."
    (interactive)
    (with-helm-alive-p
      (helm-exit-and-execute-action 'helm-open-file-with-lister)))
  ;; ========helm-open-file-with-lister=========
;;;; helm-locate
  ;; ==============helm-locate==================
  ;; 默认使用/var/lib/mlocate/mlocate.db数据库，包含系统文件，使用cron每天定时更新或sudo updatedb更新。
  (defun swint-helm-locate (&optional arg)
    (interactive "P")
    (let ((helm-locate-create-db-command "updatedb -l 0 -o ~/.helm-locate.db -U ~/")
          (helm-locate-command "locate -b -i %s -r %s -d ~/.helm-locate.db"))
      (when arg ;; 更新~/.helm-locate.db文件。
        (start-process-shell-command
         "Updating-locate-db-file" "*Updating-locate-db-file*"
         helm-locate-create-db-command))
      (helm-locate nil)))
  ;; ==============helm-locate==================
  )
;; ====================helm=====================
;;; helm_lacarte
;; ================helm_lacarte=================
(def-package! lacarte
  :commands helm-math-symbols
  :bind (("<escape> M-x" . lacarte-execute-command)
         ("C-x `" . lacarte-execute-menu-command))
  :config
  ;; 使用helm-insert-latex-math代替。
  (defvar helm-source-lacarte-math
    '((name . "Math Symbols")
      (init . (lambda()
                (setq helm-lacarte-major-mode major-mode)))
      (candidates
       . (lambda () (if (eq helm-lacarte-major-mode 'latex-mode)
                        (delete '(nil) (lacarte-get-a-menu-item-alist LaTeX-math-mode-map)))))
      (action . (("Open" . (lambda (candidate)
                             (call-interactively candidate)))))))
  (defun helm-math-symbols ()
    "Helm for searching math menus."
    (interactive)
    (helm '(helm-source-lacarte-math)
          (thing-at-point 'symbol) "Symbol: "
          nil nil "*helm math symbols*")))
;; ================helm_lacarte=================
;;; helm-bibtex
;; ================helm-bibtex==================
(def-package! helm-bibtex
  :commands (helm-bibtex-with-local-bibliography
             bibtex-completion-find-pdf
             bibtex-completion-get-entry-for-pdf)
  :bind (("C-x b" . swint-helm-bibtex)
         ("C-x B" . helm-bibtex))
  :init
  (add-hook 'LaTeX-mode-hook '(lambda ()
                                (bind-key "C-c b" 'helm-bibtex-with-local-bibliography LaTeX-mode-map)))
  :config
  (define-key helm-map (kbd "C-c j") '(lambda () (interactive)
                                        (with-helm-alive-p
                                          (helm-exit-and-execute-action 'helm-bibtex-open-pdf-externally))))
  (define-key helm-map (kbd "C-c o") '(lambda () (interactive)
                                        (with-helm-alive-p
                                          (helm-exit-and-execute-action 'helm-bibtex-open-pdf))))
  (define-key helm-map (kbd "C-c l") '(lambda () (interactive)
                                        (with-helm-alive-p
                                          (helm-exit-and-execute-action 'helm-bibtex-edit-notes))))
  (setq bibtex-completion-cite-prompt-for-optional-arguments nil
        bibtex-completion-additional-search-fields '(keywords)
        bibtex-completion-pdf-field "file"
        bibtex-completion-bibliography '("~/.bib/ALL.bib") ;zotero-better-bibtex自动更新。
        bibtex-completion-notes-path "~/Zotero/storage/TKM9D893/notes.org")
  (defvar bibtex-completion-bibliography/curr nil)
  (defun swint-helm-bibtex (&optional arg)
    (interactive "P")
    (when (or arg (not bibtex-completion-bibliography/curr))
      (setq bibtex-completion-bibliography/curr
            (helm-comp-read "Bibtex completion bibliography: "
                            (directory-files (expand-file-name "~/.bib/") t "\\.bib$")
                            :marked-candidates t
                            :buffer "*helm bibtex-swint*")))
    (let ((bibtex-completion-bibliography bibtex-completion-bibliography/curr))
      (helm-bibtex nil bibtex-completion-bibliography/curr)))
  (defun bibtex-completion-get-entry-for-pdf (pdf-file)
    "Find entry for pdf-file in .bib file."
    (with-temp-buffer
      (mapc #'insert-file-contents
            (-flatten (list bibtex-completion-bibliography)))
      (goto-char (point-min))
      (when (re-search-forward pdf-file nil t)
        (re-search-backward (concat "^@\\(" parsebib--bibtex-identifier
                                    "\\)[[:space:]]*[\(\{][[:space:]]*"
                                    parsebib--key-regexp "[[:space:]]*,"))
        (let ((entry-type (match-string 1)))
          (reverse (bibtex-completion-prepare-entry (parsebib-read-entry entry-type) nil nil))))))
  (defcustom helm-bibtex-pdf-open-externally-function '(lambda (fpath)
                                                         (dired-async-shell-command fpath))
    "The function used for opening PDF files externally."
    :group 'bibtex-completion
    :type 'function)
  (defun bibtex-completion-open-pdf-externally (candidates)
    "Open the PDFs associated with the marked entries externally."
    (--if-let
        (-flatten
         (-map 'bibtex-completion-find-pdf
               (if (listp candidates) candidates (list candidates))))
        (-each it helm-bibtex-pdf-open-externally-function)
      (message "No PDF(s) found.")))
  (helm-bibtex-helmify-action bibtex-completion-open-pdf-externally helm-bibtex-open-pdf-externally))
;; ================helm-bibtex==================
;;; helm-swoop
;; ================helm-swoop===================
(def-package! helm-swoop
  :bind (("M-s M-s" . helm-swoop)
         ("M-s M-S" . helm-multi-swoop-all))
  :config
  ;; helm-swoop 中使用C-c C-e编辑，C-x C-s保存。
  (define-key isearch-mode-map (kbd "M-s M-s") 'helm-swoop-from-isearch)
  (define-key helm-swoop-map (kbd "M-S") 'helm-multi-swoop-all-from-helm-swoop))
;; ================helm-swoop===================
;;; helm-unicode
;; ===============helm-unicode==================
(def-package! helm-unicode
  :commands helm-unicode)
;; ===============helm-unicode==================
;;; helm-ag
;; =================helm-ag=====================
(def-package! helm-ag
  ;; helm-do-ag 互动式搜索，但只能搜索一个词。
  ;; helm-ag 先输入词，可以在结果中搜索第二个词。
  :bind (("C-x g" . helm-do-ag)
         ("C-x G" . helm-do-ag-buffers))
  :config
  ;; C-c C-e 进入编辑模式，C-x C-s 保存helm-ag结果。
  (define-key helm-ag-map (kbd "C-h") 'helm-ag--up-one-level)
  (define-key helm-ag-map (kbd "C-o") 'helm-ag--run-other-window-action))
;; =================helm-ag=====================
;;; helm-descbinds
;; ==============helm-descbinds=================
(def-package! helm-descbinds
  :commands helm-descbinds)
;; ==============helm-descbinds=================
;;; helm-imenu
;; ================helm-imenu===================
(def-package! imenu
  :commands imenu-choose-buffer-index)
(def-package! imenu-anywhere
  :bind ("M-s I" . helm-imenu-anywhere)
  :config
  (setq imenu-anywhere-delimiter " | "))
(def-package! helm-imenu
  :bind (("M-s i" . helm-semantic-or-imenu)
         ("M-s M-i" . helm-imenu-outshine))
  :config
  (setq helm-imenu-delimiter " | ")
  ;; helm-imenu-outshine.
  (defvar helm-source-imenu-outshine nil)
  (defvar helm-cached-imenu-outshine-tick nil)
  (defvar helm-cached-imenu-outshine-candidates nil)
  (defvar-local imenu-outshine--index-alist nil)
  (defun imenu-outshine--make-index-alist ()
    "Create an index alist for the outshine headings."
    (setq imenu-outshine--index-alist
          (save-excursion
            (save-restriction
              (widen)
              (imenu--generic-function `((nil ,(concat (outshine-calc-outline-regexp) "\\(.*$\\)") 1))))))
    (imenu--truncate-items imenu-outshine--index-alist))
  (defun helm-imenu-outshine-candidates (&optional buffer)
    (with-current-buffer (or buffer helm-current-buffer)
      (let ((tick (buffer-modified-tick)))
        (if (eq helm-cached-imenu-outshine-tick tick)
            helm-cached-imenu-outshine-candidates
          (setq imenu-outshine--index-alist nil)
          (prog1 (setq helm-cached-imenu-outshine-candidates
                       (let ((index (imenu-outshine--make-index-alist)))
                         (helm-imenu--candidates-1
                          (delete (assoc "*Rescan*" index) index))))
            (setq helm-cached-imenu-outshine-tick tick))))))
  (defclass helm-imenu-outshine-source (helm-source-sync)
    ((candidates :initform 'helm-imenu-outshine-candidates)
     (candidate-transformer :initform 'helm-imenu-transformer)
     (persistent-action :initform 'helm-imenu-persistent-action)
     (persistent-help :initform "Show this entry")
     (keymap :initform helm-imenu-map)
     (help-message :initform 'helm-imenu-help-message)
     (action :initform 'helm-imenu-action)))
  (defun helm-imenu-outshine ()
    "Preconfigured `helm' for `imenu'."
    (interactive)
    (unless helm-source-imenu-outshine
      (setq helm-source-imenu-outshine
            (helm-make-source "Imenu outshine" 'helm-imenu-outshine-source
              :fuzzy-match helm-imenu-fuzzy-match)))
    (let ((imenu-auto-rescan t)
          (str (thing-at-point 'symbol))
          (helm-execute-action-at-once-if-one
           helm-imenu-execute-action-at-once-if-one))
      (helm :sources 'helm-source-imenu-outshine
            :default (list (concat "\\_<" str "\\_>") str)
            :preselect str
            :buffer "*helm imenu outshine*"))))
;; ================helm-imenu===================
(provide 'setup_helm)
