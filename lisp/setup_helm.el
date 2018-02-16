;;; helm
;; ====================helm=====================
(use-package helm
  :config
  (use-package helm-config)
  (use-package helm-for-files)
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
                                              (iswitchb-buffer)
                                              (dired-create-directory)))
  (setq helm-projectile-sources-list '(helm-source-projectile-projects
                                       helm-source-projectile-files-list
                                       helm-source-projectile-buffers-list))
  (setq helm-buffer-details-flag nil)
  (setq helm-ff-newfile-prompt-p nil)
  (setq helm-split-window-default-side 'same)
  (setq helm-kill-ring-threshold 1)
  (setq helm-pdfgrep-default-read-command "llpp -page %p \"%f\"")
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
  (global-set-key (kbd "C-M-y") 'helm-show-kill-ring)
  (global-set-key (kbd "C-'") 'helm-bookmarks)
  (global-set-key (kbd "C-,") 'swint-helm-file-buffers-list)
  (global-set-key (kbd "C-.") 'swint-helm-dired-buffers-list)
  (global-set-key (kbd "C-x C-f") 'helm-find-files)
  (global-set-key (kbd "C-x f") 'helm-find)
  (global-set-key (kbd "M-x") 'helm-M-x)
  (global-set-key (kbd "C-x c d") 'helm-apt)
  (global-set-key (kbd "C-x y") 'helm-resume)
  (define-key helm-map (kbd "C-;") 'helm-toggle-visible-mark)
  (define-key helm-map (kbd "C-l") 'helm-execute-persistent-action)
  (define-key helm-map (kbd "C-M-p") 'helm-previous-source)
  (define-key helm-map (kbd "C-M-n") 'helm-next-source)
  (define-key helm-map (kbd "M-U") 'helm-unmark-all)
  (define-key helm-map (kbd "M-t") 'helm-toggle-all-marks)
  (define-key helm-find-files-map (kbd "C-l") 'helm-execute-persistent-action)
  (define-key helm-find-files-map (kbd "C-h") 'helm-find-files-up-one-level)
  (define-key helm-find-files-map (kbd "M-U") 'helm-unmark-all)
  (define-key helm-find-files-map (kbd "M-t") 'helm-toggle-all-marks)
  (define-key helm-find-files-map (kbd "C-o") 'helm-ff-run-switch-other-window)
  (define-key helm-buffer-map (kbd "C-M-j") 'swint-helm-buffer-persp-add-buffers)
  (define-key helm-buffer-map (kbd "C-M-k") 'swint-helm-buffer-persp-remove-buffers)
  (define-key helm-buffer-map (kbd "C-o") 'swint-helm-buffer-switch-persp/other-window)
  (define-key helm-read-file-map (kbd "C-l") 'helm-execute-persistent-action)
  (define-key helm-read-file-map (kbd "C-h") 'helm-find-files-up-one-level)
  (define-key helm-grep-map (kbd "C-o") 'helm-grep-run-other-window-action)
  (define-key helm-generic-files-map (kbd "C-o") 'helm-ff-run-switch-other-window)
  (when is-lin
    (define-key helm-find-files-map (kbd "C-j") 'helm-ff-run-open-file-externally)
    (define-key helm-generic-files-map (kbd "C-j") 'helm-ff-run-open-file-externally))
  (when is-win
    (define-key helm-find-files-map (kbd "C-j") 'helm-ff-run-open-file-with-default-tool)
    (define-key helm-generic-files-map (kbd "C-j") 'helm-ff-run-open-file-with-default-tool))
  ;; ===============keybindings=================
;;;; helm-pinyin
  ;; ================helm-pinyin================
  (load "iswitchb-pinyin")
  ;; 支持中文拼音首字母匹配，会使helm-find-files匹配过多。
  (cl-defun helm-mm-3-match-py (orig-fn str &rest args)
    (apply orig-fn (concat str "|" (str-unicode-to-pinyin-initial str)) args))
  (advice-add 'helm-mm-3-match :around #'helm-mm-3-match-py)
  ;; 默认在输入前面加空格解决匹配问题。
  (defun helm-find-files-1-py (orig-fn fname &rest args)
    (apply orig-fn (concat fname " ") args))
  (advice-add 'helm-find-files-1 :around #'helm-find-files-1-py)
  ;; ================helm-pinyin================
;;;; helm-file-buffer
  ;; ============helm-file-buffer===============
  (defun swint-helm-file-buffers-list--init/curr-persp ()
    ;; Issue #51 Create the list before `helm-buffer' creation.
    (setq swint-helm-file-buffers-list-cache/curr-persp
          (remove-if (lambda (x) (equal (buffer-mode x) 'dired-mode))
                     (remove-if-not (lambda (x) (member x (remq nil (mapcar 'buffer-name (persp-buffers persp-curr)))))
                                    (helm-buffer-list))))
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
          (remove-if (lambda (x) (equal (buffer-mode x) 'dired-mode))
                     (remove-if (lambda (x) (member x (remq nil (mapcar 'buffer-name (persp-buffers persp-curr)))))
                                (helm-buffer-list))))
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
     (candidates :initform (lambda () (remove-if (lambda (x)
                                                   (or (file-directory-p x)
                                                       (member x (mapcar (lambda (xx)
                                                                           (buffer-file-name xx))
                                                                         (buffer-list)))))
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
    "Preconfigured `helm' lightweight version \(buffer -> recentf\)."
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
          (or (remove-if-not (lambda (x) (equal (buffer-mode x) 'dired-mode))
                             (remove-if-not (lambda (x) (member x (remq nil (mapcar 'buffer-name (persp-buffers persp-curr)))))
                                            (helm-buffer-list)))
              (list (buffer-name (get-buffer-create "*helm no dired buffers in current persp*")))))
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
          (remove-if-not (lambda (x) (equal (buffer-mode x) 'dired-mode))
                         (remove-if (lambda (x) (member x (remq nil (mapcar 'buffer-name (persp-buffers persp-curr)))))
                                    (helm-buffer-list))))
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
     (candidates :initform (lambda () (remove-if (lambda (x)
                                                   (or (not (file-directory-p x))
                                                       (member x (mapcar (lambda (xx)
                                                                           (expand-file-name (buffer-local-value 'default-directory xx)))
                                                                         (remove-if-not (lambda (x)
                                                                                          (equal (buffer-mode x) 'dired-mode))
                                                                                        (buffer-list))))))
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
    "Preconfigured `helm' to list buffers."
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
    (let ((swint-all-persps (nreverse (cons  "i" (nreverse (delete "i" (persp-names))))))
          (buffer (get-buffer BUFFER-OR-NAME)))
      (cl-loop for persp in swint-all-persps
               when (or (memq buffer (persp-buffers (gethash persp perspectives-hash)))
                        (string-equal persp "i"))
               do (if (memq buffer (persp-buffers persp-curr))
                      (switch-to-buffer buffer)
                    (swint-persp-switch persp)
                    (if (window-live-p (get-buffer-window buffer))
                        (select-window (get-buffer-window buffer))
                      (switch-to-buffer buffer))))))
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
    (if (memq buffer (persp-buffers persp-curr))
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
;;;; helm-locate
  ;; ==============helm-locate==================
  ;; 默认使用/var/lib/mlocate/mlocate.db数据库，包含系统文件，使用cron每天定时更新或sudo updatedb更新。
  (when is-lin
    (defun swint-helm-locate (&optional arg)
      (interactive "P")
      (let ((helm-locate-create-db-command "updatedb -l 0 -o ~/.helm-locate.db -U ~/")
            (helm-locate-command "locate -b -i %s -r %s -d ~/.helm-locate.db"))
        (when arg ;; 更新~/.helm-locate.db文件。
          (start-process-shell-command
           "Updating-locate-db-file" "*Updating-locate-db-file*"
           helm-locate-create-db-command))
        (helm-locate nil)))
    (global-set-key (kbd "C-x F") 'swint-helm-locate))
  ;; ==============helm-locate==================
;;;; 在其他helm-buffer中运行helm命令
  ;; ======在其他helm-buffer中运行helm命令======
  (defun swint-helm-file-buffers-after-quit ()
    (interactive)
    (helm-run-after-quit #'(lambda () (swint-helm-file-buffers-list))))
  (defun swint-helm-dired-buffers-after-quit ()
    (interactive)
    (helm-run-after-quit #'(lambda () (swint-helm-dired-buffers-list))))
  (defun swint-helm-bookmarks-after-quit ()
    (interactive)
    (helm-run-after-quit #'(lambda () (helm-bookmarks))))
  (defun swint-helm-projectile-after-quit ()
    (interactive)
    (helm-run-after-quit #'(lambda () (helm-projectile))))
  (define-key helm-map (kbd "C-,") 'swint-helm-file-buffers-after-quit)
  (define-key helm-map (kbd "C-.") 'swint-helm-dired-buffers-after-quit)
  (define-key helm-map (kbd "C-'") 'swint-helm-bookmarks-after-quit)
  (define-key helm-map (kbd "M-'") 'swint-helm-projectile-after-quit)
  (define-key helm-map (kbd "M-RET") 'helm-quit-and-find-file)
  ;; ======在其他helm-buffer中运行helm命令======
;;;; helm-open-file-with-lister
  ;; ========helm-open-file-with-lister=========
  (defun helm-open-file-with-lister (_candidate)
    "Opens a file with lister of total commander."
    (cond
     (is-win (w32-shell-execute
              "open" "c:/totalcmd/TOTALCMD.EXE" (concat "/O /T /S=L \" " (expand-file-name _candidate))))
     (is-lin (start-process-shell-command
              "tc" "*tc*"
              (concat "wine "
                      "~/.wine/drive_c/totalcmd/TOTALCMD.EXE /O /T /S=L z:"
                      (replace-regexp-in-string " " "\\\\ "
                                                (expand-file-name _candidate)))))))
  (defun helm-ff-run-open-file-with-lister ()
    "Run Rename file action from `helm-source-find-files'."
    (interactive)
    (with-helm-alive-p
      (helm-exit-and-execute-action 'helm-open-file-with-lister)))
  (define-key helm-find-files-map (kbd "C-M-j") 'helm-ff-run-open-file-with-lister)
  (define-key helm-generic-files-map (kbd "C-M-j") 'helm-ff-run-open-file-with-lister)
  ;; ========helm-open-file-with-lister=========
  )
;; ====================helm=====================
;;; helm_lacarte
;; ================helm_lacarte=================
(use-package lacarte
  :bind ("C-x `" . helm-browse-menubar)
  :init
  (add-hook 'LaTeX-mode-hook
            '(lambda ()
               (define-key LaTeX-mode-map (kbd "C-c m") 'helm-insert-latex-math)))
  (add-hook 'org-mode-hook
            '(lambda ()
               (define-key org-mode-map (kbd "C-c m") 'helm-insert-latex-math)))
  (setq LaTeX-math-menu-unicode t)
  :config
  ;; 使用helm自带的程序而不使用下列自定义的命令。
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
(use-package helm-bibtex
  :commands (swint-helm-bibtex-local bibtex-completion-find-pdf-in-field bibtex-completion-get-entry-for-pdf)
  :bind (("C-x b" . swint-helm-bibtex)
         ("C-x B" . helm-bibtex))
  :init
  (add-hook 'LaTeX-mode-hook
            '(lambda ()
               (define-key LaTeX-mode-map (kbd "C-c b") 'swint-helm-bibtex-local)))
  (add-hook 'org-mode-hook
            '(lambda ()
               (define-key org-mode-map (kbd "C-c b") 'swint-helm-bibtex-local)))
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
  (defun swint-helm-bibtex-local ()
    (interactive)
    (let* ((bibfile (or (car (zotelo--locate-bibliography-files))
                        (file-name-base)))
           (actual-bibfile (if (string-match (concat "\\." "bib" "$") bibfile)
                               (expand-file-name bibfile)
                             (concat (expand-file-name bibfile) "." "bib"))))
      (unless (file-exists-p actual-bibfile)
        (zotelo-set-collection))
      (unless zotelo-minor-mode
        (zotelo-minor-mode t))
      (let ((bibtex-completion-bibliography actual-bibfile))
        (helm-bibtex))))
  (defun swint-helm-bibtex ()
    (interactive)
    (let ((bibtex-completion-bibliography
           (read-file-name "File: " (expand-file-name "~/.bib/"))))
      (helm-bibtex)))
  (setq bibtex-completion-cite-default-command "citep"
        bibtex-completion-cite-prompt-for-optional-arguments nil
        bibtex-completion-additional-search-fields '(keywords)
        bibtex-completion-pdf-field "file"
        bibtex-completion-bibliography "~/.bib/ALL.bib"
        bibtex-completion-notes-path (concat (helm-get-firefox-user-init-dir)
                                             "zotero/storage/TKM9D893/notes.org"))
  ;; 通过pdf文件找到对应entry，供swint-interleave-open-notes-file-for-pdf使用。
  (defun bibtex-completion-get-entry-for-pdf (pdf-file)
    "Find entry for pdf-file in .bib file."
    (with-temp-buffer
      (mapc #'insert-file-contents
            (-flatten (list bibtex-completion-bibliography)))
      (goto-char (point-min))
      (when (re-search-forward (cond
                                (is-lin pdf-file)
                                (is-win (regexp-quote (replace-regexp-in-string
                                                       ":" "\\\\:"
                                                       (replace-regexp-in-string "/" "\\\\\\\\" pdf-file)))))
                               nil t)
        (re-search-backward (concat "^@\\(" parsebib--bibtex-identifier
                                    "\\)[[:space:]]*[\(\{][[:space:]]*"
                                    parsebib--key-regexp "[[:space:]]*,"))
        (let ((entry-type (match-string 1)))
          (reverse (bibtex-completion-prepare-entry (parsebib-read-entry entry-type) nil nil))))))
  ;; Added helm-bibtex-open-pdf-externally.
  (defcustom helm-bibtex-pdf-open-externally-function '(lambda (fpath)
                                                         (cond
                                                          (is-lin (dired-async-shell-command fpath))
                                                          (is-win (w32-browser fpath))))
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
  (helm-bibtex-helmify-action bibtex-completion-open-pdf-externally helm-bibtex-open-pdf-externally)
  (helm-delete-action-from-source "Insert citation" helm-source-bibtex)
  (helm-add-action-to-source "Insert citation" 'helm-bibtex-insert-citation helm-source-bibtex 0)
  (helm-add-action-to-source "Open PDF file externally (if present)" 'helm-bibtex-open-pdf-externally helm-source-bibtex 2))
;; ================helm-bibtex==================
;;; helm-swoop
;; ================helm-swoop===================
(use-package helm-swoop
  :bind (("M-s M-s" . helm-swoop)
         ("M-s M-S" . helm-multi-swoop-all))
  :config
  ;; helm-swoop 中使用C-c C-e编辑，C-x C-s保存。
  (define-key isearch-mode-map (kbd "M-s M-s") 'helm-swoop-from-isearch)
  (define-key helm-swoop-map (kbd "M-S") 'helm-multi-swoop-all-from-helm-swoop))
;; ================helm-swoop===================
;;; helm-unicode
;; ===============helm-unicode==================
(use-package helm-unicode
  :bind ("C-x c u" . helm-unicode))
;; ===============helm-unicode==================
;;; helm-ag
;; =================helm-ag=====================
(use-package helm-ag
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
(use-package helm-descbinds
  :commands helm-descbinds
  :config
  (helm-descbinds-mode))
;; ==============helm-descbinds=================
;;; helm-imenu
;; ================helm-imenu===================
(use-package imenu
  :commands imenu-choose-buffer-index)
(use-package imenu-anywhere
  :bind ("M-s I" . helm-imenu-anywhere)
  :config
  (setq imenu-anywhere-delimiter " | "))
(use-package helm-imenu
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
