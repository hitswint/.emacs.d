;;; helm-file-buffer
;; ============helm-file-buffer===============
(defun swint-helm-file-buffers-list--init/curr-persp ()
  ;; Issue #51 Create the list before `helm-buffer' creation.
  (setq swint-helm-file-buffers-list-cache/curr-persp
        (or (cl-remove-if (lambda (x) (or (equal (buffer-mode x) 'dired-mode)
                                          (not (member x (remq nil (mapcar 'buffer-name (persp-buffers persp-curr)))))))
                          (helm-buffer-list))
            '("*helm file buffers-swint*")))
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
        (or (cl-remove-if (lambda (x) (or (equal (buffer-mode x) 'dired-mode)
                                          (member x (remq nil (mapcar 'buffer-name (persp-buffers persp-curr))))))
                          (helm-buffer-list))
            '("*helm file buffers-swint*")))
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
;;;###autoload
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
;;; helm-dired-buffer
;; ============helm-dired-buffer==============
(defun swint-helm-dired-buffers-list--init/curr-persp ()
  ;; Issue #51 Create the list before `helm-buffer' creation.
  (setq swint-helm-dired-buffers-list-cache/curr-persp
        (or (cl-remove-if-not (lambda (x) (and (equal (buffer-mode x) 'dired-mode)
                                               (member x (remq nil (mapcar 'buffer-name (persp-buffers persp-curr))))))
                              (helm-buffer-list))
            '("*helm dired buffers-swint*")))
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
        (or (cl-remove-if-not (lambda (x) (and (equal (buffer-mode x) 'dired-mode)
                                               (not (member x (remq nil (mapcar 'buffer-name (persp-buffers persp-curr)))))))
                              (helm-buffer-list))
            '("*helm dired buffers-swint*")))
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
                                                        (member x (mapcar (lambda (xx)
                                                                            (expand-file-name (buffer-local-value 'default-directory xx)))
                                                                          (cl-remove-if-not (lambda (x)
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
;;;###autoload
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
;;; helm-related-to-persp
;; =========helm-related-to-persp=============
;;;###autoload
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
;;;###autoload
(defun swint-helm-buffer-switch-persp/other-window ()
  "Run switch-persp/other-window action from `helm-source-buffers-list'."
  (interactive)
  (with-helm-alive-p
    (helm-exit-and-execute-action 'swint-helm-switch-persp/other-window)))
;;;###autoload
(defun swint-helm-buffer-persp-add-buffers ()
  "Run persp-add-buffer action from `helm-source-buffers-list'."
  (interactive)
  (with-helm-alive-p
    (helm-exit-and-execute-action 'swint-helm-persp-add-buffers)))
;;;###autoload
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
;;; 在其他helm-buffer中运行helm命令
;; ======在其他helm-buffer中运行helm命令======
;;;###autoload
(defun swint-helm-file-buffers-after-quit ()
  (interactive)
  (helm-run-after-exit #'(lambda () (swint-helm-file-buffers-list))))
;;;###autoload
(defun swint-helm-dired-buffers-after-quit ()
  (interactive)
  (helm-run-after-exit #'(lambda () (swint-helm-dired-buffers-list))))
;;;###autoload
(defun swint-helm-bookmarks-after-quit ()
  (interactive)
  (helm-run-after-exit #'(lambda () (helm-bookmarks))))
;;;###autoload
(defun swint-helm-projectile-after-quit ()
  (interactive)
  (helm-run-after-exit #'(lambda () (helm-projectile))))
;; ======在其他helm-buffer中运行helm命令======
;;; helm-open-file-with-lister
;; ========helm-open-file-with-lister=========
(defun helm-open-file-with-lister (_candidate)
  "Opens a file with lister of total commander."
  (start-process-shell-command
   "tc" "*tc*"
   (concat "wine "
           "~/.wine/drive_c/totalcmd/TOTALCMD.EXE /O /T /S=L z:"
           (replace-regexp-in-string " " "\\\\ "
                                     (expand-file-name _candidate)))))
;;;###autoload
(defun helm-ff-run-open-file-with-lister ()
  "Run Rename file action from `helm-source-find-files'."
  (interactive)
  (with-helm-alive-p
    (helm-exit-and-execute-action 'helm-open-file-with-lister)))
;; ========helm-open-file-with-lister=========
;;; helm-locate
;; ==============helm-locate==================
;; 默认使用/var/lib/mlocate/mlocate.db数据库，包含系统文件，使用cron每天定时更新或sudo updatedb更新。
;;;###autoload
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
