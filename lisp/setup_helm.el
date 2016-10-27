;;; helm
;; ====================helm=====================
(use-package helm
  ;; Enabled automatically.
  :init
  ;; emacs24中tramp存在问题，helm-files调用tramp-loaddefs会花费很长时间。
  ;; 无网络时不存在问题，有网络时偶尔出现。下句解决helm启动变慢问题，源自水木。
  (setq tramp-ssh-controlmaster-options "-o ControlMaster=auto -o ControlPath='tramp.%%C' -o ControlPersist=no")
  ;; Stack Exchange上提供：(setq tramp-ssh-controlmaster-options "")。
  :config
  (use-package helm-config)
  (helm-mode 1)
  (global-set-key (kbd "C-M-y") 'helm-show-kill-ring)
  (global-set-key (kbd "C-,") 'swint-helm-file-buffers-list)
  (global-set-key (kbd "C-.") 'swint-helm-dired-buffers-list)
  (global-set-key (kbd "C-'") 'swint-helm-bookmarks)
  (global-set-key (kbd "C-x C-f") 'swint-helm-find-files)
  (global-set-key (kbd "C-x f") 'helm-find)
  (global-set-key (kbd "C-x F") 'swint-helm-locate)
  (global-set-key (kbd "M-x") 'helm-M-x)
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
  (defvar swint-helm-file-buffers-source-list/curr-persp nil)
  (defvar swint-helm-file-buffers-source-list/other-persps nil)
  (defvar swint-helm-source-recentf-file nil)
  (defcustom swint-helm-file-buffers-sources '(swint-helm-file-buffers-source-list/curr-persp
                                               swint-helm-file-buffers-source-list/other-persps
                                               swint-helm-source-recentf-file
                                               helm-source-buffer-not-found)
    "Default sources list used in `swint-file-buffers'."
    :group 'helm-misc
    :type '(repeat (choice symbol)))
  (defun swint-helm-file-buffers-list ()
    "Preconfigured `helm' lightweight version \(buffer -> recentf\)."
    (interactive)
    (unless swint-helm-file-buffers-source-list/curr-persp
      (setq swint-helm-file-buffers-source-list/curr-persp
            (helm-make-source "File Buffers in current persp" 'swint-helm-file-buffers-source/curr-persp)))
    (unless swint-helm-file-buffers-source-list/other-persps
      (progn (setq swint-helm-file-buffers-source-list/other-persps
                   (helm-make-source "File Buffers in other persps" 'swint-helm-file-buffers-source/other-persps))
             (helm-add-action-to-source "Switch to persp/buffer" 'helm-switch-persp/buffer swint-helm-file-buffers-source-list/other-persps 0)))
    (let ((helm-ff-transformer-show-only-basename nil))
      (helm-other-buffer swint-helm-file-buffers-sources "*helm file buffers-swint*")))
  ;; ============helm-file-buffer===============
;;;; helm-dired-buffer
  ;; ============helm-dired-buffer==============
  (defun swint-helm-dired-buffers-list--init/curr-persp ()
    ;; Issue #51 Create the list before `helm-buffer' creation.
    (setq swint-helm-dired-buffers-list-cache/curr-persp
          (remove-if-not (lambda (x) (equal (buffer-mode x) 'dired-mode))
                         (remove-if-not (lambda (x) (member x (remq nil (mapcar 'buffer-name (persp-buffers persp-curr)))))
                                        (helm-buffer-list))))
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
  (defvar swint-helm-dired-buffers-source-list/curr-persp nil)
  (defvar swint-helm-dired-buffers-source-list/other-persps nil)
  (defvar swint-helm-source-recentf-directory nil)
  (defun swint-helm-dired-buffers-list ()
    "Preconfigured `helm' to list buffers."
    (interactive)
    ;; (unless swint-helm-dired-buffers-source-list/curr-persp
    ;;   (setq swint-helm-dired-buffers-source-list/curr-persp
    ;;         (helm-make-source "Dired Buffers in current persp" 'swint-helm-dired-buffers-source/curr-persp)))
    ;; 根据当前persp中是否有dired buffer设置helm源，防止因无dired buffer产生错误。
    (if (member 'dired-mode (mapcar 'buffer-mode (persp-buffers persp-curr)))
        (setq swint-helm-dired-buffers-source-list/curr-persp
              (helm-make-source "Dired Buffers in current persp" 'swint-helm-dired-buffers-source/curr-persp))
      (setq swint-helm-dired-buffers-source-list/curr-persp nil))
    (unless swint-helm-dired-buffers-source-list/other-persps
      (progn (setq swint-helm-dired-buffers-source-list/other-persps
                   (helm-make-source "Dired Buffers in other persps" 'swint-helm-dired-buffers-source/other-persps))
             (helm-add-action-to-source "Switch to persp/buffer" 'helm-switch-persp/buffer swint-helm-dired-buffers-source-list/other-persps 0)))
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
  (defun swint-switch-persp/other-window (buffer)
    "Helm-switch to persp/other-window simultaneously"
    (if (memq buffer (persp-buffers persp-curr))
        (helm-switch-to-buffers buffer t)
      (let ((curr-buf (current-buffer)))
        (swint-persp-switch "i")
        (switch-to-buffer curr-buf)
        (switch-to-buffer-other-window buffer))))
  (defun swint-helm-buffer-switch-persp/other-window ()
    "Run switch-persp/other-window action from `helm-source-buffers-list'."
    (interactive)
    (with-helm-alive-p
      (helm-exit-and-execute-action 'swint-switch-persp/other-window)))
  (defun swint-helm-buffer-switch-to-buffers ()
    "Run switch-to-buffers action from `helm-source-buffers-list'."
    (interactive)
    (with-helm-alive-p
      (helm-exit-and-execute-action 'helm-switch-to-buffers)))
  (defun swint-helm-buffer-persp-remove-buffer ()
    "Run persp-remove-buffer action from `helm-source-buffers-list'."
    (interactive)
    (with-helm-alive-p
      (helm-exit-and-execute-action 'persp-remove-buffer)))
  ;; =========helm-related-to-persp=============
;;;; helm-bookmarks
  ;; ============helm-bookmarks=================
  (defcustom swint-helm-bookmarks-list
    '(helm-source-bookmarks)
    "Default sources list used in `swint-helm-bookmarks'."
    :type '(repeat (choice symbol))
    :group 'helm-files)
  (defun swint-helm-bookmarks ()
    "Preconfigured `helm-bookmarks' for opening files."
    (interactive)
    (let ((helm-ff-transformer-show-only-basename t))
      (helm-other-buffer swint-helm-bookmarks-list "*helm bookmarks-swint*")))
  ;; ============helm-bookmarks=================
;;;; helm-find-file
  ;; ============helm-find-file=================
  (defun swint-helm-find-files ()
    "Preconfigured `helm' for opening files.
Run all sources defined in `helm-for-files-preferred-list'."
    (interactive)
    (helm :sources 'helm-source-files-in-current-dir
          :ff-transformer-show-only-basename t
          :buffer "*helm find files-swint*"))
  ;; ============helm-find-file=================
;;;; helm-locate
  ;; ==============helm-locate==================
  ;; (global-set-key (kbd "C-x l") 'locate)
  ;; win上locate似乎搜索中文有问题，改用es。但是es似乎仍然搜索不了中文，而且会导致emacs死掉，放弃。
  ;; win上面的linux工具包括grep/locate/find都不能够搜索中文。
  ;; (setq helm-locate-command "es %s %s")
  (cond
   (is-lin
    ;; 使用 updatedb -l 0 -o ~/.helm-locate.db -U ~/ 建立用户数据库。
    (setq helm-locate-create-db-command "updatedb -l 0 -o ~/.helm-locate.db -U ~/")
    (setq helm-locate-command "locate -b -i %s -r %s -d ~/.helm-locate.db"))
   (is-win
    ;; 下列的命令建立locate数据库的时候，会导致数据库中记录的文件路径名为/cygdrive/c/，这种路径名emacs无法识别。
    ;; (setq helm-locate-create-db-command "updatedb --output=/cygdrive/c/Users/swint/.helm-locate.db --localpaths='/cygdrive/c/Users/swint/'")
    ;; (setq helm-locate-command "locate -b -i %s -r %s -d /cygdrive/c/Users/swint/.helm-locate.db")
    ;; 下面的命令建立locate数据库的时候，会导致cygwin警告ms dos path sytle，无妨，时间稍长；locate命令无法识别这种ms dos path sytle。
    (setq helm-locate-create-db-command "updatedb --output=c:/Users/swint/.helm-locate.db --localpaths='c:/Users/swint/'")
    (setq helm-locate-command "locate -b -i %s -r %s -d /cygdrive/c/Users/swint/.helm-locate.db")))
  (defun swint-helm-locate (&optional arg)
    (interactive "P")
    (if arg
        ;; 使用~/helm-locate-db.sh脚本更新~/.helm-locate.db文件。
        (let* ((locat-db-file (expand-file-name "~/.helm-locate.db"))
               (process (start-process-shell-command
                         "Updating-locate-db-file" "*Updating-locate-db-file*"
                         (concat "bash " (expand-file-name "~/helm-locate-db.sh")))))
          (set-process-sentinel
           process
           (lambda (process signal)
             (when (memq (process-status process) '(exit signal))
               (helm-locate nil)))))
      (call-interactively 'helm-locate)))
  ;; ==============helm-locate==================
;;;; 在别的helm-buffer中运行helm命令
  ;; ======在别的helm-buffer中运行helm命令======
  (defun swint-helm-file-buffers-after-quit ()
    "List swint-helm-file-buffers."
    (interactive)
    (helm-run-after-quit #'(lambda () (swint-helm-file-buffers-list))))
  (defun swint-helm-dired-buffers-after-quit ()
    "List swint-helm-dired-buffers."
    (interactive)
    (helm-run-after-quit #'(lambda () (swint-helm-dired-buffers-list))))
  (defun swint-helm-bookmarks-after-quit ()
    "List swint-helm-bookmarks."
    (interactive)
    (helm-run-after-quit #'(lambda () (swint-helm-bookmarks))))
  (defun swint-helm-find-files-after-quit ()
    "List swint-helm-find-files."
    (interactive)
    (helm-run-after-quit #'(lambda () (swint-helm-find-files))))
  ;; Fix bugs of helm-quit-and-find-file on dired buffer.
  (defun swint-helm-quit-and-find-file ()
    "Drop into `helm-find-files' from `helm'.
If current selection is a buffer or a file, `helm-find-files'
from its directory."
    (interactive)
    (helm-run-after-quit
     (lambda (f)
       (if (file-exists-p f)
           (if (file-directory-p f)
               (helm-find-files-1 (file-name-as-directory f))
             (helm-find-files-1 (file-name-directory f)
                                (concat
                                 "^"
                                 (regexp-quote
                                  (if helm-ff-transformer-show-only-basename
                                      (helm-basename f) f)))))))
     (let* ((sel       (helm-get-selection))
            (grep-line (and (stringp sel)
                            (helm-grep-split-line sel)))
            (bmk-name  (and (stringp sel)
                            (not grep-line)
                            (replace-regexp-in-string "\\`\\*" "" sel)))
            (bmk       (and bmk-name (assoc bmk-name bookmark-alist)))
            (buf       (helm-aif (get-buffer sel) (buffer-name it)))
            (default-preselection (or (buffer-file-name helm-current-buffer)
                                      default-directory)))
       (cond
        ;; Buffer.
        (buf (or (buffer-file-name sel)
                 (car (rassoc (get-buffer buf) dired-buffers))
                 (and (with-current-buffer buf
                        (eq major-mode 'org-agenda-mode))
                      org-directory
                      (expand-file-name org-directory))
                 (with-current-buffer buf default-directory)))
        ;; Bookmark.
        (bmk (helm-aif (bookmark-get-filename bmk)
                 (if (and ffap-url-regexp
                          (string-match ffap-url-regexp it))
                     it (expand-file-name it))
               default-directory))
        ((or (file-remote-p sel)
             (file-exists-p sel))
         (expand-file-name sel))
        ;; Grep.
        ((and grep-line (file-exists-p (car grep-line)))
         (expand-file-name (car grep-line)))
        ;; Occur.
        (grep-line
         (with-current-buffer (get-buffer (car grep-line))
           (or (buffer-file-name) default-directory)))
        ;; Url.
        ((and ffap-url-regexp (string-match ffap-url-regexp sel)) sel)
        ;; Default.
        (t default-preselection)))))
  (define-key helm-map (kbd "C-,") 'swint-helm-file-buffers-after-quit)
  (define-key helm-map (kbd "C-.") 'swint-helm-dired-buffers-after-quit)
  (define-key helm-map (kbd "C-'") 'swint-helm-bookmarks-after-quit)
  (define-key helm-map (kbd "C-x C-f") 'swint-helm-find-files-after-quit)
  (define-key helm-map (kbd "C-/") 'swint-helm-quit-and-find-file)
  (define-key helm-find-files-map (kbd "C-.") 'swint-helm-dired-buffers-after-quit)
  (define-key helm-find-files-map (kbd "C-x C-f") 'swint-helm-find-files-after-quit)
  ;; ======在别的helm-buffer中运行helm命令======
;;;; keybindings
  ;; ===============keybindings=================
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
  (define-key helm-buffer-map (kbd "M-RET") 'swint-helm-buffer-switch-to-buffers)
  (define-key helm-buffer-map (kbd "C-M-k") 'swint-helm-buffer-persp-remove-buffer)
  (define-key helm-buffer-map (kbd "C-o") 'swint-helm-buffer-switch-persp/other-window)
  (define-key helm-read-file-map (kbd "C-l") 'helm-execute-persistent-action)
  (define-key helm-read-file-map (kbd "C-h") 'helm-find-files-up-one-level)
  (define-key helm-grep-map (kbd "C-o") 'helm-grep-run-other-window-action)
  (define-key helm-generic-files-map (kbd "C-o") 'helm-ff-run-switch-other-window)
  (define-key helm-bookmark-map (kbd "C-o") 'helm-bookmark-run-jump-other-window)
  (when is-lin
    (define-key helm-find-files-map (kbd "C-j") 'helm-ff-run-open-file-externally)
    (define-key helm-generic-files-map (kbd "C-j") 'helm-ff-run-open-file-externally))
  (when is-win
    (define-key helm-find-files-map (kbd "C-j") 'helm-ff-run-open-file-with-default-tool)
    (define-key helm-generic-files-map (kbd "C-j") 'helm-ff-run-open-file-with-default-tool))
  ;; C-x c c helm-colors
  ;; C-x c b helm-resume 恢复之前的helm buffer，加C-u进行选择。
  ;; helm-mini下：C-c C-d 从列表中删除，但实际不kill buffer；C-c d kill buffer同时不关闭helm buffer；M-D kill buffer同时关闭helm。
  ;; helm-find-files-map下：
  ;; C-l 一次expand candidate，二次打开文件
  ;; TAB 打开选项
  ;; C-j 使用外部命令打开文件，加C-u选择命令
  ;; M-g s grep文件
  ;; M-e 打开eshell
  ;; M-C M-D M-R 复制 删除 重命名
  ;; C-= ediff
  ;; M-p 历史
  ;; 再重复一次C-x C-f locate查找文件，可以用sudo updatedb升级数据库
  ;; C-t 转换列表显示方式
  ;; C-] toggle basename/fullpath
  ;; C-backspace 开启关闭自动补全
  ;; C-{ C-} 放大缩小helm窗口
  ;; ===============keybindings=================
;;;; helm-pinyin
  ;; ================helm-pinyin================
  ;; iswitchb-pinyin给iswitchb增加拼音头字母搜索的，使用pinyin-initials-string-match函数。
  (load "iswitchb-pinyin")
  ;; 使buffer支持中文拼音首字母。
  (defun helm-buffer--match-pattern (pattern candidate)
    (let ((fun (if (and helm-buffers-fuzzy-matching
                        (not (pinyin-initials-string-match "\\`\\^" pattern)))
                   #'helm--mapconcat-candidate
                 #'identity)))
      (if (pinyin-initials-string-match "\\`!" pattern)
          (not (pinyin-initials-string-match (funcall fun (substring pattern 1))
                                             candidate))
        (pinyin-initials-string-match (funcall fun pattern) candidate))))
  ;; 使helm-find-files/recentf支持中文拼音首字母。
  ;; 会破坏helm-find-files的搜索方式，匹配过多，另建swint-helm-find-files。
  ;; 直接输入xx，会产生过多的匹配结果；xx后面加空格，以xx为起始字母；xx前面加空格，搜索结果正常。
  (cl-defun helm-mm-3-match (str &optional (pattern helm-pattern))
    "Check if PATTERN match STR.
When PATTERN contain a space, it is splitted and matching is done
with the several resulting regexps against STR.
e.g \"bar foo\" will match \"foobar\" and \"barfoo\".
Argument PATTERN, a string, is transformed in a list of
cons cell with `helm-mm-3-get-patterns' if it contain a space.
e.g \"foo bar\"=>((identity . \"foo\") (identity . \"bar\")).
Then each predicate of cons cell(s) is called with regexp of same
cons cell against STR (a candidate).
i.e (identity (string-match \"foo\" \"foo bar\")) => t."
    (let ((pat (helm-mm-3-get-patterns pattern)))
      (cl-loop for (predicate . regexp) in pat
               always (funcall predicate
                               (condition-case _err
                                   ;; FIXME: Probably do nothing when
                                   ;; using fuzzy leaving the job
                                   ;; to the fuzzy fn.
                                   (pinyin-initials-string-match regexp str)
                                 (invalid-regexp nil))))))
  ;; ================helm-pinyin================
;;;; total commander
  ;; ==============total commander==============
  ;;使用tc打开当前文件夹。
  (global-set-key (kbd "C-s-e") '(lambda ()
                                   (interactive)
                                   (cond
                                    (is-win (w32-shell-execute
                                             "open" "c:/totalcmd/TOTALCMD.EXE" (concat "/O /T \" " (expand-file-name default-directory))))
                                    (is-lin (progn (start-process-shell-command
                                                    "tc" "*tc*"
                                                    (concat "wine-development "
                                                            "/home/swint/.wine/drive_c/totalcmd/TOTALCMD.EXE /O /T z:"
                                                            (replace-regexp-in-string " " "\\\\ " (expand-file-name default-directory))))
                                                   (let ((default-directory
                                                           "/home/swint/.wine/drive_c/Program Files/viatc/"))
                                                     (start-process-shell-command
                                                      "viatc" "*viatc*"
                                                      "wine-development viatc.exe")))))))
  ;;使用lister直接浏览文件。
  (define-key dired-mode-map (kbd "C-M-j") '(lambda ()
                                              (interactive)
                                              (cond
                                               (is-win (w32-shell-execute
                                                        "open" "c:/totalcmd/TOTALCMD.EXE" (concat "/O /T /S=L \" " (dired-get-filename))))
                                               (is-lin (start-process-shell-command
                                                        "tc" "*tc*"
                                                        (concat "wine-development "
                                                                "/home/swint/.wine/drive_c/totalcmd/TOTALCMD.EXE /O /T /S=L z:"
                                                                (replace-regexp-in-string " " "\\\\ "
                                                                                          (expand-file-name (dired-get-filename)))))))))
  (defun helm-open-file-with-lister (_candidate)
    "Opens a file with lister of total commander."
    (cond
     (is-win (w32-shell-execute
              "open" "c:/totalcmd/TOTALCMD.EXE" (concat "/O /T /S=L \" " (expand-file-name _candidate))))
     (is-lin (start-process-shell-command
              "tc" "*tc*"
              (concat "wine-development "
                      "/home/swint/.wine/drive_c/totalcmd/TOTALCMD.EXE /O /T /S=L z:"
                      (replace-regexp-in-string " " "\\\\ "
                                                (expand-file-name _candidate)))))))
  (defun helm-ff-run-open-file-with-lister ()
    "Run Rename file action from `helm-source-find-files'."
    (interactive)
    (with-helm-alive-p
      (helm-exit-and-execute-action 'helm-open-file-with-lister)))
  (define-key helm-find-files-map (kbd "C-M-j") 'helm-ff-run-open-file-with-lister)
  (define-key helm-generic-files-map (kbd "C-M-j") 'helm-ff-run-open-file-with-lister)
  ;; ==============total commander==============
  )
;; ====================helm=====================
;;; helm_lacarte
;; ================helm_lacarte=================
(use-package lacarte
  ;; Enabled at commands.
  :defer t
  :bind ("C-x `" . helm-browse-menubar)
  :init
  (add-hook 'LaTeX-mode-hook
            '(lambda ()
               (define-key LaTeX-mode-map (kbd "C-c `") 'helm-insert-latex-math)))
  (add-hook 'org-mode-hook
            '(lambda ()
               (define-key org-mode-map (kbd "C-c `") 'helm-insert-latex-math)))
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
    "helm for searching math menus"
    (interactive)
    (helm '(helm-source-lacarte-math)
          (thing-at-point 'symbol) "Symbol: "
          nil nil "*helm math symbols*")))
;; ================helm_lacarte=================
;;; helm-bibtex
;; ================helm-bibtex==================
(use-package helm-bibtex
  ;; Enabled at commands.
  :defer t
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
  (define-key helm-map (kbd "C-c m") '(lambda () (interactive)
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
  ;; 通过pdf文件找到对应entry，供swint-interleave--open-notes-file-for-pdf使用。
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
                                                       (replace-regexp-in-string "/" "\\\\\\\\" pdf-file))))) nil t)
        (re-search-backward (concat "^@\\(" parsebib--bibtex-identifier
                                    "\\)[[:space:]]*[\(\{][[:space:]]*"
                                    parsebib--key-regexp "[[:space:]]*,"))
        (let ((entry-type (match-string 1)))
          (reverse (bibtex-completion-prepare-entry (parsebib-read-entry entry-type) nil nil))))))
  ;; Added helm-bibtex-open-pdf-externally.
  (defcustom helm-bibtex-pdf-open-externally-function '(lambda (fpath)
                                                         (cond
                                                          (is-lin (async-shell-command-no-output-buffer-from-file fpath))
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
  ;; 设置.bib文件的编码格式，否则出现乱码。
  ;; (zotelo-translator-charsets (quote ((BibTeX . "Unicode") (Default . "Unicode"))))
  (helm-delete-action-from-source "Insert citation" helm-source-bibtex)
  (helm-add-action-to-source "Insert citation" 'helm-bibtex-insert-citation helm-source-bibtex 0)
  (helm-add-action-to-source "Open PDF file externally (if present)" 'helm-bibtex-open-pdf-externally helm-source-bibtex 2))
;; ================helm-bibtex==================
;;; helm-swoop
;; ================helm-swoop===================
(use-package helm-swoop
  ;; Enabled at commands.
  :defer t
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
  ;; Enabled at commands.
  :defer t
  :bind ("M-s `" . helm-unicode))
;; ===============helm-unicode==================
;;; helm-ag
;; =================helm-ag=====================
;; 使用helm-ag代替helm-grep。
;; (global-set-key (kbd "C-x g") 'helm-do-grep) ;加C-u为递归
(use-package helm-ag
  ;; Enabled at commands.
  :defer t
  ;; helm-do-ag 互动式搜索，但只能搜索一个词。
  ;; helm-ag 先输入词，可以在结果中搜索第二个词。
  :bind (("C-x g" . helm-do-ag)
         ("C-x G" . helm-do-ag-buffers))
  :config
  (define-key helm-ag-map (kbd "C-h") 'helm-ag--up-one-level)
  (define-key helm-ag-map (kbd "C-o") 'helm-ag--run-other-window-action)
  ;; C-c C-e 进入编辑模式，C-x C-s 保存helm-ag结果。
  (when is-win
    (custom-set-variables
     '(helm-ag-base-command "pt -e --nocolor --nogroup"))))
;; =================helm-ag=====================
;;; helm-descbinds
;; ==============helm-descbinds=================
(use-package helm-descbinds
  ;; Enabled at commands.
  :defer t
  :commands helm-descbinds
  :config
  (helm-descbinds-mode))
;; ==============helm-descbinds=================
;;; helm-imenu
;; ================helm-imenu===================
(use-package imenu
  ;; Enabled at commands.
  :defer t
  :commands imenu-choose-buffer-index)
(use-package imenu-anywhere
  ;; Enabled at commands.
  :defer t
  :bind ("M-s I" . helm-imenu-anywhere)
  :config
  (setq imenu-anywhere-delimiter " | "))
(use-package helm-imenu
  ;; Enabled at commands.
  :defer t
  :bind (("M-s i" . helm-semantic-or-imenu)
         ("M-s M-I" . helm-imenu-outshine))
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
