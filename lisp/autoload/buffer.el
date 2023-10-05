;;; 退出shell时关闭buffer
;; ==============退出shell时关闭buffer=============
(defun kill-shell-buffer(process event)
  "The one actually kill shell buffer when exit. "
  (swint-kill-buffer (process-buffer process)))
;;;###autoload
(defun kill-shell-buffer-after-exit()
  "kill shell buffer when exit."
  (set-process-sentinel (get-buffer-process (current-buffer))
                        #'kill-shell-buffer))
;; ==============退出shell时关闭buffer=============
;;; 上一个下一个buffer
;; ===============上一个下一个buffer===============
;;;###autoload
(defun next-user-buffer ()
  "Switch to the next user buffer.
User buffers are those whose name does not start with *."
  (interactive)
  (next-buffer)
  (let ((i 0))
    (while (and (string-match "^*" (buffer-name)) (< i 50))
      (setq i (1+ i)) (next-buffer) )))
;;;###autoload
(defun previous-user-buffer ()
  "Switch to the previous user buffer.
User buffers are those whose name does not start with *."
  (interactive)
  (previous-buffer)
  (let ((i 0))
    (while (and (string-match "^*" (buffer-name)) (< i 50))
      (setq i (1+ i)) (previous-buffer) )))
;;;###autoload
(defun next-emacs-buffer ()
  "Switch to the next emacs buffer.
Emacs buffers are those whose name starts with *."
  (interactive)
  (next-buffer)
  (let ((i 0))
    (while (and (not (string-match "^*" (buffer-name))) (< i 50))
      (setq i (1+ i)) (next-buffer) )))
;;;###autoload
(defun previous-emacs-buffer ()
  "Switch to the previous emacs buffer.
Emacs buffers are those whose name starts with *."
  (interactive)
  (previous-buffer)
  (let ((i 0))
    (while (and (not (string-match "^*" (buffer-name))) (< i 50))
      (setq i (1+ i)) (previous-buffer) )))
;; ===============上一个下一个buffer===============
;;; 关闭buffer并删除文件
;; ==============关闭buffer并删除文件==============
;;;###autoload
(defun delete-current-buffer-file ()
  "Removes file connected to current buffer and kills buffer."
  (interactive)
  (let ((filename (buffer-file-name))
        (buffer (current-buffer))
        (name (buffer-name)))
    (when (and filename (file-exists-p filename)
               (yes-or-no-p "Are you sure you want to remove this file? "))
      (delete-file filename)
      (swint-kill-buffer buffer)
      (message "File '%s' successfully removed" filename))))
;; ==============关闭buffer并删除文件==============
;;; 重命名当前buffer
;; ================重命名当前buffer================
;;;###autoload
(defun rename-current-buffer-file ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " filename)))
        (if (get-buffer new-name)
            (error "A buffer named '%s' already exists!" new-name)
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil)
          (message "File '%s' successfully renamed to '%s'"
                   name (file-name-nondirectory new-name)))))))
;; ================重命名当前buffer================
;;; 获得当前buffer的major-mode
;; ===========获得当前buffer的major-mode===========
;;;###autoload
(defun buffer-mode (&optional buffer-or-name)
  "Returns the major mode associated with a buffer.
If buffer-or-name is nil return current buffer's mode."
  (buffer-local-value 'major-mode
                      (if buffer-or-name
                          (get-buffer buffer-or-name)
                        (current-buffer))))
;; ===========获得当前buffer的major-mode===========
;;; 获得不包括扩展名的文件名
;; ============获得不包括扩展名的文件名============
;;;###autoload
(defun file-basename (file)
  (let ((file-no-ending-slash (replace-regexp-in-string "/$" "" file)))
    (car (reverse (split-string file-no-ending-slash "/")))))
;; ============获得不包括扩展名的文件名============
;;; 关闭buffer后切换到之前的buffer
;; ==========关闭buffer后切换到之前的buffer========
;; kill-buffer存在问题：1. 会切换到helm buffer；2. 切换persp时会切换到前一个persp的buffer
(defcustom swint-iswitchb-buffer-ignore '("^ " "\\` " "\\`\\*sdcv\\*\\'" "\\`\\*Completions\\*\\'" "\\`\\*Compile\\-Log\\*\\'" "\\`\\*calculator\\*\\'" "\\`\\*Ibuffer\\*\\'" "\\`\\*Calendar\\*\\'" "\\`Enjoy\\ Music\\'" "\\`\\*helm.*\\*\\'" "\\`\\*Helm.*\\*\\'")
  "Define ignore list."
  :group 'autoload-buffer
  :type '(repeat (choice regexp function)))
;;;###autoload
(defun swint-kill-buffer (&optional BUFFER-OR-NAME)
  "Switch to prev buffer before killing current."
  (interactive)
  (let ((curr-buf (if BUFFER-OR-NAME
                      (get-buffer BUFFER-OR-NAME)
                    (current-buffer))))
    (cl-loop for w in (window-list)
             when (eq (window-buffer w) curr-buf)
             do (with-selected-window w
                  (let ((prev-buf (car (or (swint-filter-buffer-list (cl-loop for x in (window-prev-buffers w)
                                                                              collect (car x)))
                                           (swint-filter-buffer-list (buffer-list (selected-frame)) t)))))
                    (switch-to-buffer prev-buf))))
    (kill-buffer curr-buf)))
;;;###autoload
(defun swint-filter-buffer-list (buffers &optional include-current)
  "Remove buffers that are ignored or belong to other persps from buffers."
  (let* ((curr_name (buffer-name (current-buffer)))
         (buffers_name (mapcar 'buffer-name buffers))
         (buffers-persp-curr
          (or (and (bound-and-true-p persp-mode)
                   ;; 只使用(persp-buffers persp-curr)产生的buffer list顺序不对，无法切换回之前的buffer
                   (cl-remove-if-not
                    (lambda (x) (member x (remq nil (mapcar 'buffer-name (persp-buffers (persp-curr))))))
                    buffers_name))
              buffers_name)))
    (append (delete curr_name
                    (delq nil (cl-loop for x in buffers-persp-curr
                                       collect (unless (swint-iswitchb-ignore-buffername-p x) x))))
            (if include-current curr_name))))
(defun swint-iswitchb-ignore-buffername-p (bufname)
  "Return t if the buffer BUFNAME should be ignored."
  (let ((data (match-data))
        (re-list swint-iswitchb-buffer-ignore)
        ignorep
        nextstr)
    (while re-list
      (setq nextstr (car re-list))
      (cond
       ((stringp nextstr)
        (if (string-match nextstr bufname)
            (progn
              (setq ignorep t)
              (setq re-list nil))))
       ((functionp nextstr)
        (if (funcall nextstr bufname)
            (progn
              (setq ignorep t)
              (setq re-list nil)))))
      (setq re-list (cdr re-list)))
    (set-match-data data)
    ;; return the result
    ignorep))
;; ==========关闭buffer后切换到之前的buffer========
;;; minibuffer
;; =================minibuffer==================
;;;###autoload
(defun switch-to-minibuffer ()
  "Switch to minibuffer window."
  (interactive)
  (if (active-minibuffer-window)
      (select-window (active-minibuffer-window))
    (error "Minibuffer is not active")))
(define-key minibuffer-local-map (kbd "C-<tab>") 'nil)
;; =================minibuffer==================
;;; undo-kill-buffer
;; ==============undo-kill-buffer===============
(add-hook 'kill-buffer-hook #'swint-add-to-killed-list)
(defvar swint-killed-file-list nil
  "List of recently killed files.")
;;;###autoload
(defun swint-add-to-killed-list ()
  "Add to the `swint-killed-file-list' when killing the buffer."
  (let ((file-or-dired (or buffer-file-name
                           dired-directory)))
    (when file-or-dired
      (push file-or-dired swint-killed-file-list))))
;;;###autoload
(defun swint-undo-kill-buffer ()
  "Reopen the most recently killed file, if one exists."
  (interactive)
  (when swint-killed-file-list
    (find-file (pop swint-killed-file-list))))
;; ==============undo-kill-buffer===============
;;; swint-insert-zsh-completions
;; =========swint-insert-zsh-completions========
;;;###autoload
(defun swint-insert-zsh-completions ()
  (interactive)
  (let* ((enable-recursive-minibuffers t)
         (zsh-capture-completion-prog (expand-file-name "repos/zsh-capture-completion/capture.zsh" user-emacs-directory))
         (minibuffer-contents-before-point (minibuffer-completion-contents))
         (zsh-completion (car (split-string (helm-comp-read "Select options: "
                                                            (split-string (shell-command-to-string
                                                                           (format "%s '%s'" zsh-capture-completion-prog minibuffer-contents-before-point))
                                                                          "\n" t)
                                                            :buffer (concat "*helm zsh completions-swint*"))
                                            " " t))))
    (if (string-suffix-p " " minibuffer-contents-before-point)
        (insert zsh-completion)
      (when (re-search-backward (concat search-whitespace-regexp ".*"))
        (replace-match (concat " " zsh-completion))))))
(define-key minibuffer-local-map (kbd "C-x m") 'swint-insert-zsh-completions)
;; =========swint-insert-zsh-completions========
