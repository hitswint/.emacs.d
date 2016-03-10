;; ==================dirtree=========================
(use-package dirtree
  ;; Enabled at commands.
  :defer t
  :commands dirtree
  :init
  (bind-key "C-x j" 'dirtree-local)
  (bind-key "C-x J" 'dirtree-home)
  (bind-key "C-q" 'dirtree-exist-kill-this-buffer)
  (defun dirtree-local ()
    (interactive)
    (let* ((file buffer-file-name)
           (dir (if file (file-name-directory file) default-directory))
           (dirtree-buffer-name (if (eq major-mode 'dired-mode)
                                    (file-name-nondirectory (dired-get-filename))
                                  (if file (file-name-nondirectory file) nil))))
      (dirtree dir nil)
      (if dirtree-buffer-name
          (re-search-forward (concat " " dirtree-buffer-name) nil t))
      (beginning-of-line)))
  (defun dirtree-home ()
    (interactive)
    (dirtree "~/" nil))
  (defun dirtree-exist-kill-this-buffer ()
    "switch to dirtree after killing this buffer"
    (interactive)
    (let ((swint-previous-buffer (car (swint-iswitchb-make-buflist))))
      (if (equal (buffer-name (other-buffer (current-buffer) t)) "*dirtree*")
          (dirtree-switch-to-dirtree)
        (progn
          (bc-set)
          (kill-this-buffer)
          ;; 关闭当前buffer之后切换到之前所在的buffer。
          (switch-to-buffer swint-previous-buffer)))))
  (defun dirtree-switch-to-dirtree ()
    "kill this buffer and switch to other windows"
    (interactive)
    (bc-set)
    (kill-this-buffer)
    (other-window 1))
  ;; ==================关闭当前buffer之后切换到之前访问过的buffer=======================
  ;; 原始的关闭buffer存在两个问题：
  ;; 一是会切换到helm buffer中，二是在persp之间切换时会切换到上一个persp的buffer中。
  (defvar swint-iswitchb-buflist nil
    "Stores the current list of buffers that will be searched through.")
  (defvar swint-iswitchb-bufs-in-frame nil
    "List of the buffers visible in the current frame.")
  (defcustom swint-iswitchb-all-frames 'visible
    "Argument to pass to `walk-windows' when iswitchb is finding buffers.
See documentation of `walk-windows' for useful values."
    :type '(choice (const :tag "Selected frame only" nil)
                   (const :tag "All existing frames" t)
                   (const :tag "All visible frames" visible)
                   (const :tag "All frames on this terminal" 0)))
  (defun swint-iswitchb-get-bufname (win)
    "Used by `swint-iswitchb-get-buffers-in-frames' to walk through all windows."
    (let ((buf (buffer-name (window-buffer win))))
      (if (not (member buf swint-iswitchb-bufs-in-frame))
          ;; Only add buf if it is not already in list.
          ;; This prevents same buf in two different windows being
          ;; put into the list twice.
          (setq swint-iswitchb-bufs-in-frame
                (cons buf swint-iswitchb-bufs-in-frame)))))
  (defun swint-iswitchb-get-buffers-in-frames (&optional current)
    "Return the list of buffers that are visible in the current frame.
If optional argument CURRENT is given, restrict searching to the
current frame, rather than all frames, regardless of value of
swint-`iswitchb-all-frames'."
    (let ((swint-iswitchb-bufs-in-frame nil))
      (walk-windows 'swint-iswitchb-get-bufname nil
                    (if current
                        nil
                      swint-iswitchb-all-frames))
      swint-iswitchb-bufs-in-frame))
  (defun swint-iswitchb-make-buflist ()
    "该函数使关闭当前buffer之后切换到之前访问过的buffer"
    (setq swint-iswitchb-buflist
          (let* ((swint-iswitchb-current-buffers (swint-iswitchb-get-buffers-in-frames))
                 (swint-iswitchb-temp-buflist
                  (delq nil
                        (mapcar
                         (lambda (x)
                           (if (not
                                (or
                                 (swint-iswitchb-ignore-buffername-p x)
                                 (memq x swint-iswitchb-current-buffers)))
                               x))
                         ;; 只使用(persp-buffers persp-curr)产生的buffer list顺序不对，无法切换回之前的buffer。
                         (remove-if-not (lambda (x) (member x (remq nil (mapcar 'buffer-name (persp-buffers persp-curr)))))
                                        (helm-buffer-list))))))
            (setq swint-iswitchb-temp-buflist
                  (nconc swint-iswitchb-temp-buflist swint-iswitchb-current-buffers))
            swint-iswitchb-temp-buflist)))
  (defun swint-iswitchb-ignore-buffername-p (bufname)
    "Return t if the buffer BUFNAME should be ignored."
    (let ((data       (match-data))
          (re-list    swint-iswitchb-buffer-ignore)
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
  (defcustom swint-iswitchb-buffer-ignore
    '("^ ")
    "Define ignore list."
    :type '(repeat (choice regexp function)))
  (setq swint-iswitchb-buffer-ignore '("\\` " "\\`\\*sdcv\\*\\'" "\\`\\*Completions\\*\\'" "\\`\\*Compile\\-Log\\*\\'" "\\`\\*calculator\\*\\'" "\\`\\*Ibuffer\\*\\'" "\\`\\*Calendar\\*\\'" "\\`Enjoy\\ Music\\'" "\\`\\*helm.*\\*\\'" "\\`\\*Helm.*\\*\\'"))
  ;; ==================关闭当前buffer之后切换到之前访问过的buffer=======================
  :config
  (defun dirtree-shell-command ()
    "open file with external app"
    (interactive)
    (let ((widget (widget-at (1- (line-end-position))))
          file)
      (if (setq file (widget-get widget :file))
          (cond
           (is-lin
            (async-shell-command-no-output-buffer-from-file file))
           (is-win
            (progn
              (if (and (or (string-equal (file-name-extension file) "doc")
                           (string-equal (file-name-extension file) "docx"))
                       (not (string-match "WINWORD.EXE" (concat (prin1-to-string (proced-process-attributes))))))
                  (progn (w32-shell-execute "open" "word")
                         (sit-for 5)))
              (w32-browser file)))))))
  (defun dirtree-quit ()
    "quit dirtree and delete window"
    (interactive)
    (kill-this-buffer)
    (delete-window))
  (defun dirtree-open-and-kill-dirtree ()
    "go to the position of buffer"
    (interactive)
    (dirtree-display)
    (kill-buffer (other-buffer (current-buffer) t))
    (delete-other-windows))
  (add-hook 'dirtree-mode-hook
            '(lambda ()
               (define-key dirtree-mode-map (kbd "RET") 'dirtree-open-and-kill-dirtree)
               (define-key dirtree-mode-map (kbd "C-p") 'tree-mode-previous-node)
               (define-key dirtree-mode-map (kbd "C-n") 'tree-mode-next-node)
               (define-key dirtree-mode-map "\C-j" 'dirtree-shell-command)
               (define-key dirtree-mode-map (kbd "q") 'dirtree-quit)
               (define-key dirtree-mode-map (kbd "i") 'tree-mode-toggle-expand))))
;; 放弃使用键盘宏
;; (fset 'dirtree-local
;;       [?\C-c return return ?\c ?\C-a ?\M-f ?\M-b])
;; (fset 'dirtree-home
;;       [?\C-c return ?\C-a ?\C-f ?\C-f ?\C-k ?\C-m ?\C-n])
;; ==================dirtree=========================
(provide 'setup_dirtree)
