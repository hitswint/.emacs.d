;;; dirtree
;; ====================dirtree=====================
(use-package dirtree
  ;; Enabled at commands.
  :defer t
  :commands swint-filter-buffer-list
  :bind (("C-x j" . dirtree-local)
         ("C-x J" . dirtree-home)
         ("C-q" . dirtree-kill-this-buffer))
  :config
  ;; 放弃使用键盘宏。
  ;; (fset 'dirtree-local
  ;;       [?\C-c return return ?\c ?\C-a ?\M-f ?\M-b])
  ;; (fset 'dirtree-home
  ;;       [?\C-c return ?\C-a ?\C-f ?\C-f ?\C-k ?\C-m ?\C-n])
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
  (defun dirtree-kill-this-buffer ()
    "Switch to dirtree after killing this buffer."
    (interactive)
    (let ((curr-buf (current-buffer))
          (prev-buf (car (or (swint-filter-buffer-list (mapcar '(lambda (x) (car x)) (window-prev-buffers)))
                             (swint-filter-buffer-list (buffer-list (selected-frame)) t)))))
      (bc-set)
      ;; 先切换到前一个buffer，再关闭当前buffer。
      (switch-to-buffer prev-buf)
      (kill-buffer curr-buf)
      (if (member "*dirtree*" (ido-get-buffers-in-frames 'current))
          (select-window (get-buffer-window "*dirtree*")) )))
;;;; 关闭buffer后切换到之前的buffer
  ;; =========关闭buffer后切换到之前的buffer=======
  ;; 原始的kill-buffer存在两个问题：1. 会切换到helm buffer中；2. 在persp之间切换时会切换到上一个persp的buffer中。
  (defun swint-filter-buffer-list (buffers &optional include-current)
    "Remove buffers that are ignored or belong to other persps from buffers."
    (let ((curr-buf-name (buffer-name (current-buffer))))
      (append (delete curr-buf-name
                      (delq nil
                            (mapcar
                             (lambda (x)
                               (unless (swint-iswitchb-ignore-buffername-p x) x))
                             ;; 只使用(persp-buffers persp-curr)产生的buffer list顺序不对，无法切换回之前的buffer。
                             (remove-if-not (lambda (x) (member x (remq nil (mapcar 'buffer-name (persp-buffers persp-curr)))))
                                            (mapcar 'buffer-name buffers)))))
              (if include-current
                  curr-buf-name))))
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
  (setq swint-iswitchb-buffer-ignore '("\\` " "\\`\\*sdcv\\*\\'" "\\`\\*Completions\\*\\'" "\\`\\*Compile\\-Log\\*\\'" "\\`\\*calculator\\*\\'" "\\`\\*Ibuffer\\*\\'" "\\`\\*Calendar\\*\\'" "\\`Enjoy\\ Music\\'" "\\`\\*helm.*\\*\\'" "\\`\\*Helm.*\\*\\'" "\\`\\*dirtree\\*\\'"))
  ;; =========关闭buffer后切换到之前的buffer=======
  (defun dirtree-shell-command ()
    "Open file with external app."
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
  (defun dirtree-open-and-kill-dirtree ()
    "Go to the position of buffer."
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
               (define-key dirtree-mode-map (kbd "q") 'kill-buffer-and-window)
               (define-key dirtree-mode-map (kbd "i") 'tree-mode-toggle-expand))))
;; ====================dirtree=====================
(provide 'setup_dirtree)
