;; ==================dirtree=========================
;; (add-to-list 'load-path "~/.emacs.d/dirtree")
(require 'tree-mode)
(require 'windata)
(require 'dirtree)
(autoload 'dirtree "dirtree" "Add directory to tree view" t)
;; 放弃使用键盘宏
;; (fset 'dirtree-local
;;       [?\C-c return return ?\c ?\C-a ?\M-f ?\M-b])
;; (fset 'dirtree-home
;;       [?\C-c return ?\C-a ?\C-f ?\C-f ?\C-k ?\C-m ?\C-n])
(global-set-key (kbd "C-c C-j") 'dirtree-local)
(global-set-key (kbd "C-c j") 'dirtree-home)
(defun dirtree-local ()
  (interactive)
  (let* ((file buffer-file-name)
         (dir (if file (file-name-directory file) default-directory))
         (dirtree-buffer-name (if (eq major-mode 'dired-mode) (file-name-nondirectory (dired-get-filename)) (if file (file-name-nondirectory file) nil))))
    (dirtree dir nil)
    (if dirtree-buffer-name
        (re-search-forward (concat " " dirtree-buffer-name) nil t))
    (beginning-of-line)))
(defun dirtree-home ()
  (interactive)
  (dirtree "~/" nil))
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
(defun dirtree-switch-to-dirtree ()
  "kill this buffer and switch to other windows"
  (interactive)
  (bc-set)
  (kill-this-buffer)
  (other-window 1))
(defun dirtree-exist-kill-this-buffer ()
  "switch to dirtree after killing this buffer"
  (interactive)
  (if (equal (buffer-name (other-buffer (current-buffer) t)) "*dirtree*")
      (dirtree-switch-to-dirtree)
    (progn
      (bc-set)
      (cond
       (is-lin
        ;; (kill-this-buffer)             ;卸载slim后失效
        (kill-buffer (current-buffer)))
       (is-win (kill-this-buffer)))
      (switch-to-buffer (car (swint-iswitchb-make-buflist nil)))
      (switch-to-buffer (car (swint-iswitchb-make-buflist nil)))
      ;; 下面不可行，如果当前buffer不显示在buflist中，那么会kill其他的buffer
      ;; (switch-to-buffer (car (swint-iswitchb-make-buflist nil)))
      ;; (kill-buffer (car (swint-iswitchb-make-buflist nil)))
      )
    ))
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
             (define-key dirtree-mode-map (kbd "i") 'tree-mode-toggle-expand)))
(global-set-key (kbd "C-q") 'dirtree-exist-kill-this-buffer)
;; ==================dirtree=========================
;; ==================关闭当前buffer之后切换到之前访问过的buffer=======================
;; 原始的关闭buffer存在两个问题：
;; 一是会切换到helm buffer中，二是在persp之间切换时会切换到上一个persp的buffer中。
(defvar swint-iswitchb-buflist nil
  "Stores the current list of buffers that will be searched through.
The list is ordered, so that the most recent buffers come first,
although by default, the buffers visible in the current frame are put
at the end of the list.  Created by `iswitchb-make-buflist'.")
(defun swint-iswitchb-make-buflist (default)
  "该函数使关闭当前buffer之后切换到之前访问过的buffer"
  (setq swint-iswitchb-buflist
        (let* ((iswitchb-current-buffers (iswitchb-get-buffers-in-frames))
               (iswitchb-temp-buflist
                (delq nil
                      (mapcar
                       (lambda (x)
                         (let ((b-name (buffer-name x)))
                           (if (not
                                (or
                                 (swint-iswitchb-ignore-buffername-p b-name)
                                 (memq b-name iswitchb-current-buffers)))
                               b-name)))
                       (buffer-list (and iswitchb-use-frame-buffer-list
                                         (selected-frame)))))))
          (setq iswitchb-temp-buflist
                (nconc iswitchb-temp-buflist iswitchb-current-buffers))
          (run-hooks 'iswitchb-make-buflist-hook)
          ;; Should this be after the hooks, or should the hooks be the
          ;; final thing to be run?
          (if default
              (progn
                (setq iswitchb-temp-buflist
                      (delete default iswitchb-temp-buflist))
                (setq iswitchb-temp-buflist
                      (cons default iswitchb-temp-buflist))))
          iswitchb-temp-buflist)))
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
  "因为iswitchb自身忽略了文件buffer，这里定义一个包括文件buffer的ignore list."
  :type '(repeat (choice regexp function))
  :group 'iswitchb)
(setq swint-iswitchb-buffer-ignore '("\\` " "\\`\\*Messages\\*\\'" "\\`\\*scratch\\*\\'" "\\`\\*sdcv\\*\\'" "\\`\\*Completions\\*\\'" "\\`\\*Compile\\-Log\\*\\'" "\\`\\*calculator\\*\\'" "\\`\\*Inferior\\ Octave\\*\\'" "\\`\\*Ibuffer\\*\\'" "\\`\\*shell\\*\\'" "\\`\\*Calendar\\*\\'" "\\`Enjoy\\ Music\\'" "\\`\\*MATLAB\\*\\'" "\\*.*\\*"))
;; ".*\\..+" 注释掉这句使其包括文件buffer
;; ==================关闭当前buffer之后切换到之前访问过的buffer=======================
(provide 'setup_dirtree)
