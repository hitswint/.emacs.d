;; =================================magit===============================
(when is-win
  ;; 使用下列cygwin-mount可以解决cygwin的路径问题，但是会导致其他mode中路径不识别，例如org无法显示图片。
  ;; (require 'cygwin-mount)
  ;; (cygwin-mount-activate)
  ;; 下面这个是针对magit的解决方法。
  (defadvice magit-expand-git-file-name
      (before magit-expand-git-file-name-cygwin activate)
    "Handle Cygwin directory names such as /cygdrive/c/*
by changing them to C:/*"
    (when (string-match "^/cygdrive/\\([a-z]\\)/\\(.*\\)" filename)
      (setq filename (concat (match-string 1 filename) ":/"
                             (match-string 2 filename))))))
;; Subtler highlight
(require 'magit)
(set-face-background 'diff-file-header "#121212")
(set-face-foreground 'diff-context "#666666")
(set-face-foreground 'diff-added "#00cc33")
(set-face-foreground 'diff-removed "#ff0000")
(set-default 'magit-stage-all-confirm nil)
(set-default 'magit-unstage-all-confirm nil)
(eval-after-load 'ediff
  '(progn
     (set-face-foreground 'ediff-odd-diff-B "#ffffff")
     (set-face-background 'ediff-odd-diff-B "#292521")
     (set-face-foreground 'ediff-even-diff-B "#ffffff")
     (set-face-background 'ediff-even-diff-B "#292527")
     (set-face-foreground 'ediff-odd-diff-A "#ffffff")
     (set-face-background 'ediff-odd-diff-A "#292521")
     (set-face-foreground 'ediff-even-diff-A "#ffffff")
     (set-face-background 'ediff-even-diff-A "#292527")))
;; git config --add magit.extension svn
(add-hook 'magit-mode-hook 'magit-load-config-extensions)
(defun magit-save-and-exit-commit-mode ()
  (interactive)
  (save-buffer)
  (server-edit)
  (delete-window))
(defun magit-exit-commit-mode ()
  (interactive)
  (kill-buffer)
  (delete-window))
(eval-after-load "git-commit-mode"
  '(define-key git-commit-mode-map (kbd "C-c C-k") 'magit-exit-commit-mode))
;; C-c C-a to amend without any prompt
(defun magit-just-amend ()
  (interactive)
  (save-window-excursion
    (magit-with-refresh
     (shell-command "git --no-pager commit --amend --reuse-message=HEAD"))))
(eval-after-load "magit"
  '(define-key magit-status-mode-map (kbd "C-c C-a") 'magit-just-amend))
;; C-x C-k to kill file on line
(defun magit-kill-file-on-line ()
  "Show file on current magit line and prompt for deletion."
  (interactive)
  (magit-visit-item)
  (delete-current-buffer-file)
  (magit-refresh))
(define-key magit-status-mode-map (kbd "C-x C-k") 'magit-kill-file-on-line)
;; full screen magit-status
(defadvice magit-status (around magit-fullscreen activate)
  (window-configuration-to-register :magit-fullscreen)
  ad-do-it
  (delete-other-windows))
;; full screen vc-annotate
(defun vc-annotate-quit ()
  "Restores the previous window configuration and kills the vc-annotate buffer"
  (interactive)
  (kill-buffer)
  (jump-to-register :vc-annotate-fullscreen))
(eval-after-load "vc-annotate"
  '(progn
     (defadvice vc-annotate (around fullscreen activate)
       (window-configuration-to-register :vc-annotate-fullscreen)
       ad-do-it
       (delete-other-windows))
     (define-key vc-annotate-mode-map (kbd "q") 'vc-annotate-quit)))
;; ignore whitespace
(defun magit-toggle-whitespace ()
  (interactive)
  (if (member "-w" magit-diff-options)
      (magit-dont-ignore-whitespace)
    (magit-ignore-whitespace)))
(defun magit-ignore-whitespace ()
  (interactive)
  (add-to-list 'magit-diff-options "-w")
  (magit-refresh))
(defun magit-dont-ignore-whitespace ()
  (interactive)
  (setq magit-diff-options (remove "-w" magit-diff-options))
  (magit-refresh))
(define-key magit-status-mode-map (kbd "W") 'magit-toggle-whitespace)
;; Don't bother me with flyspell keybindings
(eval-after-load "flyspell"
  '(define-key flyspell-mode-map (kbd "C-.") nil))
(global-set-key (kbd "C-c g") 'magit-status)
;; 在git add remote和git clone中需要使用cygwin的路径名称 file:///cygdrive/c/Users/swint/
;; 建立的远程仓库remote.git， 初始化时使用git --bare init，直接使用remote.git文件夹作为.git文件夹
;; ==================初始化远程库和克隆远程库===================
(defun magit-clone-remote ()
  (interactive)
  (cond
   (is-lin
    (shell-command (concat "git clone ~/Nutstore/" (buffer-name (window-buffer (next-window))))))
   (is-win
    (shell-command (concat "git clone file:///cygdrive/c/Users/swint/Nutstore/" (buffer-name (window-buffer (next-window))))))))
(defun magit-bare-init-remote ()
  (interactive)
  (cond
   (is-lin
    (shell-command (concat "git --bare init ~/Nutstore/"  (buffer-name) ".git")))
   (is-win
    (shell-command (concat "git --bare init /cygdrive/c/Users/swint/Nutstore/"  (buffer-name) ".git")))))
(global-set-key (kbd "C-c C-x ,") 'magit-clone-remote)
(global-set-key (kbd "C-c C-x .") 'magit-bare-init-remote)
;; clone操作需要打开两个窗口一个是目标位置一个是Nutstore中远程库
;; init操作不需要打开两个窗口，远程库自动生成在Nutstore中
(defun magit-add-remote (remote url)
  "Add the REMOTE and fetch it.
\('git remote add REMOTE URL')."
  (interactive (list (read-string "Remote name: " "origin")
                     (read-string "Remote url: "
                                  (cond
                                   (is-lin (concat "~/Nutstore/" (buffer-name (window-buffer (next-window)))))
                                   (is-win (read-string "Remote url: " (concat "file:///cygdrive/c/Users/swint/Nutstore/" (buffer-name (window-buffer (next-window))))))))))
  (magit-run-git-async "remote" "add" "-f" remote url))
;; 使magit-add-remote默认以另一个窗口的buffer为remote
;; ==================初始化远程库和克隆远程库===================
(setq magit-last-seen-setup-instructions "1.4.0")
;; ==========使用git管理doc文件=============
(defun swint-magit-diff-doc ()
  (interactive)
  (with-temp-file ".gitattributes"
    (insert (concat "*.doc diff=word" "\n" "*.docx diff=wordx")))
  (shell-command "git config diff.word.textconv catdoc")
  (shell-command "git config diff.wordx.textconv docx2txt-git"))
(define-key magit-status-mode-map (kbd "C-c d") 'swint-magit-diff-doc)
;; ==========使用git管理doc文件=============
;; =================================magit===============================
(provide 'setup_magit)
