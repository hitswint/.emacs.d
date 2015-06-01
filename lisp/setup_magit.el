;; =================================magit===============================
;; Subtler highlight
(require 'magit)
(set-face-background 'magit-item-highlight "#121212")
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
;; todo:
;; diff-added-face diff-changed-face
;; diff-context-face diff-file-header-face
;; diff-function-face diff-header-face
;; diff-hunk-header-face diff-index-face
;; diff-indicator-added-face diff-indicator-changed-face
;; diff-indicator-removed-face diff-nonexistent-face
;; diff-removed-face
;; Load git configurations
;; For instance, to run magit-svn-mode in a project, do:
;;
;; git config --add magit.extension svn
;;
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
(defun magit-quit-session ()
  "Restores the previous window configuration and kills the magit buffer"
  (interactive)
  (kill-buffer)
  (jump-to-register :magit-fullscreen))
(define-key magit-status-mode-map (kbd "q") 'magit-quit-session)
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
;; Show blame for current line
;; (require-package 'git-messenger)
;; (global-set-key (kbd "C-x v p") #'git-messenger:popup-message)
;; Don't bother me with flyspell keybindings
(eval-after-load "flyspell"
  '(define-key flyspell-mode-map (kbd "C-.") nil))
(global-set-key (kbd "C-c g") 'magit-status)
;; ==================初始化远程库和克隆远程库===================
(defun magit-clone-remote ()
  (interactive)
  (shell-command (concat "git clone ~/Nutstore/" (buffer-name (window-buffer (next-window))))))
(defun magit-bare-init-remote ()
  (interactive)
  (shell-command (concat "git --bare init ~/Nutstore/"  (buffer-name) ".git")))
(global-set-key (kbd "C-c C-x ,") 'magit-clone-remote)
(global-set-key (kbd "C-c C-x .") 'magit-bare-init-remote)
;; clone操作需要打开两个窗口一个是目标位置一个是Nutstore中远程库
;; init操作不需要打开两个窗口，远程库自动生成在Nutstore中
(defun magit-add-remote (remote url)
  "Add the REMOTE and fetch it.
\('git remote add REMOTE URL')."
  (interactive (list (read-string "Remote name: " "origin")
                     (read-string "Remote url: " (concat "~/Nutstore/" (buffer-name (window-buffer (next-window)))))))
  (magit-run-git-async "remote" "add" "-f" remote url))
;; 使magit-add-remote默认以另一个窗口的buffer为remote
;; ==================初始化远程库和克隆远程库===================
(setq magit-last-seen-setup-instructions "1.4.0")
;; =================================magit===============================
(provide 'setup_magit)
