;;; magit
;; ====================magit=======================
(def-package! magit
  :diminish magit-auto-revert-mode
  :bind (("C-x M-g" . magit-status)
         ("C-x C-M-g" . magit-dispatch)
         ("M-g M-<" . swint-magit-clone-nutstore)
         ("M-g M->" . swint-magit-remote-nutstore))
  :init
  (setq magit-auto-revert-mode nil)
  :config
  (dolist (hook '(magit-diff-mode-hook magit-status-mode-hook))
    (add-hook hook '(lambda ()
                      (highlight-parentheses-mode -1)
                      (auto-mark-mode -1))))
  ;; 去除默认显示staged。当有大量staged时，更新变慢。
  (remove-hook 'magit-status-sections-hook 'magit-insert-staged-changes)
  (define-key magit-mode-map (kbd "C-c s") 'magit-diff-staged)
  (define-key magit-mode-map (kbd "<C-tab>") nil)
  (define-key magit-file-mode-map "\C-xg" nil)
  (define-key magit-file-mode-map "\C-x\M-g" nil)
  (defun magit-exit-commit-mode ()
    (interactive)
    (swint-kill-buffer)
    (delete-window))
  (define-key git-commit-mode-map (kbd "C-c C-k") 'magit-exit-commit-mode)
  ;; C-c C-a to amend without any prompt.
  (defun magit-just-amend ()
    (interactive)
    (save-window-excursion
      (magit-with-refresh
       (shell-command "git --no-pager commit --amend --reuse-message=HEAD"))))
  (define-key magit-status-mode-map (kbd "C-c C-a") 'magit-just-amend)
;;; 初始化远程库和克隆远程库
  ;; ===========初始化远程库和克隆远程库===========
  ;; cygwin中使用git remote add/git clone的路径为file:///cygdrive/c/*。
  (defun swint-magit-clone-nutstore ()
    "从~/Nutstore中clone远程库到本地。"
    (interactive)
    (let* ((remote-repo-prefix "~/Nutstore/")
           (remote-repo (concat remote-repo-prefix
                                (completing-read "Remote repo to clone: "
                                                 (directory-files "~/Nutstore" nil ".+\\.git")))))
      (shell-command (concat "git clone " remote-repo))))
  ;; 先使用magit-status建立本地库，建立远程库时使用git --bare init初始化。
  (defun swint-magit-remote-nutstore ()
    "使用本地库名字在~/Nutstore中建立远程库，并加为remote repo。"
    (interactive)
    (let* ((remote-repo-prefix "~/Nutstore/")
           (remote-repo (concat remote-repo-prefix
                                (file-name-nondirectory (directory-file-name (magit-toplevel))) ".git")))
      (shell-command (concat "git --bare init " remote-repo))
      (magit-remote-add (read-string "Remote name: " "origin")
                        (read-string "Remote url: " remote-repo))))
  ;; ===========初始化远程库和克隆远程库===========
;;; 使用git管理doc文件
  ;; =============使用git管理doc文件===============
  (defun swint-magit-diff-doc ()
    (interactive)
    (with-temp-file ".gitattributes"
      (insert (concat "*.doc diff=word" "\n" "*.docx diff=wordx")))
    (shell-command "git config diff.word.textconv catdoc")
    (shell-command "git config diff.wordx.textconv pandoc\\ --to=plain")
    (with-temp-file ".gitignore"
      (insert (concat ".~*" "\n"))))
  (define-key magit-status-mode-map (kbd "C-c d") 'swint-magit-diff-doc)
  ;; =============使用git管理doc文件===============
  )
;; ====================magit=======================
(provide 'setup_magit)
