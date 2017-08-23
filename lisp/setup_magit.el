;;; magit
;; ====================magit=======================
(use-package magit
  ;; Enabled at commands.
  :defer t
  :bind ("C-x M-g" . magit-status)
  :config
  (define-key magit-mode-map (kbd "<C-tab>") nil)
  (defun magit-exit-commit-mode ()
    (interactive)
    (swint-kill-this-buffer)
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
  ;; cygwin中使用git remote add/git clone的路径为file:///cygdrive/c/Users/swint/。
  (defun swint-magit-clone-nutstore ()
    "从~/Nutstore中clone远程库到本地。"
    (interactive)
    (let* ((remote-repo-prefix (cond (is-lin "~/Nutstore/")
                                     (is-win "file:///cygdrive/c/Users/swint/Nutstore/")))
           (remote-repo (concat remote-repo-prefix
                                (completing-read "Remote repo to clone: "
                                                 (directory-files "~/Nutstore" nil ".+\\.git")))))
      (shell-command (concat "git clone " remote-repo))))
  ;; 先使用magit-status建立本地库，建立远程库时使用git --bare init初始化。
  (defun swint-magit-remote-nutstore ()
    "使用本地库名字在~/Nutstore中建立远程库，并加为remote repo。"
    (interactive)
    (let* ((remote-repo-prefix (cond (is-lin "~/Nutstore/")
                                     (is-win "file:///cygdrive/c/Users/swint/Nutstore/")))
           (remote-repo (concat remote-repo-prefix
                                (file-name-nondirectory (directory-file-name (magit-toplevel))) ".git")))
      (shell-command (concat "git --bare init " remote-repo))
      (magit-remote-add (read-string "Remote name: " "origin")
                        (read-string "Remote url: " remote-repo))))
  (global-set-key (kbd "M-g M-,") 'swint-magit-clone-nutstore)
  (global-set-key (kbd "M-g M-.") 'swint-magit-remote-nutstore)
  ;; ===========初始化远程库和克隆远程库===========
;;; 使用git管理doc文件
  ;; =============使用git管理doc文件===============
  (defun swint-magit-diff-doc ()
    (interactive)
    (with-temp-file ".gitattributes"
      (insert (concat "*.doc diff=word" "\n" "*.docx diff=wordx")))
    (shell-command "git config diff.word.textconv catdoc")
    (shell-command "git config diff.wordx.textconv docx2txt-git"))
  (define-key magit-status-mode-map (kbd "C-c d") 'swint-magit-diff-doc)
  ;; =============使用git管理doc文件===============
;;; Git under Cygwin
  ;; ==============Git under Cygwin================
  ;; (when is-win
  ;;   ;; 使用下列cygwin-mount可以解决cygwin的路径问题，但是会导致其他mode中路径不识别，例如org无法显示图片。
  ;;   ;; (require 'cygwin-mount)
  ;;   ;; (cygwin-mount-activate)
  ;;   ;; 下面这个是针对magit的解决方法，官方已解决。
  ;;   ;; (defadvice magit-expand-git-file-name
  ;;   ;;       (before magit-expand-git-file-name-cygwin activate)
  ;;   ;;     "Handle Cygwin directory names such as /cygdrive/c/*
  ;;   ;; by changing them to C:/*"
  ;;   ;;     (when (string-match "^/cygdrive/\\([a-z]\\)/\\(.*\\)" filename)
  ;;   ;;       (setq filename (concat (match-string 1 filename) ":/"
  ;;   ;;                              (match-string 2 filename)))))
  ;;   ;; 解决win上git问题，但推送时，会出现"Not inside a Git repository"的错误。
  ;;   ;; 1. magit-toplevel会导致错误路径(c:/cygdrive/c/...)，官方已解决。
  ;;   ;; (defun magit-toplevel (&optional file strict)
  ;;   ;;   (magit--with-safe-default-directory file
  ;;   ;;     (-if-let (cdup (magit-rev-parse-safe "--show-cdup"))
  ;;   ;;         (magit-expand-git-file-name
  ;;   ;;          (file-name-as-directory (expand-file-name cdup)))
  ;;   ;;       (unless strict
  ;;   ;;         (-when-let (gitdir (magit-git-dir))
  ;;   ;;           (if (magit-bare-repo-p)
  ;;   ;;               gitdir
  ;;   ;;             (file-name-directory (directory-file-name gitdir))))))))
  ;;   ;; 2. ^{commint}会导致magit-insert-head-header错误，官方已解决。
  ;;   ;; (defun magit-process-git-arguments (args)
  ;;   ;;   (setq args (-flatten args))
  ;;   ;;   (when (and (eq system-type 'windows-nt)
  ;;   ;;              (let ((exec-path
  ;;   ;;                     (list (file-name-directory magit-git-executable))))
  ;;   ;;                (executable-find "cygpath.exe")))
  ;;   ;;     (setq args (--map (let* ((it (replace-regexp-in-string
  ;;   ;;                                   "{\\([0-9]+\\)}" "\\\\{\\1\\\\}" it))
  ;;   ;;                              (it (replace-regexp-in-string
  ;;   ;;                                   "\\^{commit}" "^\\\\{commit\\\\}" it)))
  ;;   ;;                         it)
  ;;   ;;                       args)))
  ;;   ;;   (append magit-git-global-arguments args))
  ;;   )
  ;; ==============Git under Cygwin================
  )
;; ====================magit=======================
(provide 'setup_magit)
