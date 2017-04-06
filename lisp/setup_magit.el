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
  ;; 在git remote add和git clone中需要使用cygwin的路径名称 file:///cygdrive/c/Users/swint/ 。
  ;; 使用magit-status建立本地仓库，建立远程仓库时使用git --bare init初始化。
  (defun swint-magit-clone-nutstore ()
    "Clone操作需要打开两个窗口一个是目标位置一个是Nutstore中远程库。"
    (interactive)
    (cond
     (is-lin
      (shell-command (concat "git clone ~/Nutstore/" (buffer-name (window-buffer (next-window))))))
     (is-win
      (shell-command (concat "git clone file:///cygdrive/c/Users/swint/Nutstore/" (buffer-name (window-buffer (next-window))))))))
  (defun swint-magit-remote-nutstore ()
    "使用本地库名字在~/Nutstore中建立远程库，并加为remote repo。"
    (interactive)
    (let ((remote-repository-name (concat (file-name-nondirectory (directory-file-name (magit-toplevel))) ".git")))
      (shell-command (concat "git --bare init" (cond (is-lin " ~/Nutstore/")
                                                     (is-win " /cygdrive/c/Users/swint/Nutstore/"))
                             remote-repository-name))
      (magit-remote-add (read-string "Remote name: " "origin")
                        (read-string "Remote url: " (cond
                                                     (is-lin (concat "~/Nutstore/" remote-repository-name))
                                                     (is-win (concat "file:///cygdrive/c/Users/swint/Nutstore/" remote-repository-name)))))))
  (global-set-key (kbd "M-g ,") 'swint-magit-clone-nutstore)
  (global-set-key (kbd "M-g .") 'swint-magit-remote-nutstore)
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
