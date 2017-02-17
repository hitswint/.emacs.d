;;; magit
;; ====================magit=======================
(use-package magit
  ;; Enabled at commands.
  :defer t
  :bind ("C-x M-g" . magit-status)
  :config
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
  ;; Subtler highlight
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
    (dirtree-kill-this-buffer)
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
  ;; full screen magit-status
  (defadvice magit-status (around magit-fullscreen activate)
    (window-configuration-to-register :magit-fullscreen)
    ad-do-it
    (delete-other-windows))
  ;; full screen vc-annotate
  (defun vc-annotate-quit ()
    "Restores the previous window configuration and kills the vc-annotate buffer"
    (interactive)
    (dirtree-kill-this-buffer)
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
;;; 初始化远程库和克隆远程库
  ;; ===========初始化远程库和克隆远程库===========
  ;; 在git remote add和git clone中需要使用cygwin的路径名称 file:///cygdrive/c/Users/swint/ 。
  ;; 使用magit-status建立本地仓库，建立远程仓库时使用git --bare init初始化。
  (defun swint-magit-clone-nutstore ()
    "clone操作需要打开两个窗口一个是目标位置一个是Nutstore中远程库。"
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
  (setq magit-last-seen-setup-instructions "1.4.0")
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
;;; 使用webdav_sync同步文件
  ;; ===========使用webdav_sync同步文件============
  (defun swint-magit-pull-current (remote branch &optional args)
    "Fetch and merge into current branch."
    (interactive (magit-pull-read-args t))
    (let ((process (start-process-shell-command "webdav_sync" "*webdav_sync*" "java -Dderby.system.home=/home/swint/.webdav_sync/ -Dbe.re.http.no-compress -jar ~/.webdav_sync/webdav_sync1_1_6.jar -r -down -u https://wgq_713%40163.com:arxg55upvg9urwus@dav.jianguoyun.com/dav/Nutstore-git/ -d ~/Nutstore-git/")))
      (set-process-sentinel
       process
       (lambda (process signal)
         (when (memq (process-status process) '(exit signal))
           (let ((webdav_sync-process-output (with-current-buffer "*webdav_sync*"
                                               (buffer-substring-no-properties (- (point-max) 6) (point-max)))))
             (if (string-equal webdav_sync-process-output "Done.\n")
                 (progn (with-current-buffer (car (get-buffers-matching-mode 'magit-status-mode))
                          (magit-run-git-async "pull" (nth 2 (magit-pull-read-args t))
                                               (and (not (equal (nth 0 (magit-pull-read-args t)) (magit-get-remote)))
                                                    (not (equal (nth 1 (magit-pull-read-args t)) (magit-get-remote-branch)))
                                                    (list (nth 0 (magit-pull-read-args t)) (nth 1 (magit-pull-read-args t))))))
                        (message "swint-magit-pull-current done."))
               (message "swint-magit-pull-current failed"))))))))
  (defun swint-magit-push-current (branch remote &optional remote-branch args)
    "Push the current branch to its upstream branch.
If the upstream isn't set, then read the remote branch."
    (interactive (magit-push-read-args t t))
    (magit-push branch remote remote-branch args)
    (let ((process (start-process-shell-command "webdav_sync" "*webdav_sync*" "java -Dderby.system.home=/home/swint/.webdav_sync/ -Dbe.re.http.no-compress -jar ~/.webdav_sync/webdav_sync1_1_6.jar -r -up -u https://wgq_713%40163.com:arxg55upvg9urwus@dav.jianguoyun.com/dav/Nutstore-git/ -d ~/Nutstore-git/")))
      (set-process-sentinel
       process
       (lambda (process signal)
         (when (memq (process-status process) '(exit signal))
           (let ((webdav_sync-process-output (with-current-buffer "*webdav_sync*"
                                               (buffer-substring-no-properties (- (point-max) 6) (point-max)))))
             (if (string-equal webdav_sync-process-output "Done.\n")
                 (message "swint-magit-push-current done.")
               (message "swint-magit-push-current failed"))))))))
  ;; webdav_sync的同步方法不可靠，而且webdav的连接方式很慢。暂停使用。
  ;; (define-key magit-status-mode-map (kbd "C-c M-,") 'swint-magit-pull-current)
  ;; (define-key magit-status-mode-map (kbd "C-c M-.") 'swint-magit-push-current)
  ;; ===========使用webdav_sync同步文件============
  (add-hook 'magit-mode-hook (lambda ()
                               (define-key magit-mode-map (kbd "<C-tab>") nil))))
;; ====================magit=======================
(provide 'setup_magit)
