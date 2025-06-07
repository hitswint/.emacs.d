;;; Projectile
;; ==================Projectile=================
(use-package projectile
  :delight '(:eval (propertize (funcall projectile-mode-line-function) 'face (if (mode-line-window-selected-p) 'font-lock-keyword-face 'ml/inactive-foreground-bold)))
  :bind-keymap ("M-\"" . projectile-command-map)
  :init
  (setq projectile-mode-line-prefix " "
        projectile-ignored-project-function (lambda (root)
                                              (or (file-remote-p root)
                                                  (string-prefix-p (expand-file-name trash-directory) root)
                                                  (string-prefix-p "/mnt/share" root)
                                                  (string-prefix-p "/mnt/sshfs" root)
                                                  (string-prefix-p "/mnt/usb" root))))
  :config
  (projectile-mode t)
  (remove-hook 'buffer-list-update-hook #'projectile-track-known-projects-find-file-hook)
  ;; (bind-key "M-\"" 'projectile-command-map projectile-mode-map)
  (setq projectile-indexing-method 'alien
        projectile-enable-caching nil
        projectile-auto-update-cache nil
        projectile-completion-system 'helm
        projectile-mode-line-function 'swint-projectile-default-mode-line
        projectile-track-known-projects-automatically t)
  (defun swint-projectile-default-mode-line ()
    "Report project name and type in the modeline."
    (or (unless (string= projectile--mode-line
                         projectile-mode-line-prefix)
          projectile--mode-line)
        (let ((mode-line (if-let ((project-root (unless (funcall projectile-ignored-project-function default-directory)
                                                  (projectile-project-root))))
                             (concat projectile-mode-line-prefix
                                     "["
                                     (truncate-string-to-width (funcall projectile-project-name-function project-root) 16)
                                     (when-let* ((current-branch  (car (vc-git-branches)))  ;git branch --show-current
                                                 (branch-p (not (equal current-branch "master"))))
                                       (format ":%s" (substring current-branch 0 (min 3 (length current-branch)))))
                                     "]")
                           "")))
          (when (file-remote-p default-directory)
            (setq projectile--mode-line mode-line)
            (force-mode-line-update))
          mode-line)))
  (add-hook 'dired-after-readin-hook 'projectile-update-mode-line)
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (projectile-update-mode-line))))
;; ==================Projectile=================
;;; helm-projectile
;; ==============helm-projectile================
(use-package helm-projectile
  :bind ("M-'" . helm-projectile)
  :config
  (helm-projectile-on)
  (bind-key "M-'" #'(lambda () (interactive)
                      (with-helm-alive-p
                        (helm-run-after-exit 'helm-projectile helm-ff-default-directory)))
            helm-find-files-map)
  (bind-key "M-'" #'(lambda () (interactive)
                      (with-helm-alive-p
                        (helm-exit-and-execute-action
                         (lambda (_candidate)
                           (helm-projectile (buffer-local-value 'default-directory _candidate))))))
            helm-buffer-map)
  (bind-key "M-'" #'(lambda () (interactive)
                      (with-helm-alive-p
                        (helm-exit-and-execute-action
                         (lambda (_candidate)
                           (when-let ((bf (bookmark-get-filename _candidate)))
                             (helm-projectile (if (directory-name-p bf)
                                                  bf
                                                (file-name-directory bf))))))))
            helm-bookmark-map)
  ;; 设置切换project的默认操作，切换后可C-tab切换回helm-projectile
  ;; helm-projectile-find-file只能切换一次，projectile-find-file会切换persp
  (setq projectile-switch-project-action 'helm-projectile-find-file-dwim)
  (defun helm-projectile-kill-persp ()
    "Kill selected persps for projects."
    (let ((projects (helm-marked-candidates :with-wildcard t)))
      (with-helm-display-marked-candidates
        helm-marked-buffer-name
        projects
        (cl-loop for p in projects do
                 (persp-kill (file-basename p))
                 (remhash p persp-projectile-hash)))))
  (helm-projectile-define-key helm-projectile-projects-map (kbd "C-s") #'(lambda (project) (helm-do-ag project)))
  (helm-projectile-define-key helm-projectile-projects-map (kbd "C-j") #'(lambda (project) (let ((helm-ff-default-directory project))
                                                                                             (projectile-switch-project-by-name project))))
  (helm-projectile-define-key helm-projectile-projects-map (kbd "C-x j") #'(lambda (project) (neotree-dir project)))
  (helm-projectile-define-key helm-projectile-projects-map (kbd "C-M-k") #'(lambda (project) (if (member project (hash-table-keys persp-projectile-hash))
                                                                                                 (helm-projectile-kill-persp)
                                                                                               (helm-projectile-remove-known-project nil))))
  (helm-projectile-define-key helm-projectile-projects-map (kbd "C-M-j") #'(lambda (project) (let ((helm-ff-transformer-show-only-basename nil)
                                                                                                   (default-directory project))
                                                                                               (helm :sources '(helm-source-projectile-buffers-list
                                                                                                                helm-source-projectile-recentf-list)
                                                                                                     :buffer "*helm projectile buffers-swint*"
                                                                                                     :truncate-lines helm-projectile-truncate-lines))))
  (defvar helm-source-projectile-projects-current
    (helm-build-sync-source "Projectile projects current"
      :candidates (lambda ()
                    (ignore-errors (list (abbreviate-file-name (projectile-project-root)))))
      :fuzzy-match helm-projectile-fuzzy-match
      :keymap helm-projectile-projects-map
      :mode-line helm-read-file-name-mode-line-string
      :action 'helm-source-projectile-projects-actions)
    "Helm source for known projectile projects.")
  (defvar helm-source-projectile-projects-with-persp
    (helm-build-sync-source "Projectile projects with persp"
      :candidates (lambda ()
                    (cl-remove-if-not
                     (lambda (x)
                       (member x (hash-table-keys persp-projectile-hash)))
                     (projectile-relevant-known-projects)))
      :fuzzy-match helm-projectile-fuzzy-match
      :keymap helm-projectile-projects-map
      :mode-line helm-read-file-name-mode-line-string
      :action 'helm-source-projectile-projects-actions)
    "Helm source for known projectile projects.")
  (defvar helm-source-projectile-projects-without-persp
    (helm-build-sync-source "Projectile projects without persp"
      :candidates (lambda ()
                    (cl-remove-if
                     (lambda (x)
                       (member x (hash-table-keys persp-projectile-hash)))
                     projectile-known-projects))
      :fuzzy-match helm-projectile-fuzzy-match
      :keymap helm-projectile-projects-map
      :mode-line helm-read-file-name-mode-line-string
      :action 'helm-source-projectile-projects-actions)
    "Helm source for known projectile projects.")
  (helm-add-action-to-source "Projectile persp switch project"
                             'projectile-persp-switch-project helm-source-projectile-projects-current 0)
  (helm-add-action-to-source "Projectile persp switch project"
                             'projectile-persp-switch-project helm-source-projectile-projects-with-persp 0)
  (helm-add-action-to-source "Projectile persp switch project"
                             'projectile-persp-switch-project helm-source-projectile-projects-without-persp 0)
  (defun helm-projectile/override (&optional dir)
    (interactive)
    (let ((default-directory (or dir (helm-current-directory))))
      (with-persp-mode-on
       (projectile-maybe-invalidate-cache (or current-prefix-arg helm-current-prefix-arg))
       (setq helm-projectile-sources-list '(helm-source-projectile-projects-current
                                            helm-source-projectile-projects-with-persp
                                            helm-source-projectile-projects-without-persp))
       (let ((helm-ff-transformer-show-only-basename nil))
         (helm :sources helm-projectile-sources-list
               :buffer "*helm projectile-swint*"
               :truncate-lines helm-projectile-truncate-lines)))))
  (advice-add 'helm-projectile :override #'helm-projectile/override))
;; ==============helm-projectile================
;;; persp-projectile
;; ================persp-projectile=============
(use-package persp-projectile
  :load-path "site-lisp/persp-projectile-20180616.1944/"
  :commands projectile-persp-switch-project
  :init
  (bind-key "M-s M-'" 'projectile-persp-switch-project)
  (defvar persp-projectile-hash (make-hash-table :test 'equal))
  :config
  (defun projectile-persp-switch-project/after (project-to-switch)
    (projectile-add-known-project project-to-switch)
    ;; 将project-to-switch加入persp-projectile-hash中
    (puthash project-to-switch
             (file-name-nondirectory (directory-file-name project-to-switch))
             persp-projectile-hash)
    ;; 删除不在projectile-known-projects中的project
    (cl-loop for p in (hash-table-keys persp-projectile-hash)
             do (unless (member p projectile-known-projects)
                  (remhash p persp-projectile-hash))))
  (advice-add 'projectile-persp-switch-project :after
              #'projectile-persp-switch-project/after))
;; ================persp-projectile=============
(provide 'setup_projectile)
