;;; Projectile
;; ==================Projectile=================
(def-package! projectile
  :bind-keymap ("M-\"" . projectile-command-map)
  :init
  (setq projectile-keymap-prefix (kbd "M-\""))
  :config
  (projectile-mode t)
  (setq projectile-enable-caching t)
  (setq projectile-mode-line nil)
  (setq projectile-completion-system 'helm))
;; ==================Projectile=================
;;; helm-projectile
;; ==============helm-projectile================
(def-package! helm-projectile
  :bind ("M-'" . helm-projectile)
  :config
  (helm-projectile-on)
  (defun helm-projectile-kill-persp (_ignore)
    "Kill selected persps for projects."
    (let* ((projects (helm-marked-candidates :with-wildcard t)))
      (with-helm-display-marked-candidates
        helm-marked-buffer-name
        projects
        (mapc (lambda (p)
                (persp-kill (file-basename p))
                (remhash p persp-projectile-hash))
              projects))))
  (helm-projectile-define-key helm-projectile-projects-map (kbd "C-j") '(lambda (project)
                                                                          (let ((projectile-completion-system 'helm))
                                                                            (projectile-switch-project-by-name project))))
  (helm-projectile-define-key helm-projectile-projects-map (kbd "C-x j") '(lambda (project) (neotree-dir project)))
  (helm-projectile-define-key helm-projectile-projects-map (kbd "C-s") 'helm-projectile-ag)
  (helm-projectile-define-key helm-projectile-projects-map (kbd "C-M-k") 'helm-projectile-kill-persp)
  (defvar helm-source-projectile-projects-with-persp
    (helm-build-sync-source "Projectile projects with persp"
      :candidates (lambda ()
                    (if (projectile-project-p)
                        (cons (abbreviate-file-name (projectile-project-root))
                              (cl-remove-if-not
                               (lambda (x)
                                 (member x (hash-table-keys persp-projectile-hash)))
                               (projectile-relevant-known-projects)))
                      (cl-remove-if-not
                       (lambda (x)
                         (member x (hash-table-keys persp-projectile-hash)))
                       projectile-known-projects)))
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
  (setq helm-projectile-sources-list '(helm-source-projectile-projects-with-persp
                                       helm-source-projectile-projects-without-persp
                                       helm-source-projectile-files-list
                                       helm-source-projectile-buffers-list))
  (helm-add-action-to-source "Projectile persp switch project"
                             'projectile-persp-switch-project helm-source-projectile-projects-with-persp 0)
  (helm-add-action-to-source "Projectile persp switch project"
                             'projectile-persp-switch-project helm-source-projectile-projects-without-persp 0)
  ;; 设置切换project的默认操作。
  (setq projectile-switch-project-action 'helm-projectile))
;; ==============helm-projectile================
;;; persp-projectile
;; ================persp-projectile=============
(def-package! persp-projectile
  :after helm-projectile
  :config
  (bind-key "M-s M-'" 'projectile-persp-switch-project)
  (defun projectile-persp-switch-project-update-hash (project-to-switch)
    (projectile-add-known-project project-to-switch)
    (unless (bound-and-true-p persp-projectile-hash)
      (setq persp-projectile-hash (make-hash-table :test 'equal)))
    ;; 将project-to-switch加入persp-projectile-hash中。
    (puthash project-to-switch
             (file-name-nondirectory (directory-file-name project-to-switch))
             persp-projectile-hash)
    ;; 删除不在projectile-known-projects中的project。
    (mapc #'(lambda (x) (remhash x persp-projectile-hash))
          (cl-remove-if (lambda (x)
                          (member x projectile-known-projects))
                        (hash-table-keys persp-projectile-hash))))
  (advice-add 'projectile-persp-switch-project :after
              #'projectile-persp-switch-project-update-hash))
;; ================persp-projectile=============
(provide 'setup_projectile)
