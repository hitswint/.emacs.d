;;; Projectile
;; ==================Projectile=================
(use-package projectile
  ;; Enabled at commands.
  :defer t
  :bind-keymap ("M-\"" . projectile-command-map)
  :init
  (setq projectile-keymap-prefix (kbd "M-\""))
  :config
  (projectile-global-mode)
  (setq projectile-enable-caching t)
  (setq projectile-mode-line nil)
  (setq projectile-completion-system 'helm))
;; ==================Projectile=================
;;; helm-projectile
;; ==============helm-projectile================
(use-package helm-projectile
  ;; Enabled at commands.
  :defer t
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
                (persp-kill (file-basename p)))
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
                              (remove-if-not
                               (lambda (x)
                                 (member (file-basename x) (persp-names)))
                               (projectile-relevant-known-projects)))
                      (remove-if-not
                       (lambda (x)
                         (member (file-basename x) (persp-names)))
                       projectile-known-projects)))
      :fuzzy-match helm-projectile-fuzzy-match
      :keymap helm-projectile-projects-map
      :mode-line helm-read-file-name-mode-line-string
      :action 'helm-source-projectile-projects-actions)
    "Helm source for known projectile projects.")
  (defvar helm-source-projectile-projects-without-persp
    (helm-build-sync-source "Projectile projects without persp"
      :candidates (lambda ()
                    (remove-if
                     (lambda (x)
                       (member (file-basename x) (persp-names)))
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
  (helm-add-action-to-source "Projectile persp switch project" 'projectile-persp-switch-project helm-source-projectile-projects-with-persp 0)
  (helm-add-action-to-source "Projectile persp switch project" 'projectile-persp-switch-project helm-source-projectile-projects-without-persp 0)
  ;; 设置切换project的默认操作。
  (setq projectile-switch-project-action 'helm-projectile))
;; ==============helm-projectile================
;;; persp-projectile
;; ================persp-projectile=============
(use-package persp-projectile
  ;; Enabled after features.
  :defer t
  :after projectile
  :config
  (bind-key "M-s M-'" 'projectile-persp-switch-project))
;; ================persp-projectile=============
(provide 'setup_projectile)
