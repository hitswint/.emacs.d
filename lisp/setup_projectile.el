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
  :after projectile
  :bind ("M-'" . helm-projectile)
  :config
  (helm-projectile-on)
  (helm-add-action-to-source "Projectile persp switch project" 'projectile-persp-switch-project helm-source-projectile-projects 0)
  (helm-projectile-define-key helm-projectile-projects-map (kbd "C-j") '(lambda (project)
                                                                          (let ((projectile-completion-system 'helm))
                                                                            (projectile-switch-project-by-name project))))
  ;; 设置切换project的默认操作。
  ;; 在helm-projectile中C-d为打开project的根目录。
  (setq projectile-switch-project-action 'helm-projectile))
;; ==============helm-projectile================
;;; persp-projectile
;; ================persp-projectile=============
(use-package persp-projectile
  ;; Enabled after features.
  :defer t
  :after projectile
  :bind ("M-s M-'" . projectile-persp-switch-project)
  :config
  (bind-key "'" 'projectile-persp-switch-project projectile-command-map))
;; ================persp-projectile=============
(provide 'setup_projectile)
