;;; Projectile
;; ==================Projectile=================
(use-package projectile
  :bind-keymap ("M-\"" . projectile-command-map)
  :init
  (setq projectile-mode-line-prefix " ")
  :config
  (require 'vc-git)
  (projectile-mode t)
  ;; (bind-key "M-\"" 'projectile-command-map projectile-mode-map)
  (setq projectile-enable-caching t)
  (setq projectile-completion-system 'helm)
  (setq projectile-mode-line-function 'swint-projectile-default-mode-line)
  (defun swint-projectile-default-mode-line ()
    "Report project name and type in the modeline."
    (let ((project-name (projectile-project-name))
          (current-branch (car (vc-git-branches))))
      (format "%s[%s%s]"
              projectile-mode-line-prefix
              (or (truncate-string-to-width project-name 32) "-")
              (if current-branch
                  (format ":%s" (substring current-branch 0 (min 3 (length current-branch))))
                ""))))
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
  (helm-projectile-define-key helm-projectile-projects-map (kbd "C-s") #'(lambda (project) (let ((helm-truncate-lines t))
                                                                                             (helm-do-ag project))))
  (helm-projectile-define-key helm-projectile-projects-map (kbd "C-j") #'(lambda (project) (let ((projectile-completion-system 'helm))
                                                                                             (projectile-switch-project-by-name project))))
  (helm-projectile-define-key helm-projectile-projects-map (kbd "C-x j") #'(lambda (project) (neotree-dir project)))
  (helm-projectile-define-key helm-projectile-projects-map (kbd "C-M-k") #'(lambda (project) (helm-projectile-kill-persp)))
  (defvar helm-source-projectile-projects-current
    (helm-build-sync-source "Projectile projects current"
      :candidates (lambda ()
                    (list (abbreviate-file-name (projectile-project-root))))
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
  (defun helm-projectile/override (&optional arg)
    (interactive "P")
    (with-persp-mode-on
     (if (not (projectile-project-p))
         (setq helm-projectile-sources-list '(helm-source-projectile-projects-with-persp
                                              helm-source-projectile-projects-without-persp))
       (projectile-maybe-invalidate-cache arg)
       (setq helm-projectile-sources-list '(helm-source-projectile-projects-current
                                            helm-source-projectile-projects-with-persp
                                            helm-source-projectile-buffers-list
                                            ;; helm-source-projectile-files-list
                                            helm-source-projectile-projects-without-persp)))
     (let ((helm-ff-transformer-show-only-basename nil))
       (helm :sources helm-projectile-sources-list
             :buffer "*helm projectile-swint*"
             :truncate-lines helm-projectile-truncate-lines
             :prompt (projectile-prepend-project-name (if (projectile-project-p)
                                                          "pattern: "
                                                        "Switch to project: "))))))
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
