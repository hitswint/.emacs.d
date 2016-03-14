;; ================Projectile================
(use-package projectile
  ;; Enabled at commands.
  :defer t
  :bind-keymap ("M-s '" . projectile-command-map)
  :init
  (setq projectile-keymap-prefix (kbd "M-s '"))
  :config
  (projectile-global-mode)
  (setq projectile-enable-caching t)
  (setq projectile-mode-line nil)
  (setq projectile-completion-system 'helm))
;; =================helm-projectile======================
(use-package helm-projectile
  ;; Enabled at commands.
  :defer t
  :bind ("M-'" . helm-projectile)
  :config
  (helm-projectile-on)
  (helm-add-action-to-source "Projectile persp switch project" 'projectile-persp-switch-project helm-source-projectile-projects 0)
  (helm-projectile-define-key helm-projectile-projects-map (kbd "C-j") '(lambda (project)
                                                                          (let ((projectile-completion-system 'helm))
                                                                            (projectile-switch-project-by-name project))))
  ;; 设置切换project的默认操作。
  (setq projectile-switch-project-action 'helm-projectile)
  ;; windows下的缓存方式从native改到alien，加快缓存速度。
  (when is-win
    (setq projectile-indexing-method 'alien)))
;; =================helm-projectile======================
;; ================persp-projectile======================
(use-package persp-projectile
  ;; Enabled at commands.
  :defer t
  :after projectile
  :config
  (bind-key "'" 'projectile-persp-switch-project projectile-command-map))
;; ================persp-projectile======================
;; projectile-grep出现find错误。
;; 在helm-projectile中C-d为打开project的根目录。
;; C-c p f         Display a list of all files in the project. With a prefix argument it will clear the cache first.
;; C-c p F         Display a list of all files in all known projects.
;; C-c p 4 f       Jump to a project's file using completion and show it in another window.
;; C-c p d         Display a list of all directories in the project. With a prefix argument it will clear the cache first.
;; C-c p 4 d       Switch to a project directory and show it in another window.
;; C-c p 4 a       Switch between files with the same name but different extensions in other window.
;; C-c p T         Display a list of all test files(specs, features, etc) in the project.
;; C-c p l         Display a list of all files in a directory (that's not necessarily a project)
;; C-c p s g       Run grep on the files in the project.
;; M-- C-c p s g   Run grep on projectile-grep-default-files in the project.
;; C-c p v         Run vc-dir on the root directory of the project.
;; C-c p b         Display a list of all project buffers currently open.
;; C-c p 4 b       Switch to a project buffer and show it in another window.
;; C-c p 4 C-o     Display a project buffer in another window without selecting it.
;; C-c p a         Switch between files with the same name but different extensions.
;; C-c p o         Runs multi-occur on all project buffers currently open.
;; C-c p r         Runs interactive query-replace on all files in the projects.
;; C-c p i         Invalidates the project cache (if existing).
;; C-c p R         Regenerates the projects TAGS file.
;; C-c p j         Find tag in project's TAGS file.
;; C-c p k         Kills all project buffers.
;; C-c p D         Opens the root of the project in dired.
;; C-c p e         Shows a list of recently visited project files.
;; C-c p s a       Runs ack on the project. Requires the presence of ack-and-a-half.
;; C-c p s s       Runs ag on the project. Requires the presence of ag.el.
;; C-c p !         Runs shell-command in the root directory of the project.
;; C-c p &         Runs async-shell-command in the root directory of the project.
;; C-c p c         Runs a standard compilation command for your type of project.
;; C-c p P         Runs a standard test command for your type of project.
;; C-c p t         Toggle between an implementation file and its test file.
;; C-c p 4 t       Jump to implementation or test file in other window.
;; C-c p z         Adds the currently visited file to the cache.
;; C-c p p         Display a list of known projects you can switch to.
;; C-c p S         Save all project buffers.
;; C-c p m         Run the commander (an interface to run commands with a single key).
;; C-c p ESC       Switch to the most recently selected projectile buffer.
;; ================Projectile================
(provide 'setup_projectile)
