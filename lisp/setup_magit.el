;;; magit
;; ====================magit=======================
(use-package transient
  :commands transient-define-prefix)
(use-package magit
  :diminish magit-auto-revert-mode
  :commands magit-status
  :bind (("C-x C-M-g" . magit-dispatch)
         ("C-x d s" . magit-diff-staged-current)
         ("C-x d u" . magit-diff-unstaged-current))
  :init
  (setq magit-auto-revert-mode nil
        magit-define-global-key-bindings nil)
  (bind-key "C-x M-g" #'(lambda () (interactive)
                          (let ((default-directory (helm-current-directory)))
                            (call-interactively 'magit-status))))
  :config
  (setq magit-show-long-lines-warning nil)
  (dolist (hook '(magit-diff-mode-hook magit-status-mode-hook))
    (add-hook hook #'(lambda ()
                       (highlight-parentheses-mode -1)
                       (auto-mark-mode -1))))
  ;; magit-status中去除headers/staged，使用magit-show-refs(y)/magit-diff-staged(ds)/magit-diff-unstaged(du)
  (dolist (hook '(magit-insert-status-headers magit-insert-staged-changes))
    (remove-hook 'magit-status-sections-hook hook))
  (define-key magit-mode-map (kbd "<C-tab>") nil)
  (defun swint-magit-diff-doc ()
    (interactive)
    (with-temp-file ".gitattributes"
      (insert (concat "*.doc diff=word" "\n" "*.docx diff=wordx")))
    (shell-command "git config diff.word.textconv catdoc")
    (shell-command "git config diff.wordx.textconv pandoc\\ --to=plain")
    (with-temp-file ".gitignore"
      (insert (concat ".~*" "\n"))))
  (define-key magit-status-mode-map (kbd "C-c d") 'swint-magit-diff-doc)
  ;; <return>: magit-diff-visit-file
  ;; C-<return>: magit-diff-visit-worktree-file
  (define-key magit-diff-section-map (kbd "C-o") 'magit-diff-visit-file-other-window)
  (define-key magit-diff-section-map (kbd "C-j") 'magit-diff-visit-worktree-file-other-window)
  (defun magit-diff-staged-current (&optional rev args)
    "Show staged changes limited to the current directory."
    (interactive
     (list (and current-prefix-arg
                (magit-read-branch-or-commit "Diff index and commit"))
           (car (magit-diff-arguments))))
    (let* ((default-directory
            (file-name-as-directory (helm-current-directory)))
           (topdir (or (magit-toplevel)
                       (user-error "Not inside a Git repository")))
           (directory (file-relative-name default-directory topdir)))
      (magit-diff-staged rev args (list directory))))
  (defun magit-diff-unstaged-current (&optional args)
    "Show unstaged changes limited to the current directory."
    (interactive (list (car (magit-diff-arguments))))
    (let* ((default-directory
            (file-name-as-directory (helm-current-directory)))
           (topdir (or (magit-toplevel)
                       (user-error "Not inside a Git repository")))
           (directory (file-relative-name default-directory topdir)))
      (magit-diff-unstaged args (list directory)))))
;; ====================magit=======================
;;; vc
;; ======================vc========================
(use-package vc-git
  :commands vc-git-branches
  :init
  (setq vc-follow-symlinks t
        vc-handled-backends '(Git)
        vc-ignore-dir-regexp (format "\\(%s\\)\\|\\(%s\\)"
                                     vc-ignore-dir-regexp
                                     tramp-file-name-regexp)))
;; ======================vc========================
(provide 'setup_magit)
