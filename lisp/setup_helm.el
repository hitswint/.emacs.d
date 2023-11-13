;;; helm
;; ====================helm=====================
(use-package helm
  :diminish (helm-mode eldoc-mode)
  :commands (helm-find-files-1
             helm-insert-latex-math
             helm-completing-read-default-1
             helm-current-directory
             helm-select-host)
  :bind-keymap ("C-x c" . helm-command-map)
  :bind (("C-'" . helm-bookmarks)
         ("C-," . swint-helm-file-buffers-list)
         ("C-." . swint-helm-dired-buffers-list)
         ("C-x C-f" . helm-find-files)
         ("C-x F" . helm-find)
         ("M-x" . helm-M-x)
         ("C-x l" . swint-helm-locate)
         ("C-x y" . helm-resume))
  :config
  (use-package helm-for-files)
  (helm-mode 1)
  (helm-top-poll-mode 1)
  (setq helm-move-to-line-cycle-in-source nil
        helm-buffer-details-flag nil
        helm-ff--RET-disabled t
        helm-ff-newfile-prompt-p nil
        ;; (add-to-list 'display-buffer-alist '("^\\*helm .*" (display-buffer-at-bottom))) ;在底部打开helm
        helm-split-window-default-side 'same
        helm-external-programs-associations file-extension-app-alist
        helm-default-external-file-browser "thunar"
        helm-pdfgrep-default-read-command "llpp -page %p \"%f\""
        helm-completing-read-handlers-alist '((describe-function . helm-completing-read-symbols)
                                              (describe-variable . helm-completing-read-symbols)
                                              (debug-on-entry . helm-completing-read-symbols)
                                              (find-function . helm-completing-read-symbols)
                                              (find-tag . helm-completing-read-with-cands-in-buffer)
                                              (ffap-alternate-file)
                                              (tmm-menubar)
                                              (find-file)
                                              (org-annotate-file)
                                              (swint-org-annotate-file)
                                              (dired-do-copy)
                                              (dired-create-directory))
        helm-boring-buffer-regexp-list (append helm-boring-buffer-regexp-list
                                               '("\\`Enjoy\\ Music\\'"
                                                 "\\`\\*Ibuffer\\*\\'"
                                                 "\\`\\*calculator\\*\\'"
                                                 "\\`\\*Calendar\\*\\'"
                                                 "\\`\\*Process\\ List\\*\\'"
                                                 "\\`\\*toc\\*\\'"
                                                 "\\`\\*buffer-selection\\*\\'"
                                                 "\\`\\*Disabled\\ Command\\*\\'"
                                                 "\\`\\*Mingus\\*\\'"
                                                 "\\`\\*Ido\\ Completions\\*\\'"
                                                 "\\`.english-words\\'"
                                                 "\\`\\*Help\\*\\'"
                                                 "\\`\\*tramp.*\\*\\'"
                                                 "\\`\\*baidu-translate\\*\\'"
                                                 "\\`\\*lingva\\*\\'"
                                                 "\\`\\*org-brain-helm\\*\\'"
                                                 "\\`\\*Org\\ Help\\*\\'"
                                                 "\\`\\*Org\\ Preview\\ LaTeX\\ Output\\*\\'"
                                                 "\\`magit-diff:"
                                                 "\\`\\*lsp-bridge"
                                                 "\\`\\*plot-data\\*\\'"
                                                 "\\`\\*Async\\ Shell\\ Command\\*\\'"
                                                 "\\`\\*Shell\\ Command\\ Output\\*\\'"
                                                 "\\`\\*Ebib-.*\\*\\'"
                                                 "\\`\\*thunar\\*\\'"
                                                 "\\`\\*tc\\*\\'"
                                                 "\\`\\*viatc\\*\\'"
                                                 "\\`\\*Article\\*\\'"
                                                 "\\`\\*xwidget-webkit:.*\\*\\'")))
  ;; (setq helm-mounted-network-directories '("/mnt/share" "/mnt/sshfs"))
  (custom-set-faces '(helm-buffer-directory ((t (:foreground "yellow" :weight bold))))
                    '(helm-buffer-file ((t (:inherit font-lock-type-face))))
                    '(helm-ff-directory ((t (:foreground "yellow" :weight bold))))
                    '(helm-ff-dotted-directory ((t (:foreground "yellow" :weight bold))))
                    '(helm-ff-file ((t (:foreground "white"))))
                    '(helm-grep-file ((t (:foreground "cyan"))))
                    '(helm-selection ((t (:background "black" :underline t))))
                    '(helm-visible-mark ((t (:foreground "DeepSkyBlue1")))))
;;;; keybindings
  ;; ===============keybindings=================
  (define-key helm-map (kbd "C-;") 'helm-toggle-visible-mark)
  (define-key helm-map (kbd "C-l") 'helm-execute-persistent-action)
  (define-key helm-map (kbd "M-o") 'helm-peep-preview-persistent)
  (define-key helm-map (kbd "C-M-p") 'helm-previous-source)
  (define-key helm-map (kbd "C-M-n") 'helm-next-source)
  (define-key helm-map (kbd "C-,") 'swint-helm-file-buffers-after-quit)
  (define-key helm-map (kbd "C-.") 'swint-helm-dired-buffers-after-quit)
  (define-key helm-map (kbd "C-'") 'swint-helm-bookmarks-after-quit)
  (define-key helm-map (kbd "M-'") 'swint-helm-projectile-after-quit)
  (define-key helm-map (kbd "M-RET") 'helm-quit-and-find-file)
  (define-key helm-map (kbd "<C-tab>") 'helm-resume-previous-session-after-quit)
  (define-key helm-map (kbd "<C-S-iso-lefttab>") 'helm-swap-windows)
  (define-key helm-map (kbd "C-\\") 'helm-toggle-resplit-and-swap-windows)
  (define-key helm-map (kbd "C-{") 'helm-narrow-window)
  (define-key helm-map (kbd "C-}") 'helm-enlarge-window)
  (define-key helm-map (kbd "M-,") 'helm-toggle-full-frame)
  (define-key helm-map (kbd "C-x y") 'helm-resume-list-buffers-after-quit)
  (define-key helm-map (kbd "C-c C-k") 'helm-kill-selection-and-quit)
  (define-key helm-map (kbd "C-c C-i") 'helm-insert-or-copy)
  (helm-define-key-with-subkeys helm-map (kbd "C-x Y") ?Y 'helm-run-cycle-resume)
  (helm-define-key-with-subkeys global-map (kbd "C-x Y") ?Y 'helm-cycle-resume)
  (define-key helm-find-files-map (kbd "C-l") 'helm-execute-persistent-action)
  (define-key helm-find-files-map (kbd "M-o") 'helm-peep-preview-persistent)
  (define-key helm-find-files-map (kbd "C-h") 'helm-find-files-up-one-level)
  (define-key helm-find-files-map (kbd "C-o") 'helm-ff-run-switch-other-window)
  (define-key helm-find-files-map (kbd "C-M-j") 'helm-ff-run-open-file-with-lister)
  (define-key helm-find-files-map (kbd "C-j") 'helm-ff-run-open-file-externally)
  (define-key helm-generic-files-map (kbd "C-o") 'helm-ff-run-switch-other-window)
  (define-key helm-generic-files-map (kbd "C-M-j") 'helm-ff-run-open-file-with-lister)
  (define-key helm-generic-files-map (kbd "C-j") 'helm-ff-run-open-file-externally)
  (define-key helm-buffer-map (kbd "C-M-j") 'swint-helm-buffer-persp-add-buffers)
  (define-key helm-buffer-map (kbd "C-M-k") 'swint-helm-buffer-persp-remove-buffers)
  (define-key helm-buffer-map (kbd "C-o") 'swint-helm-buffer-switch-persp/other-window)
  (define-key helm-read-file-map (kbd "C-l") 'helm-execute-persistent-action)
  (define-key helm-read-file-map (kbd "C-h") 'helm-find-files-up-one-level)
  (define-key helm-grep-map (kbd "C-o") 'helm-grep-run-other-window-action)
  (define-key helm-command-map (kbd "u") 'helm-unicode)
  (bind-key "C-x M-f" #'(lambda () (interactive) (let ((tramp-completion-use-auth-sources nil)) (helm-find-files-1 "/ssh:"))))
  (bind-key "C-x C-M-f" #'(lambda (&optional arg) (interactive "P") (let ((host (helm-select-host))
                                                                          (curr (or buffer-file-name dired-directory)))
                                                                      (find-file (if arg
                                                                                     (concat "/mnt/sshfs/" host (expand-file-name curr))
                                                                                   (concat "/ssh:" host ":" (abbreviate-file-name curr)))))))
  (bind-key "C-x g" #'(lambda () (interactive)
                        (with-helm-alive-p
                          (helm-run-after-exit 'swint-helm-ag helm-ff-default-directory)))
            helm-find-files-map)
  (bind-key "C-x g" #'(lambda () (interactive)
                        (with-helm-alive-p
                          (helm-exit-and-execute-action
                           (lambda (_candidate)
                             (swint-helm-ag (buffer-local-value 'default-directory _candidate))))))
            helm-buffer-map)
  (bind-key "C-x g" #'(lambda () (interactive)
                        (with-helm-alive-p
                          (helm-exit-and-execute-action
                           (lambda (_candidate)
                             (when-let ((bf (bookmark-get-filename _candidate)))
                               (swint-helm-ag (if (directory-name-p bf)
                                                  bf
                                                (file-name-directory bf))))))))
            helm-bookmark-map)
  ;; ===============keybindings=================
;;;; helm-select-host
  ;; ============helm-select-host===============
  (defun helm-select-host ()
    (helm-comp-read "Remote repo: "
                    (split-string
                     (shell-command-to-string
                      "cat ~/.ssh/config | grep \"^Host \" | awk '{print $2}'"))
                    :buffer "*helm select host*"))
  ;; ============helm-select-host===============
;;;; helm-fd
  ;; =================helm-fd===================
  (use-package helm-fd
    :bind ("C-x f" . swint-helm-fdfind)
    :init
    (bind-key "C-/" nil helm-find-files-map)
    (bind-key "C-x f" #'(lambda () (interactive)
                          (with-helm-alive-p
                            (helm-run-after-exit 'swint-helm-fdfind helm-ff-default-directory)))
              helm-find-files-map)
    (bind-key "C-x f" #'(lambda () (interactive)
                          (with-helm-alive-p
                            (helm-exit-and-execute-action
                             (lambda (_candidate)
                               (swint-helm-fdfind (buffer-local-value 'default-directory _candidate))))))
              helm-buffer-map)
    (bind-key "C-x f" #'(lambda () (interactive)
                          (with-helm-alive-p
                            (helm-exit-and-execute-action
                             (lambda (_candidate)
                               (when-let ((bf (bookmark-get-filename _candidate)))
                                 (swint-helm-fdfind (if (directory-name-p bf)
                                                        bf
                                                      (file-name-directory bf))))))))
              helm-bookmark-map)
    :config
    (bind-key "C-c C-e" #'(lambda () (interactive)
                            (with-helm-alive-p
                              (helm-run-after-exit
                               (lambda (_candidates)
                                 (let ((default-directory (helm-current-directory))
                                       (dirs (append '("*helm-fd-edit*") _candidates)))
                                   (dired dirs)))
                               (progn (helm-mark-all)
                                      (helm-marked-candidates)))))
              helm-fd-map)
    ;; helm-find-files下C-/启用
    (setq helm-fd-executable "fdfind")
    (add-to-list 'helm-fd-switches "--absolute-path")
    (add-to-list 'helm-fd-switches "--follow")
    (defun swint-helm-fdfind (&optional dir)
      (interactive)
      (let ((helm-truncate-lines t)
            ;; 生成局部变量，保证setenv不影响其他进程
            (process-environment (copy-sequence process-environment)))
        ;; ls颜色由$LS_COLORS决定，export LS_COLORS=$LS_COLORS:'di=0;33:'
        (setenv "LS_COLORS" (concat (getenv "LS_COLORS") "di=0;33:"))
        (helm-fd-1 (or dir (helm-current-directory))))))
  ;; =================helm-fd===================
;;;; helm-file-buffer
  ;; ============helm-file-buffer===============
  (defmacro swint-buffer-dired/persp-boolean (is-dired is-persp-curr)
    (let ((cl-remove-op (if (null is-dired) 'cl-remove-if 'cl-remove-if-not))
          (dired-op (if (null is-dired) 'or 'and))
          (persp-curr-op (if (eq is-dired is-persp-curr) 'or 'not)))
      (when (bound-and-true-p persp-mode)
        (cl-loop for x in (persp-buffers (persp-curr)) ;去除#<killed buffer>
                 do (unless (buffer-name x)
                      (delete x (persp-buffers (persp-curr))))))
      `(or (,cl-remove-op (lambda (x) (,dired-op (equal (buffer-mode x) 'dired-mode)
                                                 (,persp-curr-op (and (bound-and-true-p persp-mode)
                                                                      (member x (remq nil (mapcar 'buffer-name (persp-buffers (persp-curr)))))))))
                          (cl-remove-duplicates (helm-buffer-list)))
           (list (buffer-name (messages-buffer))))))
  (defvar helm-source-file-buffers-list/curr-persp nil)
  (defvar helm-source-file-buffers-list/other-persps nil)
  (defvar helm-source-recentf-file nil)
  (defun helm-file-buffers-list--init/curr-persp ()
    ;; Issue #51 Create the list before `helm-buffer' creation.
    (helm-set-attr 'candidates (funcall (helm-get-attr 'buffer-list)))
    (let ((result (cl-loop with allbufs = (memq 'helm-shadow-boring-buffers
                                                (helm-get-attr
                                                 'filtered-candidate-transformer
                                                 helm-source-file-buffers-list/curr-persp))
                           for b in (if allbufs
                                        (helm-get-attr 'candidates)
                                      (helm-skip-boring-buffers
                                       (helm-get-attr 'candidates)
                                       helm-source-file-buffers-list/curr-persp))
                           maximize (length b) into len-buf
                           maximize (length (helm-buffer--format-mode-name b))
                           into len-mode
                           finally return (cons len-buf len-mode))))
      (unless (default-value 'helm-buffer-max-length)
        (helm-set-local-variable 'helm-buffer-max-length (car result)))
      (unless (default-value 'helm-buffer-max-len-mode)
        (helm-set-local-variable 'helm-buffer-max-len-mode (cdr result)))))
  (defclass helm-file-buffers-source/curr-persp (helm-source-sync helm-type-buffer)
    ((buffer-list
      :initarg :buffer-list
      :initform (lambda () (swint-buffer-dired/persp-boolean nil 1))
      :custom function
      :documentation
      "  A function with no arguments to create buffer list.")
     (init :initform 'helm-file-buffers-list--init/curr-persp)
     (multimatch :initform nil)
     (match :initform 'helm-buffers-match-function)
     (persistent-action :initform 'helm-buffers-list-persistent-action)
     (keymap :initform 'helm-buffer-map)
     (find-file-target :initform #'helm-buffers-quit-and-find-file-fn)
     (migemo :initform 'nomultimatch)
     (volatile :initform t)
     (nohighlight :initform t)
     (resume :initform (lambda () (setq helm-buffers-in-project-p nil)))
     (help-message :initform 'helm-buffer-help-message)))
  (defun helm-file-buffers-list--init/other-persps ()
    ;; Issue #51 Create the list before `helm-buffer' creation.
    (helm-set-attr 'candidates (funcall (helm-get-attr 'buffer-list)))
    (let ((result (cl-loop with allbufs = (memq 'helm-shadow-boring-buffers
                                                (helm-get-attr
                                                 'filtered-candidate-transformer
                                                 helm-source-file-buffers-list/other-persps))
                           for b in (if allbufs
                                        (helm-get-attr 'candidates)
                                      (helm-skip-boring-buffers
                                       (helm-get-attr 'candidates)
                                       helm-source-file-buffers-list/other-persps))
                           maximize (length b) into len-buf
                           maximize (length (helm-buffer--format-mode-name b))
                           into len-mode
                           finally return (cons len-buf len-mode))))
      (unless (default-value 'helm-buffer-max-length)
        (helm-set-local-variable 'helm-buffer-max-length (car result)))
      (unless (default-value 'helm-buffer-max-len-mode)
        (helm-set-local-variable 'helm-buffer-max-len-mode (cdr result)))))
  (defclass helm-file-buffers-source/other-persps (helm-source-sync helm-type-buffer)
    ((buffer-list
      :initarg :buffer-list
      :initform (lambda () (swint-buffer-dired/persp-boolean nil nil))
      :custom function
      :documentation
      "  A function with no arguments to create buffer list.")
     (init :initform 'helm-file-buffers-list--init/other-persps)
     (multimatch :initform nil)
     (match :initform 'helm-buffers-match-function)
     (persistent-action :initform 'helm-buffers-list-persistent-action)
     (keymap :initform 'helm-buffer-map)
     (find-file-target :initform #'helm-buffers-quit-and-find-file-fn)
     (migemo :initform 'nomultimatch)
     (volatile :initform t)
     (nohighlight :initform t)
     (resume :initform (lambda () (setq helm-buffers-in-project-p nil)))
     (help-message :initform 'helm-buffer-help-message)))
  (defclass helm-recentf-file-source (helm-source-sync)
    ((init :initform (lambda () (recentf-mode 1)))
     (candidates :initform (lambda () (cl-remove-if (lambda (x)
                                                      ;; (file-directory-p x) ;调用tramp，速度较慢
                                                      (or (directory-name-p x)
                                                          (get-file-buffer x)))
                                                    recentf-list)))
     (pattern-transformer :initform 'helm-recentf-pattern-transformer)
     (match-part :initform (lambda (candidate)
                             (if (or helm-ff-transformer-show-only-basename
                                     helm-recentf--basename-flag)
                                 (helm-basename candidate) candidate)))
     (filter-one-by-one :initform (lambda (c)
                                    (if (and helm-ff-transformer-show-only-basename
                                             (not (consp c)))
                                        (cons (helm-basename c) c)
                                      c)))
     (keymap :initform helm-generic-files-map)
     (find-file-target :initform #'helm-buffers-quit-and-find-file-fn)
     (help-message :initform helm-generic-file-help-message)
     (action :initform (helm-actions-from-type-file))))
  (defun swint-helm-file-buffers-list ()
    "Preconfigured `helm' lightweight file buffers list."
    (interactive)
    (unless helm-source-file-buffers-list/curr-persp
      (setq helm-source-file-buffers-list/curr-persp
            (helm-make-source "File Buffers in current persp" 'helm-file-buffers-source/curr-persp)))
    (unless helm-source-file-buffers-list/other-persps
      (setq helm-source-file-buffers-list/other-persps
            (helm-make-source "File Buffers in other persps" 'helm-file-buffers-source/other-persps))
      (helm-add-action-to-source "Switch to persp/buffer" 'helm-switch-persp/buffer helm-source-file-buffers-list/other-persps 0))
    (unless helm-source-recentf-file
      (setq helm-source-recentf-file
            (helm-make-source "Recentf File" 'helm-recentf-file-source)))
    (let ((helm-ff-transformer-show-only-basename nil))
      (helm :sources '(helm-source-file-buffers-list/curr-persp
                       helm-source-file-buffers-list/other-persps
                       helm-source-recentf-file
                       helm-source-buffer-not-found)
            :buffer "*helm file buffers*"
            :keymap helm-buffer-map
            :truncate-lines t)))
  ;; ============helm-file-buffer===============
;;;; helm-dired-buffer
  ;; ============helm-dired-buffer==============
  (defvar helm-source-dired-buffers-list/curr-persp nil)
  (defvar helm-source-dired-buffers-list/other-persps nil)
  (defvar helm-source-recentf-directory nil)
  (defun helm-dired-buffers-list--init/curr-persp ()
    ;; Issue #51 Create the list before `helm-buffer' creation.
    (helm-set-attr 'candidates (funcall (helm-get-attr 'buffer-list)))
    (let ((result (cl-loop with allbufs = (memq 'helm-shadow-boring-buffers
                                                (helm-get-attr
                                                 'filtered-candidate-transformer
                                                 helm-source-dired-buffers-list/curr-persp))
                           for b in (if allbufs
                                        (helm-get-attr 'candidates)
                                      (helm-skip-boring-buffers
                                       (helm-get-attr 'candidates)
                                       helm-source-dired-buffers-list/curr-persp))
                           maximize (length b) into len-buf
                           maximize (length (helm-buffer--format-mode-name b))
                           into len-mode
                           finally return (cons len-buf len-mode))))
      (unless (default-value 'helm-buffer-max-length)
        (helm-set-local-variable 'helm-buffer-max-length (car result)))
      (unless (default-value 'helm-buffer-max-len-mode)
        (helm-set-local-variable 'helm-buffer-max-len-mode (cdr result)))))
  (defclass helm-dired-buffers-source/curr-persp (helm-source-sync helm-type-buffer)
    ((buffer-list
      :initarg :buffer-list
      :initform (lambda () (swint-buffer-dired/persp-boolean 1 1))
      :custom function
      :documentation
      "  A function with no arguments to create buffer list.")
     (init :initform 'helm-dired-buffers-list--init/curr-persp)
     (multimatch :initform nil)
     (match :initform 'helm-buffers-match-function)
     (persistent-action :initform 'helm-buffers-list-persistent-action)
     (keymap :initform 'helm-buffer-map)
     (find-file-target :initform #'helm-buffers-quit-and-find-file-fn)
     (migemo :initform 'nomultimatch)
     (volatile :initform t)
     (nohighlight :initform t)
     (resume :initform (lambda () (setq helm-buffers-in-project-p nil)))
     (help-message :initform 'helm-buffer-help-message)))
  (defun helm-dired-buffers-list--init/other-persps ()
    ;; Issue #51 Create the list before `helm-buffer' creation.
    (helm-set-attr 'candidates (funcall (helm-get-attr 'buffer-list)))
    (let ((result (cl-loop with allbufs = (memq 'helm-shadow-boring-buffers
                                                (helm-get-attr
                                                 'filtered-candidate-transformer
                                                 helm-source-dired-buffers-list/other-persps))
                           for b in (if allbufs
                                        (helm-get-attr 'candidates)
                                      (helm-skip-boring-buffers
                                       (helm-get-attr 'candidates)
                                       helm-source-dired-buffers-list/other-persps))
                           maximize (length b) into len-buf
                           maximize (length (helm-buffer--format-mode-name b))
                           into len-mode
                           finally return (cons len-buf len-mode))))
      (unless (default-value 'helm-buffer-max-length)
        (helm-set-local-variable 'helm-buffer-max-length (car result)))
      (unless (default-value 'helm-buffer-max-len-mode)
        (helm-set-local-variable 'helm-buffer-max-len-mode (cdr result)))))
  (defclass helm-dired-buffers-source/other-persps (helm-source-sync helm-type-buffer)
    ((buffer-list
      :initarg :buffer-list
      :initform (lambda () (swint-buffer-dired/persp-boolean 1 nil))
      :custom function
      :documentation
      "  A function with no arguments to create buffer list.")
     (init :initform 'helm-dired-buffers-list--init/other-persps)
     (multimatch :initform nil)
     (match :initform 'helm-buffers-match-function)
     (persistent-action :initform 'helm-buffers-list-persistent-action)
     (keymap :initform 'helm-buffer-map)
     (find-file-target :initform #'helm-buffers-quit-and-find-file-fn)
     (migemo :initform 'nomultimatch)
     (volatile :initform t)
     (nohighlight :initform t)
     (resume :initform (lambda () (setq helm-buffers-in-project-p nil)))
     (help-message :initform 'helm-buffer-help-message)))
  (defclass helm-recentf-directory-source (helm-source-sync)
    ((init :initform (lambda () (recentf-mode 1)))
     (candidates :initform (lambda () (cl-remove-if (lambda (x)
                                                      (or (not (directory-name-p x))
                                                          (buffer-live-p (cdr (assoc (expand-file-name x) dired-buffers)))))
                                                    recentf-list)))
     (pattern-transformer :initform 'helm-recentf-pattern-transformer)
     (match-part :initform (lambda (candidate)
                             (if (or helm-ff-transformer-show-only-basename
                                     helm-recentf--basename-flag)
                                 (helm-basename candidate) candidate)))
     (filter-one-by-one :initform (lambda (c)
                                    (if (and helm-ff-transformer-show-only-basename
                                             (not (consp c)))
                                        (cons (helm-basename c) c)
                                      c)))
     (keymap :initform helm-generic-files-map)
     (find-file-target :initform #'helm-buffers-quit-and-find-file-fn)
     (help-message :initform helm-generic-file-help-message)
     (action :initform (helm-actions-from-type-file))))
  (defun swint-helm-dired-buffers-list ()
    "Preconfigured `helm' lightweight dired buffers list."
    (interactive)
    (unless helm-source-dired-buffers-list/curr-persp
      (setq helm-source-dired-buffers-list/curr-persp
            (helm-make-source "Dired Buffers in current persp" 'helm-dired-buffers-source/curr-persp)))
    (unless helm-source-dired-buffers-list/other-persps
      (setq helm-source-dired-buffers-list/other-persps
            (helm-make-source "Dired Buffers in other persps" 'helm-dired-buffers-source/other-persps))
      (helm-add-action-to-source "Switch to persp/buffer" 'helm-switch-persp/buffer helm-source-dired-buffers-list/other-persps 0))
    (unless helm-source-recentf-directory
      (setq helm-source-recentf-directory
            (helm-make-source "Recentf Directory" 'helm-recentf-directory-source)))
    (let ((helm-ff-transformer-show-only-basename nil))
      (helm :sources '(helm-source-dired-buffers-list/curr-persp
                       helm-source-dired-buffers-list/other-persps
                       helm-source-recentf-directory
                       helm-source-buffer-not-found)
            :buffer "*helm dired buffers*"
            :keymap helm-buffer-map
            :truncate-lines t)))
  ;; ============helm-dired-buffer==============
;;;; helm-related-to-persp
  ;; =========helm-related-to-persp=============
  (defun helm-switch-persp/buffer (BUFFER-OR-NAME)
    "Helm-switch to persp/buffer simultaneously."
    (let ((buffer (get-buffer BUFFER-OR-NAME)))
      (if (bound-and-true-p persp-mode)
          (cl-loop for persp in (nreverse (cons  "i" (nreverse (delete "i" (persp-names)))))
                   when (or (memq buffer (persp-buffers (gethash persp (perspectives-hash))))
                            (string-equal persp "i"))
                   do (if (memq buffer (persp-buffers (persp-curr)))
                          (switch-to-buffer buffer)
                        (swint-persp-switch persp)
                        (if (window-live-p (get-buffer-window buffer))
                            (select-window (get-buffer-window buffer))
                          (switch-to-buffer buffer))))
        (switch-to-buffer buffer))))
  (defun swint-helm-buffer-switch-persp/other-window ()
    "Run switch-persp/other-window action from `helm-source-buffers-list'."
    (interactive)
    (with-helm-alive-p
      (helm-run-after-exit 'swint-helm-switch-persp/other-window)))
  (defun swint-helm-buffer-persp-add-buffers ()
    "Run persp-add-buffer action from `helm-source-buffers-list'."
    (interactive)
    (with-helm-alive-p
      (helm-run-after-exit 'swint-helm-persp-add-buffers)))
  (defun swint-helm-buffer-persp-remove-buffers ()
    "Run persp-remove-buffer action from `helm-source-buffers-list'."
    (interactive)
    (with-helm-alive-p
      (helm-run-after-exit 'swint-helm-persp-remove-buffers)))
  (defun swint-helm-switch-persp/other-window ()
    "Helm-switch to persp/other-window simultaneously."
    (let ((buffer (helm-get-selection)))
      (if (or (not (bound-and-true-p persp-mode))
              (memq buffer (persp-buffers (persp-curr))))
          (switch-to-buffer-other-window buffer)
        (let ((curr-buf (current-buffer)))
          (swint-persp-switch "i")
          (switch-to-buffer curr-buf)
          (switch-to-buffer-other-window buffer)))))
  (defun swint-helm-persp-add-buffers ()
    (let* ((bufs (helm-marked-candidates))
           (added-bufs (cl-count-if 'persp-add-buffer bufs)))
      (when (buffer-live-p helm-buffer)
        (with-helm-buffer
          (setq helm-marked-candidates nil
                helm-visible-mark-overlays nil)))
      (message "Addded %s buffer(s)" added-bufs)))
  (defun swint-helm-persp-remove-buffers ()
    (let* ((bufs (helm-marked-candidates))
           (removed-bufs (cl-count-if 'persp-remove-buffer bufs)))
      (when (buffer-live-p helm-buffer)
        (with-helm-buffer
          (setq helm-marked-candidates nil
                helm-visible-mark-overlays nil)))
      (message "Removed %s buffer(s)" removed-bufs)))
  ;; =========helm-related-to-persp=============
;;;; 在其他helm-buffer中运行helm命令
  ;; ======在其他helm-buffer中运行helm命令======
  (defun swint-helm-file-buffers-after-quit ()
    (interactive)
    (if (equal helm-buffer "*helm file buffers*")
        (call-interactively 'helm-maybe-exit-minibuffer)
      (helm-run-after-exit 'swint-helm-file-buffers-list)))
  (defun swint-helm-dired-buffers-after-quit ()
    (interactive)
    (if (equal helm-buffer "*helm dired buffers*")
        (call-interactively 'helm-maybe-exit-minibuffer)
      (helm-run-after-exit 'swint-helm-dired-buffers-list)))
  (defun swint-helm-bookmarks-after-quit ()
    (interactive)
    (helm-run-after-exit 'helm-bookmarks))
  (defun swint-helm-projectile-after-quit ()
    (interactive)
    (helm-run-after-exit 'helm-projectile))
  ;; ======在其他helm-buffer中运行helm命令======
;;;; helm-open-file-with-lister
  ;; ========helm-open-file-with-lister=========
  (defun helm-open-file-with-lister ()
    "Opens a file with lister of total commander."
    (let ((file (helm-get-selection)))
      (start-process-shell-command
       "tc" "*tc*"
       (concat "wine "
               "~/.wine/drive_c/totalcmd/TOTALCMD.EXE /O /T /S=L z:"
               (replace-regexp-in-string " " "\\\\ "
                                         (expand-file-name file))))))
  (defun helm-ff-run-open-file-with-lister ()
    "Run Rename file action from `helm-source-find-files'."
    (interactive)
    (with-helm-alive-p
      (helm-run-after-exit 'helm-open-file-with-lister)))
  ;; ========helm-open-file-with-lister=========
;;;; helm-locate
  ;; ==============helm-locate==================
  ;; 默认使用/var/lib/mlocate/mlocate.db数据库，使用cron每天定时更新或sudo updatedb更新
  (defun swint-helm-locate (&optional arg)
    (interactive "P")
    (let* ((helm-ff-locate-db-filename "~/.helm-locate.db")
           (helm-locate-create-db-command (format "updatedb -l 0 -o %s -U ~/" helm-ff-locate-db-filename))
           (helm-locate-command (format "locate -b -i %%s -r %%s -d %s" helm-ff-locate-db-filename)))
      (if (or arg (not (file-exists-p helm-ff-locate-db-filename))) ;; 更新~/.helm-locate.db
          (let ((process (start-process-shell-command
                          "Updating-locate-db-file" "*Updating-locate-db-file*"
                          helm-locate-create-db-command)))
            (message "Updating locate db file.")
            (set-process-sentinel
             process
             (lambda (process signal)
               (when (memq (process-status process) '(exit signal))
                 (message "Updated locate db file.")))))
        (helm-locate nil))))
  ;; ==============helm-locate==================
;;;; helm-peep-preview-persistent
  ;; ======helm-peep-preview-persistent=========
  (defun helm-peep-preview-persistent ()
    "Show properties without quitting helm."
    (interactive)
    (with-helm-alive-p
      (helm-set-attr 'peep-preview-action 'peep-dired-find-file)
      (helm-execute-persistent-action 'peep-preview-action)))
  (put 'helm-peep-preview-persistent 'helm-only t)
  ;; ======helm-peep-preview-persistent=========
  )
;; ====================helm=====================
;;; helm-pinyin
;; ================helm-pinyin==================
(use-package helm-pinyin
  :load-path "repos/helm-pinyin/"
  :after helm
  :config
  ;; 支持helm-find-files
  (turn-on-helm-pinyin)
  ;; 支持buffer切换
  (defun zjy/pinyin-match (pattern candidate)
    (let ((case-fold-search t))
      (string-match (pinyinlib-build-regexp-string pattern) candidate)))
  (defun zjy/helm-buffer--match-pattern (pattern candidate &optional nofuzzy)
    (let ((bfn (if (and helm-buffers-fuzzy-matching
                        (not nofuzzy)
                        (not helm-migemo-mode)
                        (not (string-match "\\`\\^" pattern)))
                   #'helm-buffer--memo-pattern
                 #'identity))
          (mfn (if helm-migemo-mode
                   #'helm-mm-migemo-string-match #'zjy/pinyin-match)))
      (if (string-match "\\`!" pattern)
          (not (funcall mfn (funcall bfn (substring pattern 1))
                        candidate))
        (funcall mfn (funcall bfn pattern) candidate))))
  (advice-add #'helm-buffer--match-pattern :override #'zjy/helm-buffer--match-pattern)
  ;; 支持其他切换
  ;; 使用pinyinlib-build-regexp-string
  ;; (advice-add 'helm-mm-3-get-patterns :around #'helm-mm-3-get-patterns/around)
  ;; (defun helm-mm-3-get-patterns/around (orig-fn &rest args)
  ;;   (let ((pat (apply orig-fn args)))
  ;;     (cl-loop for (predicate . regexp) in pat
  ;;              for re = (concat "\\(" regexp "\\)\\|\\("
  ;;                               (pinyinlib-build-regexp-string regexp t nil t)
  ;;                               "\\)")
  ;;              collect (cons predicate re))))
  ;; 使用iswitchb-pinyin
  (require 'iswitchb-pinyin)
  (advice-add 'helm-mm-3-match :around #'helm-mm-3-match/around)
  (cl-defun helm-mm-3-match/around (orig-fn str &rest args)
    (apply orig-fn (if (string-match-p "\\cC" str)
                       (concat str "|" (str-unicode-to-pinyin-initial str))
                     str)
           args))
  ;; 使helm-find-files匹配过多，默认在输入前面加空格解决匹配问题
  ;; (advice-add 'helm-find-files-1 :around #'helm-find-files-1/around)
  ;; (defun helm-find-files-1/around (orig-fn fname &rest args)
  ;;   (apply orig-fn (concat fname " ") args))
  )
;; ================helm-pinyin==================
;;; helm-ring
;; ==================helm-ring==================
(use-package helm-ring
  :bind ("C-M-y" . helm-show-kill-ring)
  :config
  (bind-key "C-j" 'helm-kill-ring-action-save helm-kill-ring-map)
  (setq helm-kill-ring-threshold 1)
  (defvar helm-kill-ring-current nil)
  (defun helm-kill-ring-action-save ()
    (interactive)
    (with-helm-alive-p
      (helm-run-after-exit
       #'(lambda () (let ((marked (helm-marked-candidates))
                          (sep (if (equal helm-current-prefix-arg '(16))
                                   (read-string "Separator: ")
                                 helm-kill-ring-separator)))
                      (setq helm-kill-ring-current
                            (substring-no-properties
                             (cl-loop for c in (butlast marked)
                                      concat (concat c sep) into str
                                      finally return (concat str (car (last marked))))))))))))
;; ==================helm-ring==================
;;; helm_lacarte
;; ================helm_lacarte=================
(use-package lacarte
  :commands helm-math-symbols
  :bind (("<escape> M-x" . lacarte-execute-command)
         ("C-x `" . lacarte-execute-menu-command))
  :config
  ;; 使用helm-insert-latex-math代替
  (defvar helm-source-lacarte-math
    '((name . "Math Symbols")
      (init . (lambda()
                (setq helm-lacarte-major-mode major-mode)))
      (candidates
       . (lambda () (if (eq helm-lacarte-major-mode 'latex-mode)
                        (delete '(nil) (lacarte-get-a-menu-item-alist LaTeX-math-mode-map)))))
      (action . (("Open" . (lambda (candidate)
                             (call-interactively candidate)))))))
  (defun helm-math-symbols ()
    "Helm for searching math menus."
    (interactive)
    (helm '(helm-source-lacarte-math)
          (thing-at-point 'symbol) "Symbol: "
          nil nil "*helm math symbols*")))
;; ================helm_lacarte=================
;;; helm-swoop
;; ================helm-swoop===================
(use-package helm-swoop
  :bind (("M-s M-s" . helm-swoop)
         ("M-s M-S" . helm-multi-swoop-all))
  :config
  (setq helm-swoop-split-direction 'split-window-vertically
        helm-swoop-split-with-multiple-windows t)
  ;; helm-swoop 中使用C-c C-e编辑，C-x C-s保存
  (define-key isearch-mode-map (kbd "M-s M-s") 'helm-swoop-from-isearch)
  (define-key helm-swoop-map (kbd "M-S") 'helm-multi-swoop-all-from-helm-swoop))
;; ================helm-swoop===================
;;; helm-unicode
;; ===============helm-unicode==================
(use-package helm-unicode
  :commands helm-unicode)
;; ===============helm-unicode==================
;;; helm-ag
;; =================helm-ag=====================
(use-package helm-ag
  ;; helm-do-ag 互动式搜索，但只能搜索一个词
  ;; helm-ag 先输入词，可以在结果中搜索第二个词
  ;; 默认忽略大小写，两种方式考虑大小写：
  ;; 1. 搜索-s pattern
  ;; 2. C--前缀并输入-s
  :commands (helm-do-ag helm-do-ag-buffers swint-helm-ag)
  :init
  (bind-key "C-x g" 'swint-helm-ag)
  (bind-key "C-x G" #'(lambda () (interactive) (let ((helm-truncate-lines t)) (call-interactively 'helm-do-ag-buffers))))
  :config
  (defun swint-helm-ag (&optional dir)
    (interactive)
    (let ((helm-truncate-lines t))
      (if (or current-prefix-arg helm-current-prefix-arg)
          (call-interactively 'helm-do-ag)
        (helm-do-ag (or dir (helm-current-directory))))))
  (setq helm-ag-base-command "ag --nocolor --nogroup --hidden")
  (setq helm-ag-command-option "--follow") ;Follow symlinks.
  ;; C-c C-e 进入编辑模式，C-x C-s 保存helm-ag结果
  (define-key helm-ag-map (kbd "C-o") 'helm-ag--run-other-window-action)
  (define-key helm-ag-map (kbd "C-h") 'helm-ag--up-one-level)
  (define-key helm-ag-map (kbd "C-l") 'helm-execute-persistent-action)
  (define-key helm-do-ag-map (kbd "C-h") 'helm-ag--do-ag-up-one-level)
  (define-key helm-do-ag-map (kbd "C-l") 'helm-execute-persistent-action))
;; =================helm-ag=====================
;;; helm-descbinds
;; ==============helm-descbinds=================
(use-package helm-descbinds
  :commands helm-descbinds)
;; ==============helm-descbinds=================
;;; helm-imenu
;; ================helm-imenu===================
(use-package imenu
  :commands imenu-choose-buffer-index)
(use-package imenu-anywhere
  :bind ("M-s I" . helm-imenu-anywhere)
  :config
  (setq imenu-anywhere-delimiter " | "))
(use-package helm-imenu
  :bind (("M-s i" . helm-semantic-or-imenu)
         ("M-s o" . helm-imenu-outshine))
  :config
  (setq helm-imenu-delimiter " | ")
  ;; helm-imenu-outshine.
  (defvar helm-source-imenu-outshine nil)
  (defvar helm-cached-imenu-outshine-tick nil)
  (defvar helm-cached-imenu-outshine-candidates nil)
  (defvar-local imenu-outshine--index-alist nil)
  (defun imenu-outshine--make-index-alist ()
    "Create an index alist for the outshine headings."
    (setq imenu-outshine--index-alist
          (save-excursion
            (save-restriction
              (widen)
              ;; imenu中显示headings全部内容
              (imenu--generic-function `((nil ,(concat "^" (outshine-calc-outline-regexp) ".*$") 0))))))
    (imenu--truncate-items imenu-outshine--index-alist))
  (defun helm-imenu-outshine-candidates (&optional buffer)
    (with-current-buffer (or buffer helm-current-buffer)
      (let ((tick (buffer-modified-tick)))
        (if (eq helm-cached-imenu-outshine-tick tick)
            helm-cached-imenu-outshine-candidates
          (setq imenu-outshine--index-alist nil)
          (prog1 (setq helm-cached-imenu-outshine-candidates
                       (let ((index (imenu-outshine--make-index-alist)))
                         (helm-imenu--candidates-1
                          (delete (assoc "*Rescan*" index) index))))
            (setq helm-cached-imenu-outshine-tick tick))))))
  (defclass helm-imenu-outshine-source (helm-source-sync)
    ((candidates :initform 'helm-imenu-outshine-candidates)
     (candidate-transformer :initform 'helm-imenu-transformer)
     (persistent-action :initform 'helm-imenu-persistent-action)
     (persistent-help :initform "Show this entry")
     (keymap :initform helm-imenu-map)
     (help-message :initform 'helm-imenu-help-message)
     (action :initform 'helm-imenu-action)))
  (defun helm-imenu-outshine ()
    "Preconfigured `helm' for `imenu'."
    (interactive)
    (unless helm-source-imenu-outshine
      (setq helm-source-imenu-outshine
            (helm-make-source "Imenu outshine" 'helm-imenu-outshine-source
              :fuzzy-match helm-imenu-fuzzy-match)))
    (let ((imenu-auto-rescan t)
          (str (thing-at-point 'symbol))
          (helm-execute-action-at-once-if-one
           helm-imenu-execute-action-at-once-if-one))
      (helm :sources 'helm-source-imenu-outshine
            :default (list (concat "\\_<" str "\\_>") str)
            :preselect str
            :buffer "*helm imenu outshine*"))))
;; ================helm-imenu===================
;;; ace-jump-helm-line
;; ============ace-jump-helm-line===============
(use-package ace-jump-helm-line
  :commands ace-jump-helm-line
  :config
  (setq ace-jump-helm-line-keys (append (number-sequence ?a ?z)
                                        (number-sequence ?0 ?9)
                                        '(?, ?. ?/ ?' ?\; ? ))))
;; ============ace-jump-helm-line===============
;;; key-chord
;; ================key-chord====================
(use-package key-chord
  :after helm
  :config
  (key-chord-mode 1)
  (key-chord-define helm-map "hh" 'ace-jump-helm-line))
;; ================key-chord====================
(provide 'setup_helm)
