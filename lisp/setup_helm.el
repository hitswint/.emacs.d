;;; helm
;; ====================helm=====================
(use-package helm
  :diminish (helm-mode eldoc-mode)
  :commands (helm-find-files-1
             helm-insert-latex-math
             helm-current-directory
             helm-select-host)
  :bind-keymap ("C-x c" . helm-command-map)
  :bind (("C-'" . helm-bookmarks)
         ("C-," . swint-helm-file-buffers-list)
         ("C-." . swint-helm-dired-buffers-list)
         ("C-x C-f" . helm-find-files)
         ("C-x F" . helm-find)
         ("C-x l" . swint-helm-locate)
         ("C-x y" . helm-resume))
  :init
  (setq helm-minibuffer-history-key "M-i")
  :config
  (use-package helm-for-files)
  (helm-mode 1)
  (helm-top-poll-mode 1)
  (setq helm-move-to-line-cycle-in-source nil
        helm-buffer-details-flag nil
        helm-display-header-line nil
        helm-truncate-lines t
        helm-ff--RET-disabled t
        helm-ff-newfile-prompt-p nil
        helm-ff-show-dot-file-path t
        ;; (add-to-list 'display-buffer-alist '("^\\*helm .*" (display-buffer-at-bottom))) ;在底部打开helm
        helm-split-window-default-side 'same
        helm-show-action-window-other-window nil
        helm-external-programs-associations file-extension-app-alist
        helm-default-external-file-browser "thunar"
        helm-pdfgrep-default-read-command "llpp -page %p \"%f\""
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
                                                 "\\`\\*Org\\ Agenda\\*\\'"
                                                 "\\`\\*Org\\ Preview\\ LaTeX\\ Output\\*\\'"
                                                 "\\`magit-diff:"
                                                 "\\`\\*lsp-bridge"
                                                 "\\`\\*plot-data\\*\\'"
                                                 "\\`\\*Async\\ Shell\\ Command\\*\\'"
                                                 "\\`\\*Shell\\ Command\\ Output\\*\\'"
                                                 "\\`\\*Ebib-.*\\*\\'"
                                                 "\\`\\*mu4e-.*\\*\\'"
                                                 "\\`\\*thunar\\*\\'"
                                                 "\\`\\*tc\\*\\'"
                                                 "\\`\\*viatc\\*\\'"
                                                 "\\`\\*Article\\*\\'"
                                                 "\\`\\*xwidget-webkit:.*\\*\\'"
                                                 "\\`\\*eaf.*\\*\\'"
                                                 "\\`ellama.*\\'")))
  (setq helm-completing-read-handlers-alist (append helm-completing-read-handlers-alist
                                                    '((describe-function . helm-completing-read-symbols)
                                                      (describe-variable . helm-completing-read-symbols)
                                                      (debug-on-entry . helm-completing-read-symbols)
                                                      (find-function . helm-completing-read-symbols)
                                                      (org-annotate-file . nil)
                                                      (swint-org-annotate-file . nil)
                                                      (dired-create-directory . nil))))
  ;; (setq helm-mounted-network-directories '("/mnt/share" "/mnt/sshfs"))
  (set-face-attribute 'helm-buffer-directory nil :foreground "yellow" :background 'unspecified :weight 'bold)
  (set-face-attribute 'helm-buffer-file nil :inherit 'font-lock-type-face)
  (set-face-attribute 'helm-ff-directory nil :foreground "yellow" :background 'unspecified :weight 'bold)
  (set-face-attribute 'helm-ff-dotted-directory nil :foreground "yellow" :background 'unspecified :weight 'bold)
  (set-face-attribute 'helm-ff-file nil :foreground "white")
  (set-face-attribute 'helm-grep-file nil :foreground "cyan")
  (set-face-attribute 'helm-selection nil :background "black" :underline t)
  (set-face-attribute 'helm-visible-mark nil :foreground "DeepSkyBlue1" :background 'unspecified)
;;;; keybindings
  ;; ===============keybindings=================
  (define-key helm-map (kbd "C-;") 'helm-toggle-visible-mark)
  (define-key helm-map (kbd "M-m") 'helm-toggle-visible-mark)
  (define-key helm-map (kbd "M-t") 'helm-toggle-all-marks)
  (define-key helm-map (kbd "C-l") 'helm-execute-persistent-action)
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
  (define-key helm-map (kbd "M-.") 'helm-toggle-truncate-line)
  (define-key helm-map (kbd "C-x y") 'helm-resume-list-buffers-after-quit)
  (define-key helm-map (kbd "C-c C-k") 'helm-kill-selection-and-quit)
  (define-key helm-map (kbd "C-c C-i") 'helm-insert-or-copy)
  (helm-define-key-with-subkeys helm-map (kbd "C-x Y") ?Y 'helm-run-cycle-resume)
  (helm-define-key-with-subkeys global-map (kbd "C-x Y") ?Y 'helm-cycle-resume)
  (define-key helm-find-files-map (kbd "M-.") 'helm-toggle-truncate-line)
  (define-key helm-find-files-map (kbd "M-i") 'helm-peep-preview-persistent)
  (define-key helm-find-files-map (kbd "M-o") 'helm-ff-properties-persistent)
  (define-key helm-find-files-map (kbd "C-l") 'helm-execute-persistent-action)
  (define-key helm-find-files-map (kbd "C-h") 'helm-find-files-up-one-level)
  (define-key helm-find-files-map (kbd "C-o") 'helm-ff-run-switch-other-window)
  (define-key helm-find-files-map (kbd "C-j") 'helm-ff-run-open-file-externally)
  (define-key helm-find-files-map (kbd "C-M-j") 'helm-ff-run-open-file-with-default-tool)
  (define-key helm-find-files-map (kbd "M-S-<return>") #'(lambda () (interactive) (with-helm-alive-p
                                                                                    (helm-exit-and-execute-action
                                                                                     (lambda (_candidate)
                                                                                       (cl-letf (((symbol-function 'find-file) 'find-file-literally))
                                                                                         (helm-find-file-or-marked _candidate)))))))
  (define-key helm-find-files-map (kbd "M-<return>") #'(lambda () (interactive) (with-helm-alive-p
                                                                                  (helm-exit-and-execute-action
                                                                                   (lambda (_candidate)
                                                                                     (cl-letf (((symbol-function 'find-file) 'swint-find-file-literally))
                                                                                       (helm-find-file-or-marked _candidate)))))))
  (define-key helm-generic-files-map (kbd "C-o") 'helm-ff-run-switch-other-window)
  (define-key helm-generic-files-map (kbd "C-j") 'helm-ff-run-open-file-externally)
  (define-key helm-generic-files-map (kbd "C-M-j") 'helm-ff-run-open-file-with-default-tool)
  (define-key helm-buffer-map (kbd "C-M-j") 'swint-helm-buffer-persp-add-buffers)
  (define-key helm-buffer-map (kbd "C-M-k") 'swint-helm-buffer-persp-remove-buffers)
  (define-key helm-buffer-map (kbd "C-o") 'swint-helm-buffer-switch-persp/other-window)
  (define-key helm-read-file-map (kbd "C-l") 'helm-execute-persistent-action)
  (define-key helm-read-file-map (kbd "C-h") 'helm-find-files-up-one-level)
  (define-key helm-grep-map (kbd "C-o") 'helm-grep-run-other-window-action)
  (define-key helm-command-map (kbd "u") 'helm-unicode)
  (define-key helm-bookmark-map (kbd "C-o") 'helm-bookmark-run-jump-other-window)
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
                                       (dirs (append '("helm-fd-edit") _candidates)))
                                   (dired dirs)))
                               (progn (helm-mark-all)
                                      (helm-marked-candidates)))))
              helm-fd-map)
    ;; helm-find-files下C-/启用
    (setq helm-fd-executable "fdfind")
    ;; (add-to-list 'helm-fd-switches "--absolute-path")
    (add-to-list 'helm-fd-switches "--follow")
    (defun swint-helm-fdfind (&optional dir)
      (interactive)
      ;; 生成局部变量，保证setenv不影响其他进程
      (let ((process-environment (copy-sequence process-environment)))
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
;;; helm-command
;; ===============helm-command==================
(use-package helm-command
  :bind (("M-x" . helm-M-x)
         ("M-s M-x" . helm-M-x-major-mode)
         ("M-s M-X" . helm-M-x-minor-mode))
  :config
  ;; https://github.com/DarwinAwardWinner/amx
  (defun amx-extract-commands-from-features (mode &optional match-by-name)
    (let ((library-path (symbol-file mode))
          mode-name commands)
      (when match-by-name
        (setq mode-name (symbol-name mode))
        (string-match "\\(.+?\\)\\(-mode\\)?$" mode-name)
        (setq mode-name (match-string 1 mode-name))
        (if (string= mode-name "c") (setq mode-name "cc"))
        (setq mode-name (regexp-quote mode-name)))
      (dolist (feature load-history)
        (let ((feature-path (car feature)))
          (when (and feature-path (or (equal feature-path library-path)
                                      (and mode-name
                                           (string-match mode-name (file-name-nondirectory
                                                                    feature-path)))))
            (dolist (item (cdr feature))
              (if (and (listp item) (eq 'defun (car item)))
                  (let ((function (cdr item)))
                    (when (commandp function)
                      (setq commands (append commands (list function))))))))))
      commands))
  (defun helm-M-x-transformer-major/minor (condidates &optional mode-map-alist)
    (cl-loop with local-map = (or mode-map-alist (helm-M-x-current-mode-map-alist))
             for cmd in condidates
             for local-key = (car (rassq cmd local-map))
             for key = (substitute-command-keys (format "\\[%s]" cmd))
             collect (cons (propertize
                            (format "%s%s" cmd
                                    (cond (local-key
                                           (concat " " (propertize " " 'display
                                                                   (propertize local-key 'face 'helm-M-x-key))))
                                          ((not (string-match "^M-x" key))
                                           (concat " " (propertize " " 'display
                                                                   (propertize key 'face 'helm-M-x-key))))
                                          (t ""))))
                           cmd)))
  (defun helm-M-x-major-mode ()
    (interactive)
    (let* ((commands (remove nil (delete-dups (append (map-values (helm-M-x-current-mode-map-alist))
                                                      (amx-extract-commands-from-features major-mode)))))
           (command (helm-comp-read "M-x: " (helm-M-x-transformer-major/minor commands)
                                    :buffer "*helm M-x for major-mode*")))
      (execute-extended-command (or current-prefix-arg helm-current-prefix-arg) (symbol-name command))))
  (defun helm-M-x-minor-mode ()
    (interactive)
    (let* ((minor-mode (intern (completing-read
                                "Minor Mode: "
                                local-minor-modes nil t)))
           (minor-mode-map-alist (let ((map-sym (helm-get-mode-map-from-mode minor-mode)))
                                   (when (and map-sym (boundp map-sym))
                                     (helm-M-x-get-major-mode-command-alist (symbol-value map-sym)))))
           (commands (amx-extract-commands-from-features minor-mode t))
           (command (helm-comp-read "M-x: " (helm-M-x-transformer-major/minor commands minor-mode-map-alist)
                                    :buffer "*helm M-x for minor-mode*")))
      (execute-extended-command (or current-prefix-arg helm-current-prefix-arg) (symbol-name command)))))
;; ===============helm-command==================
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
  :bind (("C-M-y" . helm-show-kill-ring)
         ("M-Y" . helm-show-copyq))
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
                                      finally return (concat str (car (last marked)))))))))))
  (define-key helm-kill-ring-map (kbd "RET") #'(lambda () (interactive)
                                                 (with-helm-alive-p
                                                   (helm-exit-and-execute-action
                                                    (lambda (_candidate)
                                                      (helm-kill-ring-action-yank _candidate))))))
  (defun helm-show-copyq ()
    (interactive)
    (let ((copyq-history (with-temp-buffer
                           ;; (split-string (shell-command-to-string
                           ;;                "copyq 'tab(config(\"clipboard_tab\")); for (var i = 0; i < size(); ++i) { print(read(i)); print(\"\\\\n\"); }'")
                           ;;               "\\n" t)
                           (when (= 0 (call-process
                                       "copyq" nil (current-buffer) nil "eval"
                                       (format "'[' + [...Array(%d).keys()].map(i => JSON.stringify(str(read(%d + i)))).join(' ') + ']'"
                                               200 0)))  ;实际copyq最多保存199条数据
                             (append (car (read-from-string (buffer-string))) nil)))))
      (helm-comp-read "Copyq: " copyq-history
                      :marked-candidates t
                      :keymap helm-kill-ring-map
                      :buffer "*helm copyq-swint*"))))
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
       . (lambda () (if (eq helm-lacarte-major-mode 'LaTeX-mode)
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
  ;; 使用C-c C-e编辑，C-x C-s保存
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
  ;; C-c C-e 进入编辑模式，C-x C-s 保存helm-ag结果
  :commands (swint-helm-ag
             helm-do-ag
             helm-do-ag-this-file
             helm-do-ag-buffers
             swint-helm-do-ag-this-file
             swint-helm-do-ag-buffers
             helm-ag-open-file-action)
  :init
  ;; 搜索压缩文件需加--all-types/--search-zip，支持gz/xz两种格式
  ;; bug：只显示部分结果，最后一行显示ag: truncated file: Success
  ;; https://github.com/ggreer/the_silver_searcher/issues/1243
  (setq helm-ag-base-command "ag --nocolor --nogroup")
  (setq helm-ag-command-option "--hidden --follow")
  (bind-key "C-x g" 'swint-helm-ag)
  (bind-key "C-x G" #'(lambda () (interactive)
                        (let ((helm-ag-base-command (if (directory-files-recursively (helm-current-directory) "\\.gz$")
                                                        "rg --smart-case --color never --no-heading --search-zip"
                                                      "rga --smart-case --color never --no-heading --line-number")))
                          (call-interactively 'swint-helm-ag))))
  (bind-key "M-s g" 'swint-helm-do-ag-this-file)
  (bind-key "M-s G" 'swint-helm-do-ag-buffers)
  (add-hook 'isearch-mode-hook (lambda ()
                                 (bind-key "M-s g" #'(lambda () (interactive) (helm-do-ag-this-file (isearch-current-thing)))
                                           isearch-mode-map)
                                 (bind-key "M-s G" #'(lambda () (interactive) (helm-do-ag-buffers (isearch-current-thing)))
                                           isearch-mode-map)))
  :config
  (advice-add 'helm-ag--action-find-file :after #'(lambda (candidate) (when (bound-and-true-p vlf-mode)
                                                                        (let ((target-entry (split-string candidate ":")))
                                                                          (swint-vlf-goto-line
                                                                           nil (string-to-number (cl-first target-entry)))))))
  (defun swint-helm-do-ag-this-file ()
    (interactive)
    (let ((current-file (or (buffer-file-name)
                            (bound-and-true-p eaf--buffer-url)))
          (helm-ag-base-command (concat "rga --color never --no-heading "
                                        (if (bound-and-true-p vlf-mode)
                                            "--byte-offset"
                                          "--line-number")))
          (ff-orig-fn (symbol-function 'find-file)))
      (cl-letf (((symbol-function 'find-file)
                 (lambda (filename &rest args)
                   (if-let (buf (get-file-buffer filename))
                       (switch-to-buffer buf)
                     (apply ff-orig-fn filename args)))))
        (helm-aif current-file
            (helm-do-ag default-directory (list current-file))
          (error "Error: This buffer is not visited file")))))
  (defun swint-helm-do-ag-buffers ()
    (interactive)
    (let ((helm-ag-base-command "rga --color never --no-heading --line-number"))
      (if (and (eq major-mode 'eaf-mode)
               (let ((eaf-pdf-bufs (cl-remove-if-not (lambda (buf)
                                                       (equal (buffer-local-value 'eaf--buffer-app-name buf) "pdf-viewer"))
                                                     (eaf--get-eaf-buffers))))
                 (= (length eaf-pdf-bufs) 1)))
          (swint-helm-do-ag-this-file)
        (helm-do-ag-buffers))))
  (defun helm-ag-open-file-action (file-name page &optional search-string)
    (if (string= (ignore-errors (downcase (file-name-extension file-name))) "pdf")
        (let* ((pdf-file (expand-file-name file-name))
               (pdf-buffer (eaf-interleave--find-buffer pdf-file)))
          (if (not (or pdf-buffer (string= (shell-command-to-string "pgrep -x qpdfview") "")))
              (start-process "Shell" nil shell-file-name shell-command-switch
                             (concat "qpdfview --unique" (when search-string
                                                           (concat " --search \"" search-string "\""))
                                     " \"" file-name "#" page "\""))
            (unless (eq (current-buffer) pdf-buffer)
              (if-let ((eaf-pdf-win (eaf-find-pdf-window)))
                  (select-window eaf-pdf-win)
                (switch-to-buffer-other-window (current-buffer))))
            (eaf-open-pdf-with-page pdf-file page)
            (when search-string
              (with-current-buffer (eaf-interleave--find-buffer pdf-file)
                (eaf-pdf-search-with-text search-string)))))
      (dired-async-shell-command (expand-file-name file-name))))
  (defun helm-ag-open-file-externally (candidates)
    (interactive)
    (let* ((candidate (car candidates))
           (this-file (unless (memq 'pt helm-ag--command-features)
                        (helm-ag--search-this-file-p)))
           (file-line (helm-grep-split-line candidate))
           (filename (or this-file (cl-first file-line) candidate))
           (line (cond ((string= (ignore-errors (downcase (file-name-extension filename))) "pdf")
                        (let ((page-string (split-string candidate ":")))
                          (cadr (split-string (if this-file
                                                  (cl-second page-string)
                                                (cl-third page-string))))))
                       (this-file
                        (cl-first (split-string candidate ":")))
                       (t
                        (cl-second file-line))))
           (default-directory (or helm-ag--default-directory
                                  helm-ag--last-default-directory
                                  default-directory)))
      (setq helm-ag--last-default-directory default-directory)
      (helm-ag-open-file-action filename line helm-ag--last-query)))
  (defun helm-ag--file-visited-buffers ()
    (let ((bufs (cl-loop for buf in (buffer-list)
                         when (if (equal (buffer-mode buf) 'eaf-mode)
                                  (when (equal (buffer-local-value 'eaf--buffer-app-name buf) "pdf-viewer")
                                    (buffer-local-value 'eaf--buffer-url buf))
                                (buffer-file-name buf))
                         collect it)))
      (if (not helm-ag-ignore-buffer-patterns)
          bufs
        (cl-loop for buf in bufs
                 when (helm-ag--search-buffer-p buf)
                 collect buf))))
  (defun swint-helm-ag (&optional dir)
    (interactive)
    (let ((default-directory (or dir (helm-current-directory))))
      (if (or current-prefix-arg helm-current-prefix-arg)
          (let ((helm-ag-command-option (concat helm-ag-command-option " -u"
                                                (let ((lines (read-string "Print N lines before and after matches: ")))
                                                  (unless (string-empty-p lines)
                                                    (format " --context=%s" lines))))))
            (call-interactively 'helm-do-ag))
        (helm-do-ag default-directory (when-let* ((marked-files (dired-get-marked-files nil nil nil t))
                                                  (mark-existp (cdr marked-files)))
                                        (remove t marked-files))))))
  (define-key helm-ag-map (kbd "C-M-p") 'helm-ag--previous-file)
  (define-key helm-ag-map (kbd "C-M-n") 'helm-ag--next-file)
  (define-key helm-ag-map (kbd "C-o") 'helm-ag--run-other-window-action)
  (define-key helm-ag-map (kbd "C-j") #'(lambda () (interactive) (with-helm-alive-p
                                                                   (helm-run-after-exit 'helm-ag-open-file-externally (helm-marked-candidates)))))
  (define-key helm-ag-map (kbd "C-h") 'helm-ag--up-one-level)
  (define-key helm-ag-map (kbd "C-l") 'helm-execute-persistent-action)
  (define-key helm-do-ag-map (kbd "C-h") 'helm-ag--do-ag-up-one-level)
  (define-key helm-do-ag-map (kbd "C-l") 'helm-execute-persistent-action))
;; =================helm-ag=====================
;;; helm-descbinds
;; ==============helm-descbinds=================
(use-package helm-descbinds
  :after helm
  :config
  (setq helm-descbinds-disable-which-key nil)
  (helm-descbinds-mode 1))
;; ==============helm-descbinds=================
;;; helm-imenu
;; ================helm-imenu===================
(use-package imenu-anywhere
  :bind ("M-s I" . helm-imenu-anywhere)
  :config
  (setq imenu-anywhere-delimiter " | "))
(use-package helm-imenu
  :bind (("M-s i" . helm-semantic-or-imenu)
         ("M-s o" . helm-imenu-outshine)
         ("M-s O" . helm-imenu-outshine-in-all-buffers))
  :config
  (setq imenu-max-item-length nil)
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
  (defun helm-imenu-outshine-candidates-in-all-buffers ()
    (let ((lst (buffer-list)))
      (cl-loop with cur-buf = (current-buffer)
               for b in lst
               when (with-current-buffer b
                      (and (or (member major-mode helm-imenu-extra-modes)
                               (derived-mode-p 'prog-mode))
                           (helm-same-major-mode-p
                            cur-buf helm-imenu-all-buffer-assoc)))
               collect (helm-make-source
                           (format "Imenu outshine in %s" (buffer-name b))
                           'helm-imenu-outshine-source
                         :candidates (with-current-buffer b
                                       (helm-imenu-outshine-candidates b))
                         :fuzzy-match helm-imenu-fuzzy-match))))
  (defclass helm-imenu-outshine-source (helm-source-sync)
    ((candidates :initform 'helm-imenu-outshine-candidates)
     (candidate-transformer :initform 'helm-imenu-transformer)
     (persistent-action :initform 'helm-imenu-persistent-action)
     (persistent-help :initform "Show this entry")
     (keymap :initform helm-imenu-map)
     (help-message :initform 'helm-imenu-help-message)
     (action :initform 'helm-imenu-action)))
  (defun helm-imenu-outshine ()
    (interactive)
    (if (not outshine-mode)
        (let ((outline-regexp (cond ((eq major-mode 'org-mode)
                                     org-outline-regexp-bol)
                                    ((eq major-mode 'LaTeX-mode)
                                     (LaTeX-outline-regexp t))
                                    (t outline-regexp)))
              helm-candidate-number-limit)
          (call-interactively 'helm-outline))
      (unless helm-source-imenu-outshine
        (setq helm-source-imenu-outshine
              (helm-make-source "Imenu outshine" 'helm-imenu-outshine-source
                :fuzzy-match helm-imenu-fuzzy-match)))
      (let ((imenu-auto-rescan t)
            (helm-highlight-matches-around-point-max-lines 'never)
            (str (thing-at-point 'symbol))
            (helm-execute-action-at-once-if-one
             helm-imenu-execute-action-at-once-if-one))
        (helm :sources 'helm-source-imenu-outshine
              :default (list (concat "\\_<" str "\\_>") str)
              :preselect str
              :buffer "*helm imenu outshine*"))))
  (defun helm-imenu-outshine-in-all-buffers ()
    (interactive)
    (let ((imenu-auto-rescan t)
          (helm-highlight-matches-around-point-max-lines 'never)
          (str (thing-at-point 'symbol))
          (helm-execute-action-at-once-if-one
           helm-imenu-execute-action-at-once-if-one))
      (helm :sources (helm-imenu-outshine-candidates-in-all-buffers)
            :default (list (concat "\\_<" str "\\_>") str)
            :preselect str
            :buffer "*helm imenu outshine all*"))))
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
