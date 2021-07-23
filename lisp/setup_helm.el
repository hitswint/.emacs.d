;;; helm
;; ====================helm=====================
(def-package! helm
  :commands (helm-find-files-1
             helm-insert-latex-math)
  :bind-keymap ("C-x c" . helm-command-map)
  :bind (("C-'" . helm-bookmarks)
         ("C-," . swint-helm-file-buffers-list)
         ("C-." . swint-helm-dired-buffers-list)
         ("C-x C-f" . helm-find-files)
         ("C-x f" . helm-find)
         ("M-x" . helm-M-x)
         ("C-x l" . swint-helm-locate)
         ("C-x y" . helm-resume))
  :config
  (def-package! helm-config)
  (def-package! helm-for-files)
  (helm-mode 1)
  (helm-top-poll-mode 1)
  (setq helm-completing-read-handlers-alist '((describe-function . helm-completing-read-symbols)
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
                                              (dired-create-directory)))
  (setq helm-buffer-details-flag nil)
  (setq helm-ff-newfile-prompt-p nil)
  (setq helm-split-window-default-side 'same)
  (setq helm-external-programs-associations file-extension-app-alist)
  (setq helm-fd-executable "fdfind")    ;helm-find-files下C-/启用
  (setq helm-pdfgrep-default-read-command "llpp -page %p \"%f\"")
  (setq helm-boring-buffer-regexp-list (append helm-boring-buffer-regexp-list '("\\`Enjoy\\ Music\\'" "\\`\\*Inferior\\ Octave\\*\\'" "\\`\\*Ibuffer\\*\\'" "\\`\\*MATLAB\\*\\'" "\\`\\*shell\\*\\'" "\\`\\*calculator\\*\\'" "\\`\\*Calendar\\*\\'" "\\`\\*Process\\ List\\*\\'" "\\`\\*toc\\*\\'" "\\`\\*buffer-selection\\*\\'" "\\`\\*Disabled\\ Command\\*\\'" "\\`\\*Mingus\\*\\'" "\\`\\*Ido\\ Completions\\*\\'" "\\`.english-words\\'" "\\`\\*Help\\*\\'" "\\`\\*tramp.*\\*\\'" "\\`\\*baidu-translate\\*\\'" "\\`\\*NOX.*\\*\\'" "\\`\\*nox.*\\*\\'" "\\`\\*org-brain-helm\\*\\'")))
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
  (define-key helm-map (kbd "C-M-p") 'helm-previous-source)
  (define-key helm-map (kbd "C-M-n") 'helm-next-source)
  (define-key helm-map (kbd "M-U") 'helm-unmark-all)
  (define-key helm-map (kbd "M-T") 'helm-toggle-all-marks)
  (define-key helm-map (kbd "C-,") 'swint-helm-file-buffers-after-quit)
  (define-key helm-map (kbd "C-.") 'swint-helm-dired-buffers-after-quit)
  (define-key helm-map (kbd "C-'") 'swint-helm-bookmarks-after-quit)
  (define-key helm-map (kbd "M-'") 'swint-helm-projectile-after-quit)
  (define-key helm-map (kbd "M-RET") 'helm-quit-and-find-file)
  (define-key helm-map (kbd "<C-tab>") 'helm-resume-previous-session-after-quit)
  (define-key helm-map (kbd "C-x y") 'helm-resume-list-buffers-after-quit)
  (define-key helm-map (kbd "C-c C-k") 'helm-kill-selection-and-quit)
  (define-key helm-map (kbd "C-c C-i") 'helm-insert-or-copy)
  (helm-define-key-with-subkeys helm-map (kbd "C-x Y") ?Y 'helm-run-cycle-resume)
  (helm-define-key-with-subkeys global-map (kbd "C-x Y") ?Y 'helm-cycle-resume)
  (define-key helm-find-files-map (kbd "C-l") 'helm-execute-persistent-action)
  (define-key helm-find-files-map (kbd "C-h") 'helm-find-files-up-one-level)
  (define-key helm-find-files-map (kbd "M-U") 'helm-unmark-all)
  (define-key helm-find-files-map (kbd "M-T") 'helm-toggle-all-marks)
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
  (bind-key "C-x M-f" '(lambda () (interactive) (helm-find-files-1 "/ssh:")))
  ;; ===============keybindings=================
;;;; helm-pinyin
  ;; ================helm-pinyin================
  (require 'iswitchb-pinyin)
  ;; 支持中文拼音首字母匹配，会使helm-find-files匹配过多。
  (cl-defun helm-mm-3-match/around (orig-fn str &rest args)
    (apply orig-fn (concat str "|" (str-unicode-to-pinyin-initial str)) args))
  (advice-add 'helm-mm-3-match :around #'helm-mm-3-match/around)
  ;; 默认在输入前面加空格解决匹配问题。
  (defun helm-find-files-1/around (orig-fn fname &rest args)
    (apply orig-fn (concat fname " ") args))
  (advice-add 'helm-find-files-1 :around #'helm-find-files-1/around)
  ;; ================helm-pinyin================
;;;; helm-file-buffer
  ;; ============helm-file-buffer===============
  (defmacro swint-buffer-dired/persp-boolean (is-dired is-persp-curr)
    (let ((cl-remove-op (if (null is-dired) 'cl-remove-if 'cl-remove-if-not))
          (dired-op (if (null is-dired) 'or 'and))
          (persp-curr-op (if (eq is-dired is-persp-curr) 'or 'not))
          (type (if (null is-dired) "file" "dired")))
      `(or (,cl-remove-op (lambda (x) (,dired-op (equal (buffer-mode x) 'dired-mode)
                                                 (,persp-curr-op (and (bound-and-true-p persp-mode)
                                                                      (member x (remq nil (mapcar 'buffer-name (persp-buffers (persp-curr)))))))))
                          (cl-remove-duplicates (helm-buffer-list)))
           (list (buffer-name (get-buffer-create (format "*Virtual helm %s buffers*" ,type)))))))
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
     (matchplugin :initform nil)
     ;; (multimatch :initform nil)
     (match :initform 'helm-buffers-match-function)
     (persistent-action :initform 'helm-buffers-list-persistent-action)
     (keymap :initform helm-buffer-map)
     ;; (migemo :initform 'nomultimatch)
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
     (matchplugin :initform nil)
     ;; (multimatch :initform nil)
     (match :initform 'helm-buffers-match-function)
     (persistent-action :initform 'helm-buffers-list-persistent-action)
     (keymap :initform helm-buffer-map)
     ;; (migemo :initform 'nomultimatch)
     (volatile :initform t)
     (nohighlight :initform t)
     (resume :initform (lambda () (setq helm-buffers-in-project-p nil)))
     (help-message :initform 'helm-buffer-help-message)))
  (defclass helm-recentf-file-source (helm-source-sync)
    ((init :initform (lambda () (recentf-mode 1)))
     (candidates :initform (lambda () (cl-remove-if (lambda (x)
                                                      (or (file-directory-p x)
                                                          (member x (mapcar 'buffer-file-name (buffer-list)))))
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
     (matchplugin :initform nil)
     ;; (multimatch :initform nil)
     (match :initform 'helm-buffers-match-function)
     (persistent-action :initform 'helm-buffers-list-persistent-action)
     (keymap :initform helm-buffer-map)
     ;; (migemo :initform 'nomultimatch)
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
     (matchplugin :initform nil)
     ;; (multimatch :initform nil)
     (match :initform 'helm-buffers-match-function)
     (persistent-action :initform 'helm-buffers-list-persistent-action)
     (keymap :initform helm-buffer-map)
     ;; (migemo :initform 'nomultimatch)
     (volatile :initform t)
     (nohighlight :initform t)
     (resume :initform (lambda () (setq helm-buffers-in-project-p nil)))
     (help-message :initform 'helm-buffer-help-message)))
  (defclass helm-recentf-directory-source (helm-source-sync)
    ((init :initform (lambda () (recentf-mode 1)))
     (candidates :initform (lambda () (let ((directorys-opened (cl-loop for x in (buffer-list)
                                                                        when (equal (buffer-mode x) 'dired-mode)
                                                                        collect (expand-file-name (buffer-local-value 'default-directory x)))))
                                        (cl-remove-if (lambda (x)
                                                        (or (not (file-directory-p x))
                                                            (member x directorys-opened)))
                                                      recentf-list))))
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
    (helm-run-after-exit #'(lambda () (swint-helm-file-buffers-list))))
  (defun swint-helm-dired-buffers-after-quit ()
    (interactive)
    (helm-run-after-exit #'(lambda () (swint-helm-dired-buffers-list))))
  (defun swint-helm-bookmarks-after-quit ()
    (interactive)
    (helm-run-after-exit #'(lambda () (helm-bookmarks))))
  (defun swint-helm-projectile-after-quit ()
    (interactive)
    (helm-run-after-exit #'(lambda () (helm-projectile))))
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
  ;; 默认使用/var/lib/mlocate/mlocate.db数据库，使用cron每天定时更新或sudo updatedb更新。
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
  )
;; ====================helm=====================
;;; helm-ring
;; ==================helm-ring==================
(def-package! helm-ring
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
(def-package! lacarte
  :commands helm-math-symbols
  :bind (("<escape> M-x" . lacarte-execute-command)
         ("C-x `" . lacarte-execute-menu-command))
  :config
  ;; 使用helm-insert-latex-math代替。
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
(def-package! helm-swoop
  :bind (("M-s M-s" . helm-swoop)
         ("M-s M-S" . helm-multi-swoop-all))
  :config
  ;; helm-swoop 中使用C-c C-e编辑，C-x C-s保存。
  (define-key isearch-mode-map (kbd "M-s M-s") 'helm-swoop-from-isearch)
  (define-key helm-swoop-map (kbd "M-S") 'helm-multi-swoop-all-from-helm-swoop))
;; ================helm-swoop===================
;;; helm-unicode
;; ===============helm-unicode==================
(def-package! helm-unicode
  :commands helm-unicode)
;; ===============helm-unicode==================
;;; helm-ag
;; =================helm-ag=====================
(def-package! helm-ag
  ;; helm-do-ag 互动式搜索，但只能搜索一个词。
  ;; helm-ag 先输入词，可以在结果中搜索第二个词。
  :commands (helm-do-ag helm-do-ag-buffers)
  :init
  (bind-key "C-x g" '(lambda () (interactive) (let ((helm-truncate-lines t)) (call-interactively 'helm-do-ag))))
  (bind-key "C-x G" '(lambda () (interactive) (let ((helm-truncate-lines t)) (call-interactively 'helm-do-ag-buffers))))
  :config
  (setq helm-ag-base-command "ag --nocolor --nogroup --hidden")
  (setq helm-ag-command-option "--follow") ;Follow symlinks.
  ;; C-c C-e 进入编辑模式，C-x C-s 保存helm-ag结果。
  (define-key helm-ag-map (kbd "C-h") 'helm-ag--up-one-level)
  (define-key helm-ag-map (kbd "C-o") 'helm-ag--run-other-window-action))
;; =================helm-ag=====================
;;; helm-descbinds
;; ==============helm-descbinds=================
(def-package! helm-descbinds
  :commands helm-descbinds)
;; ==============helm-descbinds=================
;;; helm-imenu
;; ================helm-imenu===================
(def-package! imenu
  :commands imenu-choose-buffer-index)
(def-package! imenu-anywhere
  :bind ("M-s I" . helm-imenu-anywhere)
  :config
  (setq imenu-anywhere-delimiter " | "))
(def-package! helm-imenu
  :bind (("M-s i" . helm-semantic-or-imenu)
         ("M-s M-i" . helm-imenu-outshine))
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
              ;; imenu中显示headings全部内容。
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
(def-package! ace-jump-helm-line
  :commands ace-jump-helm-line
  :config
  (setq ace-jump-helm-line-keys (append (number-sequence ?a ?z)
                                        (number-sequence ?0 ?9)
                                        '(?, ?. ?/ ?' ?\; ? ))))
;; ============ace-jump-helm-line===============
;;; key-chord
;; ================key-chord====================
(def-package! key-chord
  :after helm
  :config
  (key-chord-mode 1)
  (key-chord-define helm-map "hh" 'ace-jump-helm-line))
;; ================key-chord====================
(provide 'setup_helm)
