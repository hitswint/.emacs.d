;;; abbrev
;; ====================abbrev======================
(def-package! abbrev
  :diminish abbrev-mode
  :config
  ;; Turn on abbrev mode globally.
  (setq-default abbrev-mode t)
  ;; Stop asking whether to save newly added abbrev when quitting emacs.
  (setq save-abbrevs t)
  (setq abbrev-file-name "~/.emacs.d/abbrev_defs")
  (when (file-exists-p abbrev-file-name)
    (quietly-read-abbrev-file))
  ;; Sample use of emacs abbreviation feature.
  (define-abbrev-table 'global-abbrev-table
    '(("abqq" "278064399@qq.com")
      ("abgg" "guiqiangw2013@gmail.com")
      ("abhot" "wguiqiang@hotmail.com")
      ("ab126" "wgq_hit@126.com")
      ("ab163" "wgq_713@163.com")
      ("abwgq" "Guiqiang Wang"))))
;; 编辑abbrev-table：C-x a g为当前位置之前词语，全局加入abbrev。
;; C-x a +为当前位置之前词语，在当前mode下加入abbrev。
;; 上述命令前加前缀C-u 3表示当前位置之前三个词。
;; define-global-abbrev/define-mode-abbrev自定义abbrev。
;; 退出时会要求保存abbrev_defs文件。
;; ====================abbrev======================
;;; server
;; ====================server======================
(def-package! server
  :config
  (unless (or (and (fboundp 'daemonp) (daemonp))
              (server-running-p))
    (server-start)))
;; ====================server======================
;;; recentf
;; ====================recentf=====================
(def-package! recentf
  :commands recentf-mode
  :config
  (def-package! recentf-ext)
  (recentf-mode 1)
  (setq recentf-max-saved-items 100)
  (setq recentf-exclude
        (list "^/tmp/" "^/ssh:" "\\.?ido\\.last$" "\\.revive$" "/TAGS$" "^/var/folders/.+$" "^/mnt.+")))
;; ====================recentf=====================
;;; multiple-cursors
;; ================multiple-cursors================
(def-package! multiple-cursors
  ;; mc/xxx函数都不在mc包中。
  :defer 2
  :config
  (add-to-list 'mc/unsupported-minor-modes 'auto-mark-mode)
  (add-to-list 'mc/unsupported-minor-modes 'highlight-indentation-current-column-mode)
  (bind-key "C-M-," 'mc/mark-previous-like-this)
  (bind-key "C-M-." 'mc/mark-next-like-this)
  (bind-key "<C-M-mouse-1>" 'mc/add-cursor-on-click)
  (smartrep-define-key global-map "C-x"
    '(("C-M-," . mc/unmark-previous-like-this)
      ("C-M-." . mc/unmark-next-like-this)
      ("M-<" . mc/skip-to-previous-like-this)
      ("M->" . mc/skip-to-next-like-this)
      ("C-M-;" . mc/mark-all-like-this)
      ("C-M-/" . mc/edit-lines)
      ("C-M-'" . mc/mark-more-like-this-extended)
      ("M-:" . mc/insert-numbers)
      ("M-\"" . mc/insert-letters)
      ("M-?" . mc/sort-regions)
      ("M-m" . mc/mark-pop)))
  (define-key mc/keymap (kbd "C-`") 'mc-hide-unmatched-lines-mode))
;; ================multiple-cursors================
;;; expand-region
;; =================expand-region==================
(def-package! expand-region
  :bind ("C-M-;" . er/expand-region)
  :config
  (setq expand-region-contract-fast-key ":")
  (setq expand-region-reset-fast-key "C-;"))
;; 在octave中使用会导致emacs假死，原因是octave的function中必须带有end。
;; =================expand-region==================
;;; undo-fu
;; ===================undo-fu======================
(def-package! undo-fu
  :bind (("C-/" . undo-fu-only-undo)
         ("C-M-/" . undo-fu-only-redo)))
;; ===================undo-fu======================
;;; auto-mark
;; ==================auto-mark=====================
(def-package! auto-mark
  :load-path "site-lisp/auto-mark/"
  :config
  (setq auto-mark-command-class-alist
        '((goto-line . jump)
          (indent-for-tab-command . ignore)
          (undo . ignore)
          (goto-last-change . jump)
          (goto-last-change-reverse . jump)))
  ;; 会导致(void-variable last-command-char)错误。
  ;; (setq auto-mark-command-classifiers
  ;;       (list (lambda (command)
  ;;               (if (and (eq command 'self-insert-command)
  ;;                        (eq last-command-char ? ))
  ;;                   'ignore))))
  (defun auto-mark-mode-maybe/after ()
    (when (> (buffer-size) (* 1024 1024))
      (auto-mark-mode -1)))
  (advice-add 'auto-mark-mode-maybe :after #'auto-mark-mode-maybe/after)
  (global-auto-mark-mode 1))
;; ==================auto-mark=====================
;;; visible-mark
;; ================visible-mark====================
(def-package! visible-mark
  :config
  (global-visible-mark-mode 1)
  (setq visible-mark-max 2)
  (defface swint-visible-mark-face-1
    '((t (:background "#666666" :foreground "white")))
    "Face for the mark."
    :group 'visible-mark)
  (setq visible-mark-faces '(visible-mark-active swint-visible-mark-face-1))
  (setq visible-mark-forward-max 2)
  (defface swint-visible-mark-forward-face-1
    '((t (:background "dark red" :foreground "white")))
    "Face for the mark."
    :group 'visible-mark)
  (defface swint-visible-mark-forward-face-2
    '((t (:background "dark green" :foreground "white")))
    "Face for the mark."
    :group 'visible-mark)
  (setq visible-mark-forward-faces '(swint-visible-mark-forward-face-1 swint-visible-mark-forward-face-2))
  (set-face-attribute 'visible-mark-active nil :background "maroon" :foreground "white"))
;; ================visible-mark====================
;;; God-mode
;; ====================God-mode====================
(def-package! god-mode
  :diminish god-local-mode
  :bind ("<S-escape>" . god-local-mode)
  :config
  ;; (global-set-key (kbd "<escape>") 'god-mode-all)
  (defun my-update-cursor ()
    (setq cursor-type (if (or god-local-mode buffer-read-only)
                          'bar
                        'box)))
  (add-hook 'god-mode-enabled-hook 'my-update-cursor)
  (add-hook 'god-mode-disabled-hook 'my-update-cursor)
  (add-to-list 'god-exempt-major-modes 'dired-mode))
;; ====================God-mode====================
;;; elisp-slime-nav
;; =================elisp-slime-nav================
(def-package! help
  :commands help-command
  :config
  (define-key 'help-command (kbd "C-l") 'find-library)
  (define-key 'help-command (kbd "C-f") 'find-function)
  (define-key 'help-command (kbd "C-k") 'find-function-on-key)
  (define-key 'help-command (kbd "C-v") 'find-variable)
  (define-key 'help-command (char-to-string help-char) nil))
(def-package! elisp-slime-nav
  :diminish elisp-slime-nav-mode
  :commands elisp-slime-nav-mode
  :init
  (dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook))
    (add-hook hook 'elisp-slime-nav-mode))
  :config
  (setq eval-expression-debug-on-error nil)
  (define-key elisp-slime-nav-mode-map (kbd "C-c C-,") 'elisp-slime-nav-find-elisp-thing-at-point)
  (define-key elisp-slime-nav-mode-map (kbd "C-c C-.") 'pop-tag-mark)
  (define-key elisp-slime-nav-mode-map (kbd "C-c C-/") 'elisp-slime-nav-describe-elisp-thing-at-point)
  (define-key elisp-slime-nav-mode-map (kbd "M-.") nil)
  (define-key elisp-slime-nav-mode-map (kbd "M-,") nil)
  (define-key elisp-slime-nav-mode-map (kbd "C-c C-d d") nil)
  (define-key elisp-slime-nav-mode-map (kbd "C-c C-d C-d") nil))
;; =================elisp-slime-nav================
;;; drag stuff
;; ===================drag stuff===================
(def-package! drag-stuff
  :diminish drag-stuff-mode
  :bind (("M-P" . drag-stuff-up)
         ("M-N" . drag-stuff-down)
         ("M-B" . drag-stuff-left)
         ("M-F" . drag-stuff-right))
  :config
  (drag-stuff-global-mode t)
  ;; 重新定义drag-stuff-define-keys函数，取消"M+方向键"的快捷键。
  (defun drag-stuff-define-keys/override ()
    "Defines keys for `drag-stuff-mode'."
    (define-key drag-stuff-mode-map (drag-stuff--kbd 'up) nil)
    (define-key drag-stuff-mode-map (drag-stuff--kbd 'down) nil)
    (define-key drag-stuff-mode-map (drag-stuff--kbd 'left) nil)
    (define-key drag-stuff-mode-map (drag-stuff--kbd 'right) nil))
  (advice-add 'drag-stuff-define-keys :override #'drag-stuff-define-keys/override))
;; ===================drag stuff===================
;;; popup-kill-ring
;; ================popup-kill-ring=================
(def-package! popup-kill-ring
  :bind ("M-Y" . popup-kill-ring)
  :config
  (setq popup-kill-ring-interactive-insert nil)
  (setq popup-kill-ring-popup-width 30)
  (setq popup-kill-ring-popup-margin-left 0)
  (setq popup-kill-ring-popup-margin-right 0)
  (setq popup-kill-ring-timeout 0.5))
;; ================popup-kill-ring=================
;;; pos-tip
;; ===================pos-tip======================
(def-package! pos-tip
  :commands pos-tip-show
  :config
  ;; 使用Gtk+ tooltip需配置x-gtk-use-system-tooltips，修改~/.emacs.d/gtkrc配置字体。
  (setq x-gtk-use-system-tooltips t))
;; ===================pos-tip======================
;;; elmacro
;; ===================elmacro======================
(def-package! elmacro
  ;; F3开始录制宏，再次F3插入计数，F4停止录制。
  ;; elmacro-show-last-macro将宏转换为elisp。
  :commands elmacro-mode)
;; ===================elmacro======================
;;; hungry-delete
;; ===================hungry-delete================
(def-package! hungry-delete
  :diminish hungry-delete-mode
  :config
  (add-to-list 'hungry-delete-except-modes 'dired-mode)
  (add-to-list 'hungry-delete-except-modes 'ein:notebook-mode)
  (global-hungry-delete-mode))
;; ===================hungry-delete================
;;; fcitx
;; ======================fcitx=====================
(def-package! fcitx
  :commands swint-fcitx-setup
  :init
  (if (and (fboundp 'daemonp) (daemonp))
      (add-hook 'after-make-frame-functions 'swint-fcitx-setup)
    (add-hook 'after-init-hook 'swint-fcitx-setup))
  :config
  ;; 需设置XMODIFIERS=@im=fcitx才能使用fcitx
  (defvar swint-fcitx-setup-done nil)
  (defun swint-fcitx-setup (&optional frame)
    (when (and (display-graphic-p frame) (not swint-fcitx-setup-done))
      (fcitx-prefix-keys-add "M-s" "M-g" "M-O")
      (fcitx-aggressive-setup)
      (fcitx-isearch-turn-on)
      (fcitx--defun-maybe "sdcv")
      (setq swint-fcitx-setup-done t))))
;; ======================fcitx=====================
;;; aggressive-indent
;; ================aggressive-indent===============
(def-package! aggressive-indent
  :diminish aggressive-indent-mode
  :config
  (add-to-list 'aggressive-indent-excluded-modes 'asm-mode)
  (global-aggressive-indent-mode 1))
;; ================aggressive-indent===============
;;; clean-aindent-mode
;; ===============clean-aindent-mode===============
(def-package! clean-aindent-mode
  :config
  (clean-aindent-mode t)
  (setq clean-aindent-is-simple-indent t)
  (define-key global-map (kbd "RET") 'newline-and-indent)
  (define-key clean-aindent-mode--keymap (kbd "M-DEL") '(lambda ()
                                                          (interactive)
                                                          (if (or paredit-mode
                                                                  paredit-everywhere-mode)
                                                              (call-interactively 'swint-backward-kill-word)
                                                            (call-interactively 'clean-aindent--bsunindent)))))
;; RET：自动清除white space，光标停留在前一行indentation处。
;; M-DEL：unindent，回到前一行indentation处。
;; ===============clean-aindent-mode===============
;;; multifiles
;; ===================multifiles===================
(def-package! multifiles
  :bind ("M-g m" . mf/mirror-region-in-multifile))
;; ===================multifiles===================
;;; ztree
;; =====================ztree======================
(def-package! ztree-diff
  :bind ("M-g z" . ztree-diff))
;; =====================ztree======================
;;; which-key
;; ====================which-key===================
(def-package! which-key
  :diminish which-key-mode
  :defer 2
  :config
  (which-key-mode)
  (which-key-setup-side-window-right-bottom)
  (setq which-key-sort-order 'which-key-description-order)
  (bind-key "M-s x" 'which-key-show-major-mode)
  (bind-key "M-s X" 'which-key-show-minor-mode-keymap)
  ;; 默认C-h启用describe-prefix-bindings。
  (defun which-key-show-standard-help/override (&optional _)
    (interactive)
    (let ((which-key-inhibit t))
      (which-key--hide-popup-ignore-command)
      (helm-descbinds (kbd (which-key--current-key-string)))))
  (advice-add 'which-key-show-standard-help :override
              #'which-key-show-standard-help/override))
;; ====================which-key===================
;;; backup
;; ======================backup====================
(def-package! git-timemachine
  :bind ("M-s M-b" . git-timemachine))
(def-package! backup-walker
  :bind ("M-s M-B" . backup-walker-start))
;; ======================backup====================
;;; visual-regexp
;; ===================visual-regexp================
(def-package! visual-regexp
  :bind (("M-s r" . vr/query-replace)
         ("M-s R" . vr/replace)
         ("M-s C-;" . vr/mc-mark)))
;; ===================visual-regexp================
;;; vlf
;; =======================vlf======================
(def-package! vlf
  :bind (:map dired-mode-map
              ("C-c v" . dired-vlf))
  :init
  ;; Enable vlf when opening files bigger than 100MB.
  (setq large-file-warning-threshold 100000000)
  :config
  (def-package! vlf-setup)
  (smartrep-define-key vlf-mode-map ""
    '(("n" . vlf-next-batch)
      ("p" . vlf-prev-batch)))
  (setq vlf-application 'dont-ask)
  (add-to-list 'vlf-forbidden-modes-list 'pdf-view-mode))
;; =======================vlf======================
;;; easy-kill
;; =====================easy-kill==================
(def-package! easy-kill
  :bind ("M-w" . kill-ring-save)
  :init
  (global-set-key [remap kill-ring-save] 'easy-kill)
  (global-set-key [remap mark-sexp] 'easy-mark)
  :config
  (define-key easy-kill-base-map (kbd "C-w") 'easy-kill-region)
  (define-key easy-kill-base-map (kbd ",") 'easy-kill-shrink)
  (define-key easy-kill-base-map (kbd ".") 'easy-kill-expand))
;; M-w ?: help 查看M-w prefix快捷键。
;; =====================easy-kill==================
;;; smex
;; ======================smex======================
(def-package! smex
  :bind (("M-s M-x" . smex)
         ("M-s M-X" . smex-major-mode-commands)))
;; ======================smex======================
;;; bm
;; =======================bm=======================
(def-package! bm
  :commands (bm-toggle
             bm-previous
             bm-next)
  :init
  (smartrep-define-key global-map "C-x"
    '(("C-'" . bm-toggle)
      ("'" . bm-previous)
      ("\"" . bm-next)))
  (setq bm-restore-repository-on-load t)
  :config
  (setq-default bm-buffer-persistence t)
  (add-hook 'find-file-hook 'bm-buffer-restore)
  (add-hook 'kill-buffer-hook 'bm-buffer-save)
  (add-hook 'kill-emacs-hook '(lambda ()
                                (bm-buffer-save-all)
                                (bm-repository-save)))
  (add-hook 'after-save-hook 'bm-buffer-save)
  (add-hook 'after-revert-hook 'bm-buffer-restore)
  (setq bm-cycle-all-buffers nil)
  (setq bm-highlight-style 'bm-highlight-only-fringe)
  (bm-buffer-restore-all))
;; =======================bm=======================
;;; helm-bm
;; ====================helm-bm=====================
(def-package! helm-bm
  :bind ("C-M-'" . helm-bm)
  :config
  (defun helm-bm-action-switch-to-buffer/override (candidate)
    "Switch to buffer of CANDIDATE."
    (helm-bm-with-candidate candidates
      (helm-switch-persp/buffer bufname)
      (goto-char (point-min))
      (forward-line (1- lineno))))
  (advice-add 'helm-bm-action-switch-to-buffer :override
              #'helm-bm-action-switch-to-buffer/override))
;; ====================helm-bm=====================
;;; operate-on-number
;; ================operate-on-number===============
;; 两种操作方式：C-= 计算符号，支持C-u前缀数字；C-= = 依次确定计算符号和数字。
(def-package! operate-on-number
  :commands (apply-operation-to-number-at-point
             operate-on-number-at-point)
  :init
  (smartrep-define-key global-map "C-="
    '(("+" . apply-operation-to-number-at-point)
      ("-" . apply-operation-to-number-at-point)
      ("*" . apply-operation-to-number-at-point)
      ("/" . apply-operation-to-number-at-point)
      ("\\" . apply-operation-to-number-at-point)
      ("^" . apply-operation-to-number-at-point)
      ("<" . apply-operation-to-number-at-point)
      (">" . apply-operation-to-number-at-point)
      ("#" . apply-operation-to-number-at-point)
      ("%" . apply-operation-to-number-at-point)
      ("=" . operate-on-number-at-point))))
;; ================operate-on-number===============
;;; goto-last-change
;; =================goto-last-change===============
(def-package! goto-chg
  :bind ("M-?" . swint-goto-last-change)
  :config
  (defun swint-goto-last-change ()
    (interactive)
    (condition-case nil
        (progn (call-interactively 'goto-last-change)
               (setq last-command 'goto-last-change))
      (error
       (message "Buffer has not been changed")))
    ;; (invoke-prefix-command)
    ;; (initialize-event-loop)
    ;; The form (condition-case ERR FORM (quit QUIT-HANDLER))
    ;; is there to catch C-g key presses and make sure that
    ;; finalization code is run.
    (condition-case e
        (smartrep-read-event-loop
         '(("M-?" . goto-last-change)
           ("M-/" . goto-last-change-reverse)))
      (quit nil))
    ;; (finalize-event-loop)
    ))
;; =================goto-last-change===============
;;; Proced
;; =====================Proced=====================
(def-package! proced
  :commands proced-process-attributes
  :bind ("C-M-4" . proced)
  :config
  (define-key proced-mode-map (kbd "q") 'kill-buffer-and-window)
  ;; Proced自动更新，10秒。
  (defun proced-settings ()
    (proced-toggle-auto-update 1))
  (add-hook 'proced-mode-hook 'proced-settings))
;; =====================Proced=====================
;;; bbyac
;; =====================bbyac======================
(def-package! bbyac
  :diminish bbyac-mode
  :bind (("M-g M-u" . bbyac-expand-substring)
         ("M-g M-U" . bbyac-expand-symbols))
  :config
  ;; (bbyac-global-mode 1)
  (setq browse-kill-ring-display-style 'one-line))
;; =====================bbyac======================
;;; vimish-fold
;; ==================vimish-fold===================
(def-package! vimish-fold
  :bind (("C-x C-`" . vimish-fold)
         ("C-x C-~" . vimish-fold-delete)))
;; ==================vimish-fold===================
;;; clipmon
;; ====================clipmon=====================
(def-package! clipmon
  :after easy-kill
  :config
  (bind-key "M-g w" 'clipmon-mode)
  (bind-key "M-g M-w" 'clipmon-autoinsert-toggle)
  (clipmon-mode 1)
  (advice-add 'clipmon-mode-start :after #'(lambda () (xclipmon-mode 0)))
  (advice-add 'clipmon-mode-stop :after #'(lambda () (xclipmon-mode 1)))
  ;; 原clipmon--get-selection中文乱码，另有时yank内容不更新。修改后clipmon-autoinsert失效。
  (advice-add 'clipmon--get-selection :override #'(lambda () (current-kill 0)))
  (defvar xclipmon-process nil)
  (define-minor-mode xclipmon-mode
    "Toggle xclipmon.sh - watch system clipboard, add changes to kill ring."
    :global t
    :lighter ""
    (if (not xclipmon-mode)
        (ignore-errors (kill-process xclipmon-process))
      (setq xclipmon-process (start-process-shell-command
                              "xclipmon" "*xclipmon*" "xclipmon.sh"))
      (set-process-query-on-exit-flag xclipmon-process nil))))
;; ====================clipmon=====================
;;; volatile-highlights
;; ==============volatile-highlights===============
(def-package! volatile-highlights
  :diminish volatile-highlights-mode
  :config
  (volatile-highlights-mode t))
;; ==============volatile-highlights===============
;;; quickrun
;; ===================quickrun=====================
(def-package! quickrun
  :bind (("M-s q" . swint-quickrun)
         ("M-s Q" . quickrun-shell))
  :config
  (defun swint-quickrun ()
    (interactive)
    (if mark-active
        (call-interactively 'quickrun-region)
      (call-interactively 'quickrun))))
;; ===================quickrun=====================
;;; highlight-symbol
;; ================highlight-symbol================
(def-package! highlight-symbol
  :commands (highlight-symbol-prev
             highlight-symbol-next
             highlight-symbol-at-point
             highlight-symbol-get-symbol)
  :init
  (smartrep-define-key global-map "M-s"
    '(("," . highlight-symbol-prev)
      ("." . highlight-symbol-next)
      (";" . highlight-symbol-at-point)))
  :config
  (setq highlight-symbol-foreground-color "gray30"))
;; ================highlight-symbol================
;;; auto-highlight-symbol
;; =============auto-highlight-symbol==============
(def-package! auto-highlight-symbol
  :diminish auto-highlight-symbol-mode
  :defer 2
  :config
  (global-auto-highlight-symbol-mode t)
  (set-face-attribute 'ahs-face nil
                      :foreground "white"
                      :background "DarkMagenta")
  (set-face-attribute 'ahs-plugin-whole-buffer-face nil
                      :foreground "white"
                      :background "DarkBlue")
  (add-to-list 'ahs-modes 'octave-mode)
  (add-to-list 'ahs-modes 'gnuplot-mode)
  (add-to-list 'ahs-modes 'graphviz-dot-mode)
  (add-to-list 'ahs-modes 'arduino-mode)
  ;; C-u for whole buffer.
  (define-key auto-highlight-symbol-mode-map (kbd "M-s /") 'ahs-edit-mode)
  (define-key auto-highlight-symbol-mode-map (kbd "M-<left>") nil)
  (define-key auto-highlight-symbol-mode-map (kbd "M-<right>") nil)
  (define-key auto-highlight-symbol-mode-map (kbd "M-S-<left>") nil)
  (define-key auto-highlight-symbol-mode-map (kbd "M-S-<right>") nil)
  (define-key auto-highlight-symbol-mode-map (kbd "M--") nil)
  (define-key auto-highlight-symbol-mode-map (kbd "C-x C-'") nil)
  (define-key auto-highlight-symbol-mode-map (kbd "C-x C-a") nil))
;; =============auto-highlight-symbol==============
;;; dumb-jump
;; ==================dumb-jump=====================
(def-package! dumb-jump
  :commands (dumb-jump-go
             dumb-jump-back
             dumb-jump-quick-look)
  :init
  (smartrep-define-key global-map "C-x"
    '(("," . dumb-jump-go)
      ("." . dumb-jump-back)
      ("/" . dumb-jump-quick-look))))
;; ==================dumb-jump=====================
;;; diff-hl
;; ===================diff-hl======================
(def-package! diff-hl
  :defer 2
  :config
  ;; 默认快捷键以C-x v为前缀。
  (smartrep-define-key global-map "C-x v"
    '((";" . diff-hl-mark-hunk)
      ("," . diff-hl-previous-hunk)
      ("." . diff-hl-next-hunk)
      ("/" . diff-hl-revert-hunk)))
  (global-diff-hl-mode)
  ;; 无需保存即显示diff，造成大文件卡顿。
  ;; (diff-hl-flydiff-mode)
  ;; 只有当前diff-hl-dired-update进程结束后才启动新进程
  (defun diff-hl-dired-update/around (orig-fn &rest args)
    (unless (and (buffer-live-p diff-hl-dired-process-buffer)
                 (get-buffer-process diff-hl-dired-process-buffer))
      (apply orig-fn args)))
  (advice-add 'diff-hl-dired-update :around #'diff-hl-dired-update/around)
  (add-hook 'dired-mode-hook 'diff-hl-dired-mode)
  ;; 在已有dired-mode中开启diff-hl-dired-mode。
  (dolist (buf (cl-remove-if-not (lambda (x)
                                   (equal (buffer-mode x) 'dired-mode))
                                 (buffer-list)))
    (with-current-buffer buf
      (diff-hl-dired-mode)
      (diff-hl-dired-update)))
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
  (set-face-attribute 'diff-hl-insert nil
                      :foreground "gray"
                      :background "dark green")
  (set-face-attribute 'diff-hl-delete nil
                      :foreground "gray"
                      :background "dark red")
  (set-face-attribute 'diff-hl-change nil
                      :foreground "gray"
                      :background "dark blue"))
;; ===================diff-hl======================
;;; evil-nerd-commenter
;; =============evil-nerd-commenter================
(def-package! evil-nerd-commenter
  :bind ("M-:" . evilnc-comment-or-uncomment-lines))
;; =============evil-nerd-commenter================
;;; markdown-mode
;; =================markdown-mode==================
(def-package! markdown-mode
  :commands (markdown-mode
             gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init
  (add-hook 'markdown-mode-hook 'markdown-display-inline-images)
  :config
  (setq markdown-open-image-command "feh.sh"
        markdown-max-image-size '(640 . 480))
  (define-key markdown-mode-map (kbd "C-c C-x C-M-v") 'markdown-display-inline-images)
  (define-key markdown-mode-map (kbd "C-c C-x C-v") 'markdown-toggle-inline-images)
  (define-key markdown-mode-map (kbd "C-c j") 'markdown-follow-thing-at-point))
;; =================markdown-mode==================
;;; highlight-indentation
;; =============highlight-indentation==============
(def-package! highlight-indentation
  :diminish (highlight-indentation-mode
             highlight-indentation-current-column-mode)
  :init
  (add-hook 'prog-mode-hook 'highlight-indentation-current-column-mode))
;; =============highlight-indentation==============
;;; rainbow-mode
;; =================rainbow-mode===================
(def-package! rainbow-mode
  :diminish rainbow-mode
  :commands rainbow-mode
  :init
  (dolist (hook '(web-mode-hook
                  css-mode-hook
                  html-mode-hook
                  emacs-lisp-mode-hook))
    (add-hook hook 'rainbow-mode)))
;; =================rainbow-mode===================
;;; pass
;; =====================pass=======================
(def-package! pass
  :bind ("M-g s" . pass)
  :config
  (setq pass-show-keybindings nil))
(def-package! helm-pass
  :bind ("M-g M-s" . helm-pass))
;; =====================pass=======================
;;; sudo
;; =====================sudo=======================
(def-package! sudo-edit
  ;; 需新建~/.ssh/sockets文件夹。
  :commands (sudo-edit
             sudo-dired)
  :config
  ;; M(dired-do-chmod)改变权限；O(dired-do-chown)改变owner；G(dired-do-chgrp)改变group。
  (defun sudo-dired ()
    (interactive)
    (require 'tramp)
    (let ((dir (expand-file-name default-directory)))
      (if (string-match "^/sudo:" dir)
          (user-error "Already in sudo")
        (dired (concat "/sudo::" dir))))))
;; =====================sudo=======================
;;; gnuplot
;; ====================gnuplot=====================
(def-package! gnuplot-mode
  :mode ("\\.\\(gp\\|gnuplot\\)$" . gnuplot-mode)
  :config
  (define-key gnuplot-mode-map (kbd "C-c C-v") 'swint-open-output-file))
;; ====================gnuplot=====================
;;; graphviz-dot-mode
;; ================graphviz-dot-mode===============
(def-package! graphviz-dot-mode
  :mode ("\\.dot\\'" . graphviz-dot-mode)
  :config
  (define-key graphviz-dot-mode-map (kbd "C-c C-c") 'compile)
  (define-key graphviz-dot-mode-map (kbd "C-c C-v") 'swint-open-output-file))
;; ================graphviz-dot-mode===============
;;; ido
;; ======================ido=======================
(def-package! ido
  :defer t
  :config
  (setq ido-auto-merge-delay-time 0.7
        ido-default-buffer-method 'raise-frame
        ido-default-file-method 'raise-frame
        ido-enable-flex-matching t
        ido-file-extensions-order nil
        ido-use-virtual-buffers nil)
  (custom-set-faces '(ido-first-match ((t (:foreground "yellow" :weight bold))))
                    '(ido-only-match ((((class color)) (:foreground "DeepSkyBlue1" :weight bold))))
                    '(ido-subdir ((t (:foreground "green")))))
  (setq ido-ignore-buffers '("\\`Enjoy\\ Music\\'" "\\`\\*Inferior\\ Octave\\*\\'" "\\`\\*Ibuffer\\*\\'" "\\`\\*MATLAB\\*\\'" "\\`\\*shell\\*\\'" "\\`\\*calculator\\*\\'" "\\`\\*Calendar\\*\\'" "\\`\\*Compile\\=Log\\*\\'" "\\`\\*Completions\\*\\'" "\\`\\*sdcv\\*\\'" "\\`\\*scratch\\*\\'" "\\`\\*Process\\ List\\*\\'" "\\`\\*toc\\*\\'" "\\`\\*helm.*\\*\\'" "\\`\\*Helm.*\\*\\'" "\\`\\*buffer-selection\\*\\'" "\\`\\*Disabled\\ Command\\*\\'" "\\`\\*Mingus\\*\\'" "\\`\\*compilation\\*\\'" "\\`\\*Ido\\ Completions\\*\\'" "\\` " "\\`.english-words\\'" "\\`\\*NOX.*\\*\\'" "\\`\\*nox.*\\*\\'")))
;; ======================ido=======================
;;; term-keys
;; ===================term-keys====================
(def-package! term-keys
  :config
  (term-keys-mode t))
;; ===================term-keys====================
;; ===================yaml-mode====================
(def-package! yaml-mode
  :mode ("\\.yml\\'" . yaml-mode))
;; ===================yaml-mode====================
;;; Auto-revert-mode
;; =================Auto-revert-mode===============
(def-package! autorevert
  :diminish auto-revert-mode
  :defer 2
  :config
  (setq auto-revert-verbose nil)
  (setq auto-revert-interval 5)
  ;; 开启后导致修改文件后dired-mode中光标回到开始处。
  (setq global-auto-revert-non-file-buffers t)
  ;; (setq global-auto-revert-ignore-modes '(dired-mode))
  (global-auto-revert-mode 1))
;; =================Auto-revert-mode===============
;;; academic-phrases
;; =================academic-phrases===============
(def-package! academic-phrases
  :bind (("M-s a" . academic-phrases-by-section)
         ("M-s A" . academic-phrases)))
;; =================academic-phrases===============
;;; reftex
;; =====================reftex=====================
(def-package! reftex
  :diminish reftex-mode
  :commands (reftex-mode reftex-label)
  :init
  (dolist (hook '(LaTeX-mode-hook org-mode-hook))
    (add-hook hook (lambda ()
                     (local-set-key (kbd "C-c r") 'reftex-mode))))
  :config
  ;; 对元素的引用(reference)：C-c ( 添加label，C-c ) 引用label
  ;; 对文献的引用(citation)：C-c [ reftex-citation，C-c C-x [ org-reftex-citation
  (define-key reftex-mode-map (kbd "C-c r") 'reftex-parse-all)
  (setq reftex-plug-into-AUCTeX t
        reftex-toc-split-windows-horizontally t
        reftex-toc-split-windows-fraction 0.2))
;; =====================reftex=====================
;;; eaf
;; ======================eaf=======================
(def-package! eaf
  :load-path "site-lisp/emacs-application-framework/"
  :commands eaf-open
  :bind (:map dired-mode-map
              ("E" . eaf-open-this-from-dired))
  :custom
  (eaf-grip-token "2b9cd942f6960d763364607f258f45196b55c660")
  :config
  (advice-add 'eaf-open :before #'(lambda (url &optional app-name arguments open-always)
                                    (unless (equal (bound-and-true-p pyvenv-virtual-env-name) "py3")
                                      (pyvenv-activate (format "%s/%s" (pyvenv-workon-home) "py3")))))
  (add-hook 'eaf-mode-hook (lambda ()
                             (set (make-local-variable 'ring-bell-function) 'ignore))))
;; ======================eaf=======================
;;; annot
;; =====================annot======================
(def-package! annot
  :load-path "site-lisp/annot/src/"
  :bind (("M-g ;" . annot-edit/add)
         ("M-g :" . annot-remove)
         ("M-g M-;" . annot-add-image))
  :config
  (setq annot-directory "~/org/.annot")
  ;; 与volatile-highlights-mode有冲突。
  (vhl/unload-extension 'kill)
  (vhl/unload-extension 'delete)
  (dolist (buf (cl-remove-if-not (lambda (x)
                                   (buffer-file-name x))
                                 (buffer-list)))
    (with-current-buffer buf (annot-load-annotations)))
  (define-key ctl-x-map "\C-a" nil)
  (define-key ctl-x-map "\C-r" 'swint-counsel-history)
  (define-key ctl-x-map "\C-i" 'cleanup-buffer))
;; =====================annot======================
;;; insert-translated-name
;; ============insert-translated-name==============
(def-package! insert-translated-name
  :load-path "site-lisp/insert-translated-name/"
  :bind (("M-g d" . insert-translated-name-replace)
         ("M-g D" . insert-translated-name-insert))
  :config
  (setq insert-translated-name-default-style "origin"))
;; ============insert-translated-name==============
;;; awesome-tab
;; =================awesome-tab====================
(def-package! awesome-tab
  :load-path "site-lisp/awesome-tab/"
  :commands awesome-tab-mode
  :config
  ;; (awesome-tab-mode t)
  (setq awesome-tab-cycle-scope 'tabs)
  (setq awesome-tab-common-group-name "i")
  (setq awesome-tab-buffer-groups-function 'swint-awesome-tab-buffer-groups)
  (defun swint-awesome-tab-buffer-groups ()
    (list
     (cond
      ((bound-and-true-p persp-mode)
       (cl-loop for key in (nreverse (cons  "i" (nreverse (delete "i" (persp-names)))))
                until (member (current-buffer)
                              (persp-buffers (gethash key (perspectives-hash))))
                finally return key))
      (t "i")))))
;; =================awesome-tab====================
;;; idf-mode
;; ==================idf-mode======================
(def-package! idf-mode
  :load-path "site-lisp/idf-mode/"
  :mode ("\\.[iI][dD][fF]\\'" . idf-mode)
  :config
  (smartrep-define-key idf-mode-map "C-c"
    '(("p" . idf-prev-object)
      ("n" . idf-next-object)
      ("f" . idf-next-type)
      ("b" . idf-prev-type)))
  (define-key idf-mode-map (kbd "C-c C-,") 'idf-find-object-at-point)
  (define-key idf-mode-map (kbd "C-c C-.") 'pop-tag-mark)
  (define-key idf-mode-map (kbd "C-c '") 'idf-show-ctable))
;; ==================idf-mode======================
;;; gnu-elpa-keyring-update
;; ==========gnu-elpa-keyring-update===============
(def-package! gnu-elpa-keyring-update
  ;; 解决GPG公钥过期的问题。
  ;; (setq package-check-signature nil) ;临时关闭验证，package-install安装。
  :commands gnu-elpa-keyring-update)
;; ==========gnu-elpa-keyring-update===============
;;; rg
;; ===================rg===========================
(def-package! rg
  :commands rg-run
  :bind ("M-s g" . rg-menu)
  :config
  (rg-enable-default-bindings (kbd "M-s g"))
  (setq rg-executable (executable-find "rga"))
  (setq rg-default-alias-fallback "everything")
  (define-key rg-mode-map (kbd "C-p") 'compilation-previous-error)
  (define-key rg-mode-map (kbd "C-n") 'compilation-next-error)
  (define-key rg-mode-map (kbd "M-p") 'rg-prev-file)
  (define-key rg-mode-map (kbd "M-n") 'rg-next-file)
  (define-key rg-mode-map (kbd "C-j") 'rg-result-open-externally)
  (defun rg-result-open-externally ()
    (interactive)
    (let ((file-name (or (save-excursion
                           (goto-char (line-beginning-position))
                           (when (re-search-forward "File:\\ \\(.*\\)" (line-end-position) t)
                             (match-string-no-properties 1)))
                         (save-excursion (when (re-search-backward "File:\\ \\(.*\\)" nil t)
                                           (match-string-no-properties 1))))))
      (if (string= (ignore-errors (downcase (file-name-extension file-name))) "pdf")
          (start-process "Shell" nil shell-file-name shell-command-switch
                         (concat "qpdfview --unique --search \"" (rg-search-pattern rg-cur-search)
                                 "\" \""
                                 file-name "#" (save-excursion
                                                 (goto-char (line-beginning-position))
                                                 (re-search-forward "Page\\ \\([[:digit:]]+\\):" nil t)
                                                 (match-string-no-properties 1))
                                 "\""))
        (dired-async-shell-command
         (expand-file-name file-name))))))
;; ===================rg===========================
(provide 'setup_packages)
