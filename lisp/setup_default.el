;;; Setup
;; ===================Setup=====================
(fset 'yes-or-no-p 'y-or-n-p)
(global-font-lock-mode t)               ;语法高亮。
(auto-image-file-mode t)                ;打开图片显示功能。
(transient-mark-mode t)                 ;高亮选中得区域。
(show-paren-mode t)                     ;显示括号匹配。
(global-linum-mode -1)
(global-hl-line-mode t)
(fringe-mode)
;; 导致emacs25卡顿。
(menu-bar-mode -1)
(delete-selection-mode t)
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))
(when (fboundp 'horizontal-scroll-bar-mode)
  (horizontal-scroll-bar-mode -1))
(mouse-avoidance-mode 'animate)         ;光标靠近鼠标指针时，让鼠标指针自动让开。
(setq fill-column 80)                   ;默认显示 80列就换行。
(setq visible-bell t)                   ;关闭烦人的出错时的提示声。
(setq mouse-yank-at-point t)            ;支持中键粘贴。
(setq kill-ring-max 200)                ;用一个很大的 kill ring。
(setq next-line-add-newlines t)         ;最后一行自动打开新行。
(setq make-pointer-invisible t)         ;打字时光标不可见。
(setq diary-file "~/org/journal.org.gpg")
(setq disabled-command-function nil)
(setq uniquify-buffer-name-style 'forward)
(setq-default indent-tabs-mode nil)
(setq auto-window-vscroll nil)          ;解决C-n卡顿。
(setq save-interprogram-paste-before-kill t)
(setq require-final-newline t)
(setq load-prefer-newer t)
(setq shift-select-mode nil)
(setq epa-pinentry-mode 'loopback)      ;使用minibuffer输入密码。
(setq delete-by-moving-to-trash t)
(setq display-line-numbers-type t)
(setq trash-directory "~/.Trash")
(setq tramp-default-method "ssh")
(setq tramp-ssh-controlmaster-options   ;默认设置导致helm启动慢。
      "-o ControlMaster=auto -o ControlPath='tramp.%%C' -o ControlPersist=no")
;; (setq debug-on-error t)
(set-face-attribute 'highlight nil :background "black")
(unless noninteractive
  (advice-add #'display-startup-echo-area-message :override #'ignore)
  (setq inhibit-startup-screen t
        inhibit-startup-echo-area-message user-login-name
        inhibit-default-init t
        initial-major-mode 'fundamental-mode
        initial-scratch-message nil))
;; ===================Setup=====================
;;; Keybindings
;; ================Keybindings==================
(global-set-key (kbd "C-x C-M-j") 'speedbar-get-focus)
(global-set-key (kbd "M-,") 'delete-other-windows)
(global-set-key (kbd "M-.") 'delete-window)
(global-set-key (kbd "M-s M-,") 'split-window-vertically)
(global-set-key (kbd "M-s M-.") 'split-window-horizontally)
(global-set-key (kbd "C-M-2") 'run-octave)
(global-set-key (kbd "C-M-@") 'matlab-shell)
(global-set-key (kbd "C-M-6") 'calendar)
(global-set-key (kbd "C-x C-l") 'reposition-window)
(global-set-key (kbd "C-;") 'set-mark-command)
(global-set-key (kbd "C-x r ;") 'rectangle-mark-mode)
(global-set-key [f9] 'menu-bar-mode)
(global-set-key (kbd "S-SPC") 'just-one-space)
(global-set-key (kbd "<S-return>") 'join-line)
(global-set-key (kbd "M-s SPC") 'delete-horizontal-space)
(global-set-key (kbd "C-SPC") nil)
(global-set-key (kbd "<C-M-backspace>") 'backward-kill-sexp)
(define-key lisp-interaction-mode-map (kbd "C-j") nil)
(global-set-key (kbd "<C-prior>") 'previous-user-buffer)
(global-set-key (kbd "<C-next>") 'next-user-buffer)
(global-set-key (kbd "C-x M-d") 'delete-current-buffer-file)
(global-set-key (kbd "C-x M-r") 'rename-current-buffer-file)
(global-set-key (kbd "C-q") 'swint-kill-buffer)
(global-set-key (kbd "M-c") 'toggle-letter-case)
(global-set-key (kbd "M-Q") 'compact-uncompact-block)
(global-set-key (kbd "C-w") 'cut-line-or-region)
(global-set-key [(meta n)] 'window-move-up)
(global-set-key [(meta p)] 'window-move-down)
(global-set-key (kbd "C-x C-i") 'cleanup-buffer)
(global-set-key (kbd "M-g g") 'linum-mode)
(global-set-key [remap goto-line] 'goto-line-with-feedback)
(global-set-key (kbd "M-s M-;") 'comment-or-uncomment-region-or-line)
(global-set-key (kbd "C-a") 'smart-beginning-of-line)
(global-set-key (kbd "M-s -") 'jcs-dashify)
(global-set-key (kbd "M-s _") 'jcs-dashify-underline)
(global-set-key (kbd "C-j") 'open-line-or-new-line-dep-pos)
(global-set-key (kbd "M-m") 'pop-to-mark-command)
(global-set-key (kbd "M-M") 'unpop-to-mark-command)
(global-set-key (kbd "M-s =") '(lambda () (interactive)
                                 (if swint-diff-region-tag
                                     (diff-region-compare-with-b)
                                   (diff-region-tag-selected-as-a))))
(global-set-key (kbd "C-x RET RET") '(lambda () (interactive)
                                       (revert-buffer-with-coding-system 'utf-8)))
(global-set-key (kbd "C-x M-s") 'save-buffer-with-dos2unix)
(global-set-key (kbd "C-x e") 'replace-last-sexp)
(global-set-key (kbd "C-x C-y") 'xsel-paste-primary)
(global-set-key (kbd "M-g =") 'swint-count-words-region)
(global-set-key (kbd "M-g o") 'swint-pandoc-output)
(global-set-key (kbd "M-g t") 'swint-pdftk-output)
(global-set-key (kbd "M-s M-e") 'show-some-last-messages)
(global-set-key (kbd "M-s M-d") 'swint-sdcv-to-tip)
(global-set-key (kbd "M-s M-D") 'swint-sdcv-to-buffer)
(global-set-key (kbd "M-s D") 'swint-online-to-buffer)
(global-set-key (kbd "C-s-<return>") 'urxvt-default-directory)
(global-set-key (kbd "C-s-e") 'tc-open-default-directory)
(global-set-key (kbd "C-x M-,") '(lambda () (interactive) (swint-nutstore-sync "down")))
(global-set-key (kbd "C-x M-.") '(lambda () (interactive) (swint-nutstore-sync "up")))
(global-set-key (kbd "C-x M-/") '(lambda () (interactive) (swint-nutstore-sync "bi")))
(global-set-key (kbd "M-s C-,") '(lambda () (interactive) (swint-bypy-sync)))
(global-set-key (kbd "M-s C-.") '(lambda () (interactive) (swint-bypy-sync t)))
(global-set-key (kbd "M-s C-/") 'swint-unison-sync-backups)
(global-set-key (kbd "M-g C-,") '(lambda () (interactive) (swint-onedrive-sync "down")))
(global-set-key (kbd "M-g C-.") '(lambda () (interactive) (swint-onedrive-sync "up")))
(global-set-key (kbd "M-g C-/") '(lambda () (interactive) (swint-onedrive-sync "bi")))
(global-set-key (kbd "C-S-s") 'swint-pinyin-search-forward)
(global-set-key (kbd "C-S-r") 'swint-pinyin-search-backward)
(global-set-key (kbd "C-x C-<tab>") 'switch-to-minibuffer)
(global-set-key (kbd "C-\\") 'toggle-window-split)
(global-set-key (kbd "C-x O") 'rotate-windows)
(global-set-key (kbd "C-x M-p") 'swint-screenshot)
(global-set-key (kbd "C-x M-P") '(lambda () (interactive) (swint-screenshot t)))
(global-set-key (kbd "C-x p") 'swint-insert-screenshot)
(global-set-key (kbd "C-x P") '(lambda () (interactive) (swint-insert-screenshot t)))
(global-set-key (kbd "M-g v") 'iimage-mode)
(global-set-key (kbd "M-g ,") '(lambda (&optional arg) (interactive "P") (swint-dired-rsync/unison "pull")))
(global-set-key (kbd "M-g .") '(lambda (&optional arg) (interactive "P") (swint-dired-rsync/unison "push")))
(global-set-key (kbd "M-g /") '(lambda (&optional arg) (interactive "P") (swint-dired-rsync/unison "sync")))
(global-set-key (kbd "C-M-q") 'swint-undo-kill-buffer)
(define-key emacs-lisp-mode-map "\e\C-q" nil)
(define-key lisp-interaction-mode-map "\e\C-q" nil)
(define-key prog-mode-map "\e\C-q" nil)
;; ================Keybindings==================
(provide 'setup_default)
