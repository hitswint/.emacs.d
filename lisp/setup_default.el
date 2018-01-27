;;; Default
;; ==================Default====================
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
(setq inhibit-startup-screen t)         ;禁用启动信息。
(setq visible-bell t)                   ;关闭烦人的出错时的提示声。
(setq mouse-yank-at-point t)            ;支持中键粘贴。
(setq kill-ring-max 200)                ;用一个很大的 kill ring。
(setq next-line-add-newlines t)         ;最后一行自动打开新行。
(setq make-pointer-invisible t)         ;打字时光标不可见。
(setq diary-file "~/org/journal.org.gpg")
(setq disabled-command-function nil)
(setq uniquify-buffer-name-style 'forward)
(setq-default indent-tabs-mode nil)
(setq save-interprogram-paste-before-kill t)
(setq require-final-newline t)
(setq load-prefer-newer t)
(setq shift-select-mode nil)
(setq epa-pinentry-mode 'loopback)      ;使用minibuffer输入密码。
(setq delete-by-moving-to-trash t)
(setq trash-directory "~/.Trash")
(setq tramp-default-method "ssh")
(setq tramp-ssh-controlmaster-options   ;默认设置导致helm启动慢。
      "-o ControlMaster=auto -o ControlPath='tramp.%%C' -o ControlPersist=no")
;; (setq debug-on-error t)
(set-face-attribute 'highlight nil :background "black")
;; ==================Default====================
;;; Keybindings
;; ================Keybindings==================
(global-set-key (kbd "C-x C-M-j") 'speedbar-get-focus)
(global-set-key (kbd "M-,") 'delete-other-windows)
(global-set-key (kbd "M-.") 'delete-window)
(global-set-key (kbd "M-s M-,") 'split-window-vertically)
(global-set-key (kbd "M-s M-.") 'split-window-horizontally)
(global-set-key (kbd "C-M-2") 'run-octave)
(global-set-key (kbd "C-M-@") 'calculator)
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
;; ================Keybindings==================
;;; Local Variables
;; ==============Local Variables================
;; add-file-local-variable/-prop-line分别在尾首加local variables。
;; ==============Local Variables================
(provide 'setup_default)
