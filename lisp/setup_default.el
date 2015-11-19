;; ==================DEFAULT====================
(setq default-major-mode 'text-mode)    ;一打开就起用 text 模式。
(global-font-lock-mode t)               ;语法高亮
(auto-image-file-mode t)                ;打开图片显示功能
(fset 'yes-or-no-p 'y-or-n-p)           ;以 y/n代表 yes/no
(global-linum-mode 0)
(show-paren-mode t)                     ;显示括号匹配
(tool-bar-mode 0)                       ;去掉那个大大的工具栏
(menu-bar-mode 0)                       ;去掉菜单栏
(scroll-bar-mode 0)                     ;去掉滚动条
(mouse-avoidance-mode 'animate)         ;光标靠近鼠标指针时，让鼠标指针自动让开
(require 'hl-line)                      ;光标所在行高亮
(global-hl-line-mode t)
(transient-mark-mode t)                 ;高亮选中得区域
(setq x-select-enable-clipboard t)      ;支持emacs和外部程序的粘贴
(setq frame-title-format "Emacs@ %b")   ;在标题栏提示你目前在什么位置。
(setq default-fill-column 80)           ;默认显示 80列就换行
(setq inhibit-startup-message t)        ;禁用启动信息
(setq visible-bell t)                   ;关闭烦人的出错时的提示声。
(setq mouse-yank-at-point t)            ;支持中键粘贴
(setq kill-ring-max 200)                ;用一个很大的 kill ring
(delete-selection-mode t)
(setq diary-file "~/org/journal.org.gpg")
;; ==================DEFAULT====================
;; ===================快捷键====================
(global-set-key (kbd "C-c `") 'ibuffer)
(global-set-key (kbd "C-c ~") 'speedbar-get-focus)
(global-set-key (kbd "M-,") 'delete-other-windows)
(global-set-key (kbd "M-.") 'delete-window)
(global-set-key (kbd "M-s M-,") 'split-window-vertically)
(global-set-key (kbd "M-s M-.") 'split-window-horizontally)
(global-set-key (kbd "C-M-1") 'eshell)
(global-set-key (kbd "C-M-!") 'shell)
(global-set-key (kbd "C-M-2") 'kid-sdcv-to-buffer)
(global-set-key (kbd "C-M-3") 'run-octave)
(global-set-key (kbd "C-M-#") 'calculator)
(global-set-key (kbd "C-M-5") 'w3m)
(global-set-key (kbd "C-M-6") 'calendar)
(global-set-key (kbd "C-;") 'set-mark-command)
(global-set-key [f9] 'menu-bar-mode)
(global-set-key (kbd "C-SPC") nil)
(define-key lisp-interaction-mode-map (kbd "C-j") nil)
;; ===================快捷键====================
;; ==================smartrep===================
(use-package smartrep
  ;; Enabled automatically.
  :config
  (setq smartrep-mode-line-string-activated nil))
;; ==================smartrep===================
(provide 'setup_default)
