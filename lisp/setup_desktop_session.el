;; =====================desktop-and-session========================
(require 'session)
(add-hook 'after-init-hook 'session-initialize)
(load "desktop")
(add-to-list 'desktop-modes-not-to-save 'Info-mode)
(add-to-list 'desktop-modes-not-to-save 'info-lookup-mode)
(add-to-list 'desktop-modes-not-to-save 'fundamental-mode)
(setq desktop-path '("~/.emacs.d/"))
(setq desktop-dirname "~/.emacs.d/")
(setq desktop-base-file-name "emacs-desktop")
(desktop-save-mode t)
;; ==========打开文件时自动跳转到上次的位置(不好用)===========
;; ;; Save point position between sessions
;; (require 'saveplace)
;; (setq-default save-place t)
;; (setq save-place-file (expand-file-name ".places" user-emacs-directory))
;; ==========打开文件时自动跳转到上次的位置(不好用)===========
;; =====================desktop-and-session========================
(provide 'setup_desktop_session)
