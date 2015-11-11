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
;; (global-set-key (kbd "M-M") 'session-jump-to-last-change)
;; Expanded folded secitons as required
(defun le::maybe-reveal ()
  (when (and (or (memq major-mode  '(org-mode outline-mode))
                 (and (boundp 'outline-minor-mode)
                      outline-minor-mode))
             (outline-invisible-p))
    (if (eq major-mode 'org-mode)
        (org-reveal)
      (show-subtree))))
(add-hook 'session-after-jump-to-last-change-hook
          'le::maybe-reveal)
;; ====================打开文件时自动跳转到上次的位置(不好用)================
;; ;; Save point position between sessions
;; (require 'saveplace)
;; (setq-default save-place t)
;; (setq save-place-file (expand-file-name ".places" user-emacs-directory))
;; ====================打开文件时自动跳转到上次的位置(不好用)================
;; =====================desktop-and-session========================
(provide 'setup_desktop_session)
