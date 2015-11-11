;; =======================全屏和最大化==============================
(when is-lin
  ;;启动全屏的快捷键
  (global-set-key [f11] 'my-fullscreen)
  ;;全屏
  (defun my-fullscreen ()
    (interactive)
    (x-send-client-message
     nil 0 nil "_NET_WM_STATE" 32
     '(2 "_NET_WM_STATE_FULLSCREEN" 0)))
  ;;最大化
  (defun my-maximized ()
    (interactive)
    (x-send-client-message
     nil 0 nil "_NET_WM_STATE" 32
     '(2 "_NET_WM_STATE_MAXIMIZED_HORZ" 0))
    (x-send-client-message
     nil 0 nil "_NET_WM_STATE" 32
     '(2 "_NET_WM_STATE_MAXIMIZED_VERT" 0)))
  ;;启动emacs时窗口最大化
  (my-maximized)
  ;;启动窗口大小
  ;;(setq default-frame-alist
  ;;      '((height . 30) (width . 75) (menu-bar-lines . 20) (tool-bar-lines . 0)))
  )
;; =======================全屏和最大化==============================
;; =========================tabbar================================
;; (add-to-list 'load-path "~/.emacs.d/tabbar")
;; (require 'tabbar)
;; (tabbar-mode 1)
;; (global-set-key (kbd "M-2") 'tabbar-backward)
;; (global-set-key (kbd "M-3") 'tabbar-forward)
;; ;; (global-set-key (kbd "C-M-2") 'tabbar-backward-group)
;; ;; (global-set-key (kbd "C-M-3") 'tabbar-forward-group)
;; ;;设置tabbar外观
;; ;;设置默认主题: 字体, 背景和前景颜色，大小
;; (set-face-attribute 'tabbar-default nil
;;                     :family "DejaVu Sans Mono"
;;                     :background "gray80"
;;                     :foreground "gray30"
;;                     :height 0.9
;;                     )
;; ;;设置左边按钮外观：外框框边大小和颜色
;; (set-face-attribute 'tabbar-button nil
;;                     :inherit 'tabbar-default
;;                     :box '(:line-width 1 :color "yellow")
;;                     )
;; ;;设置当前tab外观：颜色，字体，外框大小和颜色
;; (set-face-attribute 'tabbar-selected nil
;;                     :inherit 'tabbar-default
;;                     :foreground "DarkGreen"
;;                     :background "LightGoldenrod"
;;                     :box '(:line-width 1 :color "DarkGoldenrod")
;;                     :overline "black"
;;                     :underline "black"
;;                     :weight 'bold
;;                     )
;; ;;设置非当前tab外观：外框大小和颜色
;; (set-face-attribute 'tabbar-unselected nil
;;                     :inherit 'tabbar-default
;;                     :box '(:line-width 1 :color "#00B2BF")
;;                     )
;; ;; don't show help information,don't show tabbar button
;; (setq
;;  tabbar-scroll-left-help-function nil
;;  tabbar-scroll-right-help-function nil
;;  tabbar-help-on-tab-function nil
;;  tabbar-home-help-function nil
;;  tabbar-buffer-home-button (quote (("") ""))
;;  tabbar-scroll-left-button (quote (("") ""))
;;  tabbar-scroll-right-button (quote (("") "")))
;; ;; user and emacs group
;; (defun my-tabbar-buffer-groups ()
;;   ;; customize to show all normal files in one group
;;   ;;   "Returns the name of the tab group names the current buffer belongs to.
;;   ;; There are two groups: Emacs buffers (those whose name starts with “*”, plus
;;   ;; dired buffers), and the rest.  This works at least with Emacs v24.2 using
;;   ;; tabbar.el v1.7.
;;   (list (cond ((string-equal "*" (substring (buffer-name) 0 1)) "emacs")
;;               ;; ((eq major-mode 'dired-mode) "emacs")
;;               (t "user"))))
;; (setq tabbar-buffer-groups-function 'my-tabbar-buffer-groups)
;; ;; 根据major-mode选择是否打开tabbar
;; (when (require 'tabbar nil t)
;;   ;; Enable tabbars globally:
;;   (tabbar-mode 1)
;;   ;; I use this minor-mode mainly as a global mode (see below):
;;   (define-minor-mode tabbar-on-dired-only-mode
;;     "Display tabbar on terminals and buffers in fundamental mode only."
;;     :init-value t
;;     :lighter nil
;;     :keymap nil
;;     (if tabbar-on-dired-only-mode
;;         ;; filter is enabled
;;         (if (eq major-mode 'dired-mode); <- this can be easily customizable...
;;             (tabbar-local-mode -1)
;;           (tabbar-local-mode 1))
;;       ;; always activate tabbar locally when we disable the minor mode:
;;       (tabbar-local-mode -1)))
;;   (defun tabbar-on-dired-only-mode-on ()
;;     "Turn on tabbar if current buffer is a terminal."
;;     (unless (minibufferp) (tabbar-on-dired-only-mode 1)))
;;   ;; Define a global switch for the mode. Note that this is not set for buffers
;;   ;; in fundamental mode.
;;   ;; I use it 'cause some major modes do not run the
;;   ;; `after-change-major-mode-hook'...
;;   (define-globalized-minor-mode global-tabbar-on-dired-only-mode
;;     tabbar-on-dired-only-mode tabbar-on-dired-only-mode-on)
;;   ;; Eventually, switch on this global filter for tabbars:
;;   (global-tabbar-on-dired-only-mode 1))
;; =========================tabbar================================
;; ==================color-theme===================
;; 用于emacs23以下。
;; ;; (add-to-list 'load-path "~/.emacs.d/color-theme-6.6.0/")
;; (require 'color-theme)
;; (setq color-theme-directory (concat user-emacs-directory "themes/")
;;       color-theme-load-all-themes nil)
;; (color-theme-initialize)
;; ;; (add-to-list 'load-path "~/.emacs.d/color-theme-6.6.0/themes/")
;; (color-theme-tangotango)
;; ;; (color-theme-tango-light)
;; ==================color-theme===================
;; ==============tangotango-theme==============
(load-theme 'tangotango t)
;; ==============tangotango-theme==============
(provide 'setup_appearance)
