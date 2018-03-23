;;; 全屏和最大化
;; ================全屏和最大化===============
;; 启动全屏的快捷键。
(global-set-key [f11] 'my-fullscreen)
;;全屏。
(defun my-fullscreen ()
  (interactive)
  (x-send-client-message
   nil 0 nil "_NET_WM_STATE" 32
   '(2 "_NET_WM_STATE_FULLSCREEN" 0)))
;;最大化。
(defun my-maximized ()
  (interactive)
  (x-send-client-message
   nil 0 nil "_NET_WM_STATE" 32
   '(2 "_NET_WM_STATE_MAXIMIZED_HORZ" 0))
  (x-send-client-message
   nil 0 nil "_NET_WM_STATE" 32
   '(2 "_NET_WM_STATE_MAXIMIZED_VERT" 0)))
;; 启动emacs时窗口最大化。
;; (my-maximized)
;; 启动窗口大小。
;; (setq default-frame-alist
;;    '((height . 30) (width . 75) (menu-bar-lines . 20) (tool-bar-lines . 0)))
;; ================全屏和最大化===============
;;; tangotango-theme
;; ==============tangotango-theme=============
(def-package! tangotango-theme
  :config
  (load-theme 'tangotango t))
;; ==============tangotango-theme=============
;;; cycle-mini
;; ================cycle-mini=================
(def-package! cycle-mini
  :load-path "site-lisp/cycle-mini/"
  :bind (:map minibuffer-local-completion-map
              ("C-p" . cycle-mini-previous-completion)
              ("C-n" . cycle-mini-next-completion)))
;; ================cycle-mini=================
(provide 'setup_appearance)
