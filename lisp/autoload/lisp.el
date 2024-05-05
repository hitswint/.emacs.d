;;; eval-expression
;; =================eval-expression================
;;;###autoload
(defun replace-last-sexp ()
  (interactive)
  (let ((value (eval (elisp--preceding-sexp))))
    (kill-sexp -1)
    (insert (format "%s" value))))
;; =================eval-expression================
;;; Determination internet status
;; ==========Determination internet status=========
;;;###autoload
(defun internet-active-p ()
  "Return non-nil if internet can be reached."
  (or (assoc "eth0" (network-interface-list))
      (assoc "wlan0" (network-interface-list))))
;; ==========Determination internet status=========
;;; shadowsocks-proxy-mode
;; =============shadowsocks-proxy-mode=============
(setq socks-server '("Default server" "127.0.0.1" 1080 5))
;;;###autoload
(define-minor-mode shadowsocks-proxy-mode
  "Mode for shadowsocks proxy."
  :global t
  :init-value nil
  :lighter " SS"
  (if shadowsocks-proxy-mode
      (setq url-gateway-method 'socks)
    (setq url-gateway-method 'native)))
;;;###autoload
(define-global-minor-mode global-shadowsocks-proxy-mode
  shadowsocks-proxy-mode shadowsocks-proxy-mode
  :group 'shadowsocks-proxy)
;; =============shadowsocks-proxy-mode=============
;;; ignore-error-wrapper
;; ==============ignore-error-wrapper==============
;;;###autoload
(defun ignore-error-wrapper (fn)
  "Funtion return new function that ignore errors.
   The function wraps a function with `ignore-errors' macro."
  (lexical-let ((fn fn))
    (lambda ()
      (interactive)
      (ignore-errors
        (funcall fn)))))
;; ==============ignore-error-wrapper==============
;;; shutdown-emacs-server
;; =============shutdown-emacs-server==============
;;;###autoload
(defun shutdown-emacs-server ()
  (interactive)
  ;; 若在GUI下，关闭server之前重开一个frame，容纳后续弹窗
  ;; (when (and (not (eq window-system 'x)) x-display-name)
  ;;   (when (not x-display-name) (setq x-display-name (getenv "DISPLAY")))
  ;;   (window-system-initialization)
  ;;   (select-frame (make-frame-on-display x-display-name '((window-system . x)))))
  ;; (let (;; 使弹窗使用dialog-box，而不是minibuffer
  ;;       ;; (last-nonmenu-event nil)
  ;;       )
  ;;   (switch-to-buffer (messages-buffer))
  ;;   (goto-char (point-max)))
  (save-buffers-kill-emacs))
;; =============shutdown-emacs-server==============
