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
;;; 拼音首字母搜索
;; =================拼音首字母搜索=================
;;;###autoload
(defun swint-pinyin-search-forward (&optional bound noerror count)
  (interactive)
  (let ((swint-current-buffer (str-unicode-to-pinyin-initial (buffer-substring-no-properties (point-min) (point-max))))
        (current-point (point))
        (string (car minibuffer-history)))
    (setq string (read-string (format  "Pinyin search(default %s): " string)
                              nil nil string))
    (with-temp-buffer
      (insert swint-current-buffer)
      (goto-char current-point)
      (if (string-match-p string (buffer-substring-no-properties current-point (point-max)))
          (re-search-forward string bound noerror count)
        (progn
          (goto-char (point-min))
          (re-search-forward string bound noerror count)))
      (setq swint-match-end (match-end 0)))
    (goto-char swint-match-end)))
;;;###autoload
(defun swint-pinyin-search-backward (&optional bound noerror count)
  (interactive)
  (let ((swint-current-buffer (str-unicode-to-pinyin-initial (buffer-substring-no-properties (point-min) (point-max))))
        (current-point (point))
        (string (car minibuffer-history)))
    (setq string (read-string (format  "Pinyin search(default %s): " string)
                              nil nil string))
    (with-temp-buffer
      (insert swint-current-buffer)
      (goto-char current-point)
      (if (string-match-p string (buffer-substring-no-properties (point-min) current-point))
          (re-search-backward string bound noerror count)
        (progn
          (goto-char (point-max))
          (re-search-backward string bound noerror count)))
      (setq swint-match-beginning (match-beginning 0)))
    (goto-char swint-match-beginning)))
(define-key minibuffer-local-map (kbd "C-S-s") 'exit-minibuffer)
(define-key minibuffer-local-map (kbd "C-S-r") 'exit-minibuffer)
;; =================拼音首字母搜索=================
;;; shutdown-emacs-server
;; =============shutdown-emacs-server==============
;;;###autoload
(defun shutdown-emacs-server ()
  (interactive)
  ;; 若在GUI下，关闭server之前重开一个frame，容纳后续弹窗
  (when (and (eq window-system 'x) (display-graphic-p x-display-name))
    (select-frame (make-frame-on-display x-display-name '((window-system . x)))))
  (let ((last-nonmenu-event nil)) ;使弹窗使用dialog-box，而不是minibuffer
    (save-buffers-kill-emacs)))
;; =============shutdown-emacs-server==============
