;; ====================eval-expression=====================
(global-set-key (kbd "C-c =") 'replace-last-sexp)
(defun replace-last-sexp ()
  (interactive)
  (let ((value (eval (preceding-sexp))))
    (kill-sexp -1)
    (insert (format "%s" value))))
;; ====================eval-expression=====================
;; ===============解决C-n卡顿的问题===============
;; 参考http://www.tuicool.com/articles/q6j263
;; 作者说得并不对，问题在于next-line时需要自动调整窗口滚动的数值，进而对较高的行(图片)更好的查看。
;; 两种解决方法：
;; 1. 设置(next-line (&optional arg try-vscroll))中try-vscroll为nil。
;; 2. 设置auto-window-vscroll为nil，C-p C-n都受影响。
;; 下句牺牲对图片的支持，换取流畅的换行速度。
(setq auto-window-vscroll nil)
;; auto-window-vscroll: Non-nil means to automatically adjust `window-vscroll' to view tall lines.
;; ===============解决C-n卡顿的问题===============
;; =========Determination internet status=========
(defun internet-active-p ()
  "Return non-nil if internet can be reached."
  (cond
   (is-lin
    (or (assoc "eth0" (network-interface-list))
        (assoc "wlan0" (network-interface-list))))
   (is-win
    (not (and (equal (vector 0 0 0 0 0) (cdr (assoc "eth0" (network-interface-list))))
              (equal (vector 0 0 0 0 0) (cdr (assoc "wlan0" (network-interface-list)))))))))
;; =========Determination internet status=========
