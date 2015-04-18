;; ============gdb=============
;; 默认打开多窗口会有问题
(setq gdb-many-windows t)
;; 出现问题，在于 gdb-ui过时了，似乎改成gdb-mi。
(defun gdb-or-gud-go ()
  "If gdb isn't running; run gdb, else call gud-go."
  (interactive)
  (if (and gud-comint-buffer
           (buffer-name gud-comint-buffer)
           (get-buffer-process gud-comint-buffer)
           ;; (with-current-buffer gud-comint-buffer (eq gud-minor-mode 'gdb))
           )
      (gud-call (if gdb-active-process "continue" "run") "")
    (gdb (gud-query-cmdline 'gdb))))
(defun gud-kill ()
  "Kill gdb process."
  (interactive)
  ;; 关闭其他四个buffer，其中io buffer会询问
  (kill-buffer (gdb-locals-buffer-name))
  (kill-buffer (gdb-stack-buffer-name))
  (kill-buffer (gdb-breakpoints-buffer-name))
  (kill-buffer (gdb-inferior-io-name))
  ;; 关闭gdb buffer
  (with-current-buffer gud-comint-buffer (comint-skip-input))
  (kill-process (get-buffer-process gud-comint-buffer))
  (delete-window)
  )
;; 关闭gdb的process时，关闭buffer。取自setup_misc中'退出shell时关闭buffer'。
(add-hook 'gdb-mode-hook 'kill-shell-buffer-after-exit t)
;; 直接使用gdb-or-gud-go会显示gud-comint-buffer变量未定义，需要先使用gdb一次，然后才能使用gdb-or-gud-go。
;; 所以先使用C-M-S-g一次，再使用C-M-g。
(global-set-key (kbd "C-M-S-g") 'gdb)
(global-set-key (kbd "C-M-g") 'gdb-or-gud-go)
(global-set-key (kbd "C-S-g") 'gud-kill)
;; (kill-buffer-and-window )
(add-hook 'c-mode-hook '(lambda ()
                          (setq compile-command (concat "gcc " (buffer-name) " -g -o " (file-name-base (buffer-name))))
                          (define-key c-mode-base-map (kbd "C-c C-c") '(lambda () (interactive) (compile compile-command)))
                          (define-key c-mode-base-map (kbd "(") nil)
                          (define-key c-mode-base-map (kbd "{") nil)))
;; ============gdb=============
(provide 'setup_ccmode)
