;;; perspective
;; =================perspective=================
(def-package! perspective
  :config
  (setq persp-initial-frame-name "i"
        persp-modestring-dividers '("" "" ""))
  (set-face-attribute 'persp-selected-face nil :foreground "green" :weight 'bold)
  (defun persp-format-name (name)
    "Format the perspective name given by NAME for display in `persp-modestring'."
    (let ((string-name (format "%s" name)))
      (if (equal name (persp-name persp-curr))
          (propertize string-name 'face 'persp-selected-face))))
  (defun persp-activate (persp)
    "Activate the perspective given by the persp struct PERSP."
    (check-persp persp)
    (persp-save)
    (setq persp-curr persp)
    (persp-set-local-variables (persp-local-variables persp))
    ;; 切换persp会打乱buffer list顺序，因persp-reactivate-buffers后(persp-buffers persp)顺序不对。
    (persp-reactivate-buffers
     (mapcar 'get-buffer (cl-remove-if-not
                          (lambda (x)
                            (member x (remq nil (mapcar 'buffer-name (persp-buffers persp-curr)))))
                          (helm-buffer-list))))
    (setq buffer-name-history (persp-buffer-history persp))
    (set-window-configuration (persp-window-configuration persp))
    (goto-char (persp-point-marker persp))
    (persp-update-modestring)
    (run-hooks 'persp-activated-hook))
  (global-set-key (kbd "C-8") '(lambda () (interactive) (swint-persp-switch "8")))
  (global-set-key (kbd "C-9") '(lambda () (interactive) (swint-persp-switch "9")))
  (global-set-key (kbd "C-0") '(lambda () (interactive) (swint-persp-switch "0")))
  (global-set-key (kbd "C-7") 'persp-push-all-buffer-to-init)
  (global-set-key (kbd "C-*") '(lambda () (interactive) (persp-push-current-buffer "8")))
  (global-set-key (kbd "C-(") '(lambda () (interactive) (persp-push-current-buffer "9")))
  (global-set-key (kbd "C-)") '(lambda () (interactive) (persp-push-current-buffer "0")))
  (global-set-key (kbd "C-&") '(lambda () (interactive) (persp-push-current-buffer "i")))
  (global-set-key (kbd "C-`") '(lambda () (interactive) (swint-persp-switch (persp-name persp-last))))
  (global-set-key (kbd "C-~") 'persp-push-current-buffer-to-last)
;;;; 开启时加载perspectives
  ;; ========emacs开启时加载perspectives========
  (setq swint-perspectives-saved-file "~/.emacs.d/saved-perspectives.el")
  (defmacro senny-persp (name &rest body)
    `(let ((initialize (not (gethash ,name perspectives-hash)))
           (current-perspective persp-curr))
       (persp-switch ,name)
       (when initialize ,@body)
       (setq persp-last current-perspective)))
  (defun swint-load-perspectives ()
    ;; 升级到emacs24.5之后，(persp-mode)启动初始化错误，这里重新初始化。
    (load swint-perspectives-saved-file t)
    (persp-mode t)
    (remove-hook 'ido-make-buffer-list-hook 'persp-set-ido-buffers)
    (mapc #'(lambda (x)
              (senny-persp x)
              (persp-reactivate-buffers
               (remove nil (mapcar #'get-buffer (symbol-value (intern (format "buffers-in-perspectives-%s" x))))))
              (ignore-errors (window-state-put (symbol-value (intern (format "window-configuration-of-persp-%s" x)))
                                               nil t)))
          swint-persp-names)
    (persp-switch persp-last-session))
  (add-hook 'desktop-after-read-hook 'swint-load-perspectives)
  (add-hook 'kill-emacs-hook 'swint-save-perspectives)
  ;; 在不同的persp中关闭同一个buffer时，会产生无效的(persp-point-marker persp)。
  (add-hook 'persp-before-switch-hook '(lambda ()
                                         (unless (marker-position (persp-point-marker persp))
                                           (setf (persp-point-marker persp) (point)))))
  ;; ========emacs开启时加载perspectives========
  )
;; =================perspective=================
(provide 'setup_perspective)
