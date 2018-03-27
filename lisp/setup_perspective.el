;;; perspective
;; =================perspective=================
(def-package! perspective
  :commands (persp-push-current-buffer
             persp-push-current-buffer-to-last
             persp-push-all-buffer-to-init
             swint-persp-switch
             swint-load-perspectives)
  :init
  (bind-key "C-8" '(lambda () (interactive) (swint-persp-switch "8")))
  (bind-key "C-9" '(lambda () (interactive) (swint-persp-switch "9")))
  (bind-key "C-0" '(lambda () (interactive) (swint-persp-switch "0")))
  (bind-key "C-7" 'persp-push-all-buffer-to-init)
  (bind-key "C-*" '(lambda () (interactive) (persp-push-current-buffer "8")))
  (bind-key "C-(" '(lambda () (interactive) (persp-push-current-buffer "9")))
  (bind-key "C-)" '(lambda () (interactive) (persp-push-current-buffer "0")))
  (bind-key "C-&" '(lambda () (interactive) (persp-push-current-buffer "i")))
  (bind-key "C-`" '(lambda () (interactive) (swint-persp-switch (persp-name persp-last))))
  (bind-key "C-~" 'persp-push-current-buffer-to-last)
  (defmacro with-persp-mode-on (&rest body)
    "Switch to the perspective given by NAME while evaluating BODY."
    `(progn (unless persp-mode
              (swint-load-perspectives))
            ,@body))
  :config
  (setq persp-initial-frame-name "i"
        persp-modestring-dividers '("" "" "")
        swint-perspectives-saved-file "~/.emacs.d/saved-perspectives.el")
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
                          (if (bound-and-true-p helm-mode)
                              (helm-buffer-list) (buffer-list)))))
    (setq buffer-name-history (persp-buffer-history persp))
    (set-window-configuration (persp-window-configuration persp))
    (goto-char (persp-point-marker persp))
    (persp-update-modestring)
    (run-hooks 'persp-activated-hook))
;;;; defuns
  ;; ===================defuns==================
  (defun persp-push-current-buffer (name)
    (interactive)
    (with-persp-mode-on
     (let ((persp (gethash name perspectives-hash)))
       (push (current-buffer) (persp-buffers persp))
       (persp-remove-buffer (current-buffer))
       (persp-switch name))))
  (defun persp-push-current-buffer-to-last ()
    (interactive)
    (with-persp-mode-on
     (persp-push-current-buffer (persp-name persp-last))))
  (defun persp-push-all-buffer-to-init ()
    (interactive)
    (with-persp-mode-on
     (let ((init-persp (gethash persp-initial-frame-name perspectives-hash)))
       (cl-loop for element in (buffer-list)
                do (unless (member element (persp-buffers init-persp))
                     (push element (persp-buffers init-persp))))
       (swint-persp-switch persp-initial-frame-name))))
  (defun swint-persp-switch (name)
    (with-persp-mode-on
     (if (and (featurep 'helm) helm-alive-p)
         (helm-run-after-exit #'(lambda (swint-persp-name) (persp-switch swint-persp-name)) name)
       (persp-switch name))))
  ;; ===================defuns==================
;;;; 开启时加载perspectives
  ;; ========emacs开启时加载perspectives========
  (defun swint-load-perspectives ()
    ;; 升级到emacs24.5之后，(persp-mode)启动初始化错误，这里重新初始化。
    (load swint-perspectives-saved-file t)
    (persp-mode 1)
    (remove-hook 'ido-make-buffer-list-hook 'persp-set-ido-buffers)
    (cl-loop for x in swint-persp-names do
             (with-perspective x
               (persp-reactivate-buffers
                (remove nil (mapcar #'get-buffer
                                    (symbol-value (intern (format "buffers-in-perspectives-%s" x))))))
               (ignore-errors (window-state-put
                               (symbol-value (intern (format "window-configuration-of-persp-%s" x)))
                               nil t)))))
  ;; (add-hook 'desktop-after-read-hook 'swint-load-perspectives)
  ;; ========emacs开启时加载perspectives========
;;;; 关闭时保存perspectives
  ;; ========emacs关闭时保存perspectives========
  (defun swint-save-perspectives ()
    (when persp-mode
      (cl-loop for x in (persp-names) do
               (swint-persp-switch x)
               (set (intern (format "window-configuration-of-persp-%s" x)) (window-state-get nil t)))
      (with-temp-file swint-perspectives-saved-file
        (insert "(setq swint-persp-names '" (prin1-to-string (persp-names)) ")\n")
        (insert "(setq persp-projectile-hash '" (prin1-to-string persp-projectile-hash) ")\n")
        (cl-loop for x in (persp-names) do
                 (insert (concat (format "(setq buffers-in-perspectives-%s '" x)
                                 (prin1-to-string
                                  (remove nil (mapcar #'buffer-name (elt (gethash x perspectives-hash) 2))))
                                 ")\n"
                                 (format "(setq window-configuration-of-persp-%s '" x)
                                 (prin1-to-string
                                  (symbol-value (intern (format "window-configuration-of-persp-%s" x))))
                                 ")\n"))))))
  (add-hook 'kill-emacs-hook 'swint-save-perspectives)
  ;; 在不同的persp中关闭同一个buffer时，会产生无效的(persp-point-marker persp)。
  (add-hook 'persp-before-switch-hook '(lambda ()
                                         (unless (marker-position (persp-point-marker persp))
                                           (setf (persp-point-marker persp) (point)))))
  ;; ========emacs关闭时保存perspectives========
  )
;; =================perspective=================
(provide 'setup_perspective)
