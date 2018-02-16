;;; perspective
;; =================perspective=================
;; 将配置写到:config启动时会有错误。
;; Eager macro-expansion failure:
;; (error "(persp-buffers persp) is not a valid place expression")
(use-package perspective
  :config
  (setq persp-initial-frame-name "i"
        persp-modestring-dividers '("" "" ""))
  (set-face-attribute 'persp-selected-face nil :foreground "green" :weight 'bold))
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
   (mapcar 'get-buffer (remove-if-not
                        (lambda (x)
                          (member x (remq nil (mapcar 'buffer-name (persp-buffers persp)))))
                        (helm-buffer-list))))
  (setq buffer-name-history (persp-buffer-history persp))
  (set-window-configuration (persp-window-configuration persp))
  (goto-char (persp-point-marker persp))
  (persp-update-modestring)
  (run-hooks 'persp-activated-hook))
;; =================perspective=================
;;; setup-and-keybindings
;; ============setup-and-keybindings============
(defun persp-push-current-buffer (name)
  (interactive)
  (let ((persp (gethash name perspectives-hash)))
    (push (current-buffer) (persp-buffers persp))
    (persp-remove-buffer (current-buffer))
    (persp-switch name)))
(defun persp-push-current-buffer-to-last ()
  (interactive)
  (persp-push-current-buffer (persp-name persp-last)))
(defun persp-push-all-buffer-to-init ()
  (interactive)
  (let ((init-persp (gethash persp-initial-frame-name perspectives-hash)))
    (mapcar #'(lambda (element)
                (unless (member element (persp-buffers init-persp))
                  (push element (persp-buffers init-persp))))
            (buffer-list))
    (swint-persp-switch persp-initial-frame-name)))
(defun swint-persp-switch (name)
  (if (and (featurep 'helm) helm-alive-p)
      (helm-run-after-quit #'(lambda (swint-persp-name) (persp-switch swint-persp-name)) name)
    (persp-switch name)))
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
;; ============setup-and-keybindings============
;;; emacs关闭时保存perspectives
;; =========emacs关闭时保存perspectives=========
(setq swint-perspectives-saved-file "~/.emacs.d/saved-perspectives.el")
(defun swint-save-perspectives ()
  (when persp-mode
    (setq persp-last-session (persp-name persp-curr))
    (mapcar #'(lambda (x)
                (swint-persp-switch x)
                (set (intern (format "window-configuration-of-persp-%s" x)) (window-state-get nil t)))
            (persp-names))
    (with-temp-file
        swint-perspectives-saved-file
      (when is-win
        (insert ";;; -*- coding: utf-8; -*-\n"))
      (insert "(setq swint-persp-names '" (prin1-to-string (persp-names)) ")\n")
      (insert "(setq persp-projectile-hash '" (prin1-to-string persp-projectile-hash) ")\n")
      (insert "(setq persp-last-session " (prin1-to-string persp-last-session) ")\n")
      (mapcar #'(lambda (x)
                  (insert (concat (format "(setq buffers-in-perspectives-%s '" x)
                                  (prin1-to-string
                                   (remove nil (mapcar 'buffer-name (elt (gethash x perspectives-hash) 2))))
                                  ")\n"
                                  (format "(setq window-configuration-of-persp-%s '" x)
                                  (prin1-to-string
                                   (symbol-value (intern (format "window-configuration-of-persp-%s" x))))
                                  ")\n")))
              (persp-names)))))
(add-hook 'kill-emacs-hook 'swint-save-perspectives)
;; =========emacs关闭时保存perspectives=========
;;; emacs开启时加载perspectives
;; =========emacs开启时加载perspectives=========
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
  (mapcar #'(lambda (x)
              (senny-persp x)
              (persp-reactivate-buffers
               (remove nil (mapcar 'get-buffer (symbol-value (intern (format "buffers-in-perspectives-%s" x))))))
              (ignore-errors (window-state-put (symbol-value (intern (format "window-configuration-of-persp-%s" x)))
                                               nil t)))
          swint-persp-names)
  (persp-switch persp-last-session))
(add-hook 'desktop-after-read-hook 'swint-load-perspectives)
;; 在不同的persp中关闭同一个buffer时，会产生无效的(persp-point-marker persp)。
(add-hook 'persp-before-switch-hook '(lambda ()
                                       (unless (marker-position (persp-point-marker persp))
                                         (setf (persp-point-marker persp) (point)))))
;; =========emacs开启时加载perspectives=========
(provide 'setup_perspective)
