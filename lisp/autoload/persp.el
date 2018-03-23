;;; setup-and-keybindings
;; ===========setup-and-keybindings===========
;;;###autoload
(defun persp-push-current-buffer (name)
  (interactive)
  (let ((persp (gethash name perspectives-hash)))
    (push (current-buffer) (persp-buffers persp))
    (persp-remove-buffer (current-buffer))
    (persp-switch name)))
;;;###autoload
(defun persp-push-current-buffer-to-last ()
  (interactive)
  (persp-push-current-buffer (persp-name persp-last)))
;;;###autoload
(defun persp-push-all-buffer-to-init ()
  (interactive)
  (let ((init-persp (gethash persp-initial-frame-name perspectives-hash)))
    (mapc #'(lambda (element)
              (unless (member element (persp-buffers init-persp))
                (push element (persp-buffers init-persp))))
          (buffer-list))
    (swint-persp-switch persp-initial-frame-name)))
;;;###autoload
(defun swint-persp-switch (name)
  (if (and (featurep 'helm) helm-alive-p)
      (helm-run-after-exit #'(lambda (swint-persp-name) (persp-switch swint-persp-name)) name)
    (persp-switch name)))
;; ===========setup-and-keybindings===========
;;; 关闭时保存perspectives
;; ========emacs关闭时保存perspectives========
;;;###autoload
(defun swint-save-perspectives ()
  (when persp-mode
    (setq persp-last-session (persp-name persp-curr))
    (mapc #'(lambda (x)
              (swint-persp-switch x)
              (set (intern (format "window-configuration-of-persp-%s" x)) (window-state-get nil t)))
          (persp-names))
    (with-temp-file swint-perspectives-saved-file
      (insert "(setq swint-persp-names '" (prin1-to-string (persp-names)) ")\n")
      (insert "(setq persp-projectile-hash '" (prin1-to-string persp-projectile-hash) ")\n")
      (insert "(setq persp-last-session " (prin1-to-string persp-last-session) ")\n")
      (mapc #'(lambda (x)
                (insert (concat (format "(setq buffers-in-perspectives-%s '" x)
                                (prin1-to-string
                                 (remove nil (mapcar #'buffer-name (elt (gethash x perspectives-hash) 2))))
                                ")\n"
                                (format "(setq window-configuration-of-persp-%s '" x)
                                (prin1-to-string
                                 (symbol-value (intern (format "window-configuration-of-persp-%s" x))))
                                ")\n")))
            (persp-names)))))
;; ========emacs关闭时保存perspectives========
