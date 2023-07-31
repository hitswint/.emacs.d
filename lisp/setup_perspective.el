;;; perspective
;; =================perspective=================
(use-package perspective
  ;; 编译流程：
  ;; 1. doom//clean-byte-compiled-files并重启emacs，更新编译包
  ;; 2. doom//byte-compile编译配置文件
  ;; 3. 如出现undefined等提示，运行1命令重新安装perspective
  ;; 4. pdf-tools需要先加载再更新，否则部分函数显示未定义
  :load-path perspective-dir
  :commands (persp-push-current-buffer
             persp-push-current-buffer-to-last
             swint-persp-switch
             swint-load-perspectives)
  :init
  (bind-key "C-8" #'(lambda () (interactive) (swint-persp-switch "8")))
  (bind-key "C-9" #'(lambda () (interactive) (swint-persp-switch "9")))
  (bind-key "C-0" #'(lambda () (interactive) (swint-persp-switch "0")))
  (bind-key "C-7" #'(lambda () (interactive) (swint-persp-switch "i")))
  (bind-key "C-*" #'(lambda () (interactive) (persp-push-current-buffer "8")))
  (bind-key "C-(" #'(lambda () (interactive) (persp-push-current-buffer "9")))
  (bind-key "C-)" #'(lambda () (interactive) (persp-push-current-buffer "0")))
  (bind-key "C-&" #'(lambda () (interactive) (persp-push-current-buffer "i")))
  (bind-key "C-`" 'persp-switch-last)
  (bind-key "<C-escape>" 'persp-switch-last)
  (bind-key "C-~" 'persp-push-current-buffer-to-last)
  (defmacro with-persp-mode-on (&rest body)
    "Switch to the perspective given by NAME while evaluating BODY."
    `(progn (unless (and persp-mode (frame-parameter nil 'swint-persp-loadp))
              (swint-load-perspectives))
            ,@body))
  (defvar swint-persp-loadp nil)
  :config
  (setq persp-initial-frame-name "i"
        persp-modestring-dividers '("" "" "")
        swint-perspectives-saved-file "~/.emacs.d/saved-perspectives.el")
  (set-face-attribute 'persp-selected-face nil :foreground "green" :weight 'bold)
  (defun persp-format-name (name)
    "Format the perspective name given by NAME for display in `persp-modestring'."
    (let ((string-name (format "%s" name)))
      (if (equal name (persp-name (persp-curr)))
          (propertize string-name 'face 'persp-selected-face))))
  (defun persp-activate (persp)
    "Activate the perspective given by the persp struct PERSP."
    (check-persp persp)
    (persp-save)
    (set-frame-parameter nil 'persp--curr persp)
    (persp-set-local-variables (persp-local-variables persp))
    ;; 切换persp会打乱buffer list顺序，因persp-reactivate-buffers后(persp-buffers persp)顺序不对
    (persp-reactivate-buffers
     (mapcar 'get-buffer (cl-remove-if-not
                          (lambda (x)
                            (member x (remq nil (mapcar #'buffer-name (persp-buffers (persp-curr))))))
                          (if (bound-and-true-p helm-mode)
                              (helm-buffer-list) (buffer-list)))))
    (setq buffer-name-history (persp-buffer-history persp))
    (set-window-configuration (persp-window-configuration persp))
    (when (marker-position (persp-point-marker persp))
      (goto-char (persp-point-marker persp)))
    (persp-update-modestring)
    (run-hooks 'persp-activated-hook))
  (defun persp-add-buffer (buffer)
    "Associate BUFFER with the current perspective.

See also `persp-switch' and `persp-remove-buffer'."
    (interactive
     (list
      (let ((read-buffer-function nil))
        (read-buffer "Add buffer to perspective: "))))
    (let ((buffer (get-buffer buffer)))
      (unless (or (not (persp-curr)) (memq buffer (persp-buffers (persp-curr))))
        (push buffer (persp-buffers (persp-curr))))))
;;;; defuns
  ;; ===================defuns==================
  (defun persp-push-current-buffer (name)
    (interactive)
    (with-persp-mode-on
     (let ((persp (gethash name (perspectives-hash))))
       (push (current-buffer) (persp-buffers persp))
       (persp-remove-buffer (current-buffer))
       (persp-switch name))))
  (defun persp-push-current-buffer-to-last ()
    (interactive)
    (with-persp-mode-on
     (persp-push-current-buffer (persp-name (persp-last)))))
  (defun swint-persp-switch (name)
    (with-persp-mode-on
     (if (and (featurep 'helm) helm-alive-p)
         (helm-run-after-exit #'(lambda (swint-persp-name) (persp-switch swint-persp-name)) name)
       (persp-switch name))))
  ;; ===================defuns==================
;;;; 开启时加载perspectives
  ;; ========emacs开启时加载perspectives========
  (defun swint-load-perspectives ()
    ;; 升级到emacs24.5之后，(persp-mode)启动初始化错误，这里重新初始化
    ;; persp-mode启动时初始化，并分配buffer，会导致后续分配错误
    (with-current-buffer "*scratch*"
      (persp-mode 1))
    (remove-hook 'ido-make-buffer-list-hook 'persp-set-ido-buffers)
    (when (load swint-perspectives-saved-file t)
      (cl-loop for x in saved-persps do
               (with-perspective x
                 (persp-reactivate-buffers
                  (remove nil (mapcar #'get-buffer
                                      (symbol-value (intern (format "buffers-in-perspectives-%s" x))))))
                 (ignore-errors (window-state-put
                                 (symbol-value (intern (format "window-configuration-of-persp-%s" x)))
                                 nil t)))))
    (modify-frame-parameters nil '((swint-persp-loadp . t))))
  ;; (add-hook 'desktop-after-read-hook 'swint-load-perspectives)
  ;; ========emacs开启时加载perspectives========
;;;; 关闭时保存perspectives
  ;; ========emacs关闭时保存perspectives========
  (defun swint-save-perspectives ()
    (when (and persp-mode (frame-parameter nil 'swint-persp-loadp))
      (let ((persps-to-save (delete "i" (persp-names))))
        (cl-loop for x in persps-to-save do
                 (with-perspective x
                   (set (intern (format "window-configuration-of-persp-%s" x)) (window-state-get nil t))))
        (with-temp-file swint-perspectives-saved-file
          (insert "(setq saved-persps '" (prin1-to-string persps-to-save) ")\n")
          (insert "(setq persp-projectile-hash '" (prin1-to-string persp-projectile-hash) ")\n")
          (cl-loop for x in persps-to-save do
                   (insert (concat (format "(setq buffers-in-perspectives-%s '" x)
                                   (prin1-to-string
                                    (remove nil (mapcar (lambda (b)
                                                          (let ((bn (buffer-name b)))
                                                            (unless (or (null bn)
                                                                        (string-match "^\\*" bn)
                                                                        (and (string-match "^ " bn)
                                                                             (null (buffer-file-name b))))
                                                              bn)))
                                                        (persp-buffers (gethash x (perspectives-hash))))))
                                   ")\n"
                                   (format "(setq window-configuration-of-persp-%s '" x)
                                   (prin1-to-string
                                    (symbol-value (intern (format "window-configuration-of-persp-%s" x))))
                                   ")\n")))))
      (persp-mode -1)))
  (if (and (fboundp 'daemonp) (daemonp))
      (add-hook 'delete-frame-functions (lambda (frame) (swint-save-perspectives)))
    (add-hook 'kill-emacs-hook 'swint-save-perspectives))
  ;; ========emacs关闭时保存perspectives========
  )
;; =================perspective=================
(provide 'setup_perspective)
