;; ================================perspective====================================
;; 将配置写到:config启动时会有错误。
;; Eager macro-expansion failure:
;; (error "(persp-buffers persp) is not a valid place expression")
(use-package perspective)
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
;; =================模板宏==================
(defmacro senny-persp (name &rest body)
  `(let ((initialize (not (gethash ,name perspectives-hash)))
         (current-perspective persp-curr))
     (persp-switch ,name)
     (when initialize ,@body)
     (setq persp-last current-perspective)))
(defun persp-format-name (name)
  "Format the perspective name given by NAME for display in `persp-modestring'."
  (let ((string-name (format "%s" name)))
    (if (equal name (persp-name persp-curr))
        (propertize string-name 'face 'persp-selected-face))))
(defun persp-update-modestring ()
  "Update `persp-modestring' to reflect the current perspectives.
Has no effect when `persp-show-modestring' is nil."
  (when persp-show-modestring
    (setq persp-modestring
          (append '("")
                  (persp-intersperse (mapcar 'persp-format-name (persp-names)) "")
                  '("")))))
;; Perspective Defuns
(defun senny-persp-last ()
  (interactive)
  (persp-switch (persp-name persp-last)))
;; =================模板宏==================
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
  (let ((init-persp (gethash "i" perspectives-hash)))
    (mapcar #'(lambda (element)
                (unless (member element (persp-buffers init-persp))
                  (push element (persp-buffers init-persp))))
            (buffer-list))
    (swint-persp-switch "i")))
(defun swint-persp-switch (name)
  (if (if (boundp 'helm--minor-mode)
          helm--minor-mode)
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
(global-set-key (kbd "C-`") 'senny-persp-last)
(global-set-key (kbd "C-~") 'persp-push-current-buffer-to-last)
;; ========放弃使用键盘宏分配buffers到perspective的做法=========
;; (global-set-key (kbd "M-s s") 'swint-persp-start)
;; (fset 'swint-persp-all
;;       [?\M-s ?s
;;              ?% ?n ?\\ ?* ?s ?c ?r ?a ?t ?c ?h ?\\ ?* ?  ?\( ?8 ?\C-m ?% ?f ?~ ?/ ?D ?o ?c ?u ?m ?e ?n ?t ?s ?\C-m ?% ?f ?s ?w ?i ?n ?t ?/ ?D ?o ?c ?u ?m ?e ?n ?t ?s ?\C-m ?% ?f ?~ ?/ ?D ?r ?o ?p ?b ?o ?x ?\C-m ?% ?f ?s ?w ?i ?n ?t ?/ ?D ?r ?o ?p ?b ?o ?x ?\C-m ?% ?f ?~ ?/ ?N ?u ?t ?s ?t ?o ?r ?e ?\C-m ?% ?f ?s ?w ?i ?n ?t ?/ ?N ?u ?t ?s ?t ?o ?r ?e ?\C-m ?A ?\M-, ?\C-, ?\C-m ?\C-. ?\C-m
;;              ?\C-9 M-delete ?\C-m ?% ?n ?\\ ?* ?s ?c ?r ?a ?t ?c ?h ?\\ ?* ?  ?\( ?9 ?\C-m ?% ?f ?~ ?/ ?b ?o ?o ?k ?\C-m ?% ?f ?~ ?/ ?p ?a ?p ?e ?r ?s ?\C-m ?% ?f ?~ ?/ ?l ?i ?n ?u ?x ?\C-m ?% ?f ?s ?w ?i ?n ?t ?/ ?b ?o ?o ?k ?\C-m ?% ?f ?s ?w ?i ?n ?t ?/ ?p ?a ?p ?e ?r ?s ?\C-m ?% ?f ?s ?w ?i ?n ?t ?/ ?l ?i ?n ?u ?x ?\C-m ?A ?\M-, ?\C-, ?\C-m ?\C-. ?\C-m
;;              ?\C-0 M-delete ?\C-m ?% ?n ?\\ ?* ?s ?c ?r ?a ?t ?c ?h ?\\ ?* ?  ?\( ?0 ?\C-m ?% ?f ?~ ?/ ?t ?e ?x ?\C-m ?% ?f ?s ?w ?i ?n ?t ?/ ?t ?e ?x ?\C-m ?% ?f ?~ ?/ ?M ?u ?s ?i ?c ?\C-m ?% ?f ?s ?w ?i ?n ?t ?/ ?M ?u ?s ?i ?c ?\C-m ?% ?f ?~ ?/ ?P ?i ?c ?t ?u ?r ?e ?s ?\C-m ?% ?f ?s ?w ?i ?n ?t ?/ ?P ?i ?c ?t ?u ?r ?e ?s ?\C-m ?% ?f ?~ ?/ ?m ?y ?f ?i ?l ?e ?\C-m ?% ?f ?s ?w ?i ?n ?t ?/ ?m ?y ?f ?i ?l ?e ?\C-m ?A ?\M-, ?\C-, ?\C-m ?\C-. ?\C-m ?\C-c ?` ?q ?\C-0
;;              ])
;; ;; (global-set-key (kbd "S-SPC") 'swint-persp-all)
;; ;; 所有包加载完之后，启动swint-persp-all宏。
;; ;; 不行，虽然buffer已经加载完，但是界面上并没有显示，这个键盘宏发生作用的时候，并没有加载buffer，造成结果中没有保存的buffer
;; ;; 这个应该是函数和键盘宏的区别，函数是背后发生的事情，不需要界面的响应，而键盘宏是需要界面相应的，就像是一个人在操作一样。
;; ;; (eval-after-load 'setup_desktop_session
;; ;; '(execute-kbd-macro (symbol-function 'swint-persp-all)))
;; ;; 先让desktop恢复buffers，然后再启动swint-persp-all
;; (defun swint-perspective ()
;;   (interactive)
;;   (execute-kbd-macro (symbol-function 'swint-persp-all)))
;; (add-hook 'desktop-after-read-hook 'swint-perspective)
;; ========放弃使用键盘宏分配buffers到perspective的做法=========
;; 使用lisp方式在emacs关闭时保存perspectives，开启时读取
(setq swint-perspectives-saved-file "~/.emacs.d/saved-perspectives.el")
(defun swint-save-perspectives ()
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
            (persp-names))))
(add-hook 'kill-emacs-hook 'swint-save-perspectives)
(load swint-perspectives-saved-file t)
(defun swint-perspective-init ()
  ;; 升级到emacs24.5之后，(persp-mode)启动初始化错误，这里重新初始化。
  (persp-mode t)
  (remove-hook 'ido-make-buffer-list-hook 'persp-set-ido-buffers)
  (mapcar #'(lambda (x)
              (senny-persp x)
              (persp-reactivate-buffers
               (remove nil (mapcar 'get-buffer (symbol-value (intern (format "buffers-in-perspectives-%s" x))))))
              (window-state-put (symbol-value (intern (format "window-configuration-of-persp-%s" x))) nil t))
          swint-persp-names)
  (persp-switch persp-last-session))
(add-hook 'desktop-after-read-hook 'swint-perspective-init)
;; 在不同的persp中关闭同一个buffer时，会产生无效的(persp-point-marker persp)。
(add-hook 'persp-before-switch-hook '(lambda ()
                                       (unless (marker-position (persp-point-marker persp))
                                         (setf (persp-point-marker persp) (point)))))
;; ;; ================================perspective====================================
(provide 'setup_perspective)
