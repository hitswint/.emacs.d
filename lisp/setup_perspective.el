;; ================================perspective====================================
;; 将配置写到:config启动时会有错误。
;; Eager macro-expansion failure:
;; (error "(persp-buffers persp) is not a valid place expression")
(use-package perspective
  :load-path "site-lisp/perspective/")
(persp-mode)
;; iswitch限制在当前persp
(defvar iswitchb-temp-buflist/other-persps nil)
(defvar iswitchb-temp-buflist/all-persps nil)
(defun iswitchb-persp-curr-only ()
  (let* ((names (remq nil (mapcar 'buffer-name (persp-buffers persp-curr))))
         (matches (remove-if-not (lambda (x) (member x names)) iswitchb-temp-buflist))
         (matches-other-persps (remove-if (lambda (x) (member x names)) iswitchb-temp-buflist)))
    (setq iswitchb-temp-buflist/other-persps matches-other-persps)
    (setq iswitchb-temp-buflist/all-persps iswitchb-temp-buflist)
    (setq iswitchb-temp-buflist matches)))
(add-hook 'iswitchb-make-buflist-hook 'iswitchb-persp-curr-only)
;; ido限制在当前persp
(defvar ido-temp-list/other-persps nil)
(defvar ido-temp-list/all-persps nil)
(defun ido-persp-curr-only ()
  (let* ((names (remq nil (mapcar 'buffer-name (persp-buffers persp-curr))))
         (matches (remove-if-not (lambda (x) (member x names)) ido-temp-list))
         (matches-other-persps (remove-if (lambda (x) (member x names)) ido-temp-list)))
    (setq ido-temp-list/other-persps matches-other-persps)
    (setq ido-temp-list/all-persps ido-temp-list)
    (setq ido-temp-list matches)))
(add-hook 'ido-make-buffer-list-hook 'ido-persp-curr-only)
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
                (push element (persp-buffers init-persp)))
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
  (with-temp-file
      swint-perspectives-saved-file
    (when is-win
      (insert ";;; -*- coding: utf-8; -*-\n"))
    (insert "(setq " "buffers-in-perspectives-8 '")
    (insert (prin1-to-string (remove nil (mapcar 'buffer-name (elt (gethash "8" perspectives-hash) 2)))))
    (insert ")\n")
    (insert "(setq " "buffers-in-perspectives-9 '")
    (insert (prin1-to-string (remove nil (mapcar 'buffer-name (elt (gethash "9" perspectives-hash) 2)))))
    (insert ")\n")
    (insert "(setq " "buffers-in-perspectives-0 '")
    (insert (prin1-to-string (remove nil (mapcar 'buffer-name (elt (gethash "0" perspectives-hash) 2)))))
    (insert ")\n")))
(add-hook 'kill-emacs-hook 'swint-save-perspectives)
(load swint-perspectives-saved-file t)
(defun swint-perspective-init ()
  ;; 升级到emacs24.5之后，(persp-mode)启动初始化错误，这里重新初始化。
  (persp-mode t)
  (senny-persp "8")
  (mapcar 'persp-add-buffer (remove nil (mapcar 'get-buffer buffers-in-perspectives-8)))
  (senny-persp "9")
  (mapcar 'persp-add-buffer (remove nil (mapcar 'get-buffer buffers-in-perspectives-9)))
  (senny-persp "0")
  (mapcar 'persp-add-buffer (remove nil (mapcar 'get-buffer buffers-in-perspectives-0)))
  (persp-switch "i")
  (find-file (first org-agenda-files)))
(add-hook 'desktop-after-read-hook 'swint-perspective-init)
;; ;; ================================perspective====================================
(provide 'setup_perspective)
