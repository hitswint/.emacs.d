;; ================================perspective====================================
;; (add-to-list 'load-path "~/.emacs.d/perspective")
(require 'perspective)
(persp-mode)
(global-set-key (kbd "C-c .") 'persp-switch)
;;; iswitch限制在当前persp
(defun iswitchb-persp-curr-only ()
  (let* ((names (remq nil (mapcar 'buffer-name (persp-buffers persp-curr))))
         (matches (remove-if-not (lambda (x) (member x names)) iswitchb-temp-buflist)))
    (setq iswitchb-temp-buflist matches)))
(add-hook 'iswitchb-make-buflist-hook 'iswitchb-persp-curr-only)
;;; ido限制在当前persp
(defun ido-persp-curr-only ()
  (let* ((names (remq nil (mapcar 'buffer-name (persp-buffers persp-curr))))
         (matches (remove-if-not (lambda (x) (member x names)) ido-temp-list)))
    (setq ido-temp-list matches)))
(add-hook 'ido-make-buffer-list-hook 'ido-persp-curr-only)
;; =================模板宏==================
(eval-after-load 'perspective
  '(progn
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
     ;; Perspective Definitions
     (defun persp/coding()
       (interactive)
       (senny-persp "8"
                    (call-interactively 'ibuffer)
                    ))
     (defun persp/reference()
       (interactive)
       (senny-persp "9"
                    (call-interactively 'ibuffer)
                    ))
     (defun persp/tex()
       (interactive)
       (senny-persp "0"
                    (call-interactively 'ibuffer)
                    ))
     ))
;; =================模板宏==================
(defun swint-persp-start ()
  (interactive)
  (find-file (first org-agenda-files))
  (persp/tex)
  (persp/reference)
  (persp/coding))
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
    (persp-switch "i")))
(global-set-key (kbd "C-8") '(lambda () (interactive) (persp-switch "8")))
(global-set-key (kbd "C-9") '(lambda () (interactive) (persp-switch "9")))
(global-set-key (kbd "C-0") '(lambda () (interactive) (persp-switch "0")))
(global-set-key (kbd "C-7") 'persp-push-all-buffer-to-init)
(global-set-key (kbd "C-*") '(lambda () (interactive) (persp-push-current-buffer "8")))
(global-set-key (kbd "C-(") '(lambda () (interactive) (persp-push-current-buffer "9")))
(global-set-key (kbd "C-)") '(lambda () (interactive) (persp-push-current-buffer "0")))
(global-set-key (kbd "C-&") '(lambda () (interactive) (persp-push-current-buffer "i")))
(global-set-key (kbd "C-`") 'senny-persp-last)
(global-set-key (kbd "C-~") 'persp-push-current-buffer-to-last)
(global-set-key (kbd "M-s s") 'swint-persp-start)
(fset 'swint-persp-all
      [?\M-s ?s
             ?% ?n ?\\ ?* ?s ?c ?r ?a ?t ?c ?h ?\\ ?* ?  ?\( ?8 ?\C-m ?% ?f ?~ ?/ ?D ?o ?c ?u ?m ?e ?n ?t ?s ?\C-m ?% ?f ?s ?w ?i ?n ?t ?/ ?D ?o ?c ?u ?m ?e ?n ?t ?s ?\C-m ?% ?f ?~ ?/ ?D ?r ?o ?p ?b ?o ?x ?\C-m ?% ?f ?s ?w ?i ?n ?t ?/ ?D ?r ?o ?p ?b ?o ?x ?\C-m ?% ?f ?~ ?/ ?N ?u ?t ?s ?t ?o ?r ?e ?\C-m ?% ?f ?s ?w ?i ?n ?t ?/ ?N ?u ?t ?s ?t ?o ?r ?e ?\C-m ?A ?\M-, ?\C-, ?\C-m ?\C-. ?\C-m
             ?\C-9 M-delete ?\C-m ?% ?n ?\\ ?* ?s ?c ?r ?a ?t ?c ?h ?\\ ?* ?  ?\( ?9 ?\C-m ?% ?f ?~ ?/ ?b ?o ?o ?k ?\C-m ?% ?f ?~ ?/ ?p ?a ?p ?e ?r ?s ?\C-m ?% ?f ?~ ?/ ?l ?i ?n ?u ?x ?\C-m ?% ?f ?s ?w ?i ?n ?t ?/ ?b ?o ?o ?k ?\C-m ?% ?f ?s ?w ?i ?n ?t ?/ ?p ?a ?p ?e ?r ?s ?\C-m ?% ?f ?s ?w ?i ?n ?t ?/ ?l ?i ?n ?u ?x ?\C-m ?A ?\M-, ?\C-, ?\C-m ?\C-. ?\C-m
             ?\C-0 M-delete ?\C-m ?% ?n ?\\ ?* ?s ?c ?r ?a ?t ?c ?h ?\\ ?* ?  ?\( ?0 ?\C-m ?% ?f ?~ ?/ ?t ?e ?x ?\C-m ?% ?f ?s ?w ?i ?n ?t ?/ ?t ?e ?x ?\C-m ?% ?f ?~ ?/ ?M ?u ?s ?i ?c ?\C-m ?% ?f ?s ?w ?i ?n ?t ?/ ?M ?u ?s ?i ?c ?\C-m ?% ?f ?~ ?/ ?P ?i ?c ?t ?u ?r ?e ?s ?\C-m ?% ?f ?s ?w ?i ?n ?t ?/ ?P ?i ?c ?t ?u ?r ?e ?s ?\C-m ?% ?f ?~ ?/ ?m ?y ?f ?i ?l ?e ?\C-m ?% ?f ?s ?w ?i ?n ?t ?/ ?m ?y ?f ?i ?l ?e ?\C-m ?A ?\M-, ?\C-, ?\C-m ?\C-. ?\C-m ?\C-c ?` ?q ?\C-0
             ])
;; (global-set-key (kbd "S-SPC") 'swint-persp-all)
;; 所有包加载完之后，启动swint-persp-all宏。
;; 不行，虽然buffer已经加载完，但是界面上并没有显示，这个键盘宏发生作用的时候，并没有加载buffer，造成结果中没有保存的buffer
;; 这个应该是函数和键盘宏的区别，函数是背后发生的事情，不需要界面的响应，而键盘宏是需要界面相应的，就像是一个人在操作一样。
;; (eval-after-load 'setup_desktop_session
;; '(execute-kbd-macro (symbol-function 'swint-persp-all)))
;; 先让desktop恢复buffers，然后再启动swint-persp-all
(defun swint-perspective ()
  (interactive)
  (execute-kbd-macro (symbol-function 'swint-persp-all)))
(add-hook 'desktop-after-read-hook 'swint-perspective)
;; ================================perspective====================================
(provide 'setup_perspective)
