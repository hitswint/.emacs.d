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
     (defun persp/org()
       (interactive)
       (senny-persp "1"
                    (find-file (first org-agenda-files))
                    (call-interactively 'ibuffer)
                    ))
     (defun persp/coding()
       (interactive)
       (senny-persp "2"
                    (call-interactively 'ibuffer)
                    ))
     (defun persp/reference()
       (interactive)
       (senny-persp "3"
                    (call-interactively 'ibuffer)
                    ))
     (defun persp/tex()
       (interactive)
       (senny-persp "4"
                    (call-interactively 'ibuffer)
                    ))
     ))
;; =================模板宏==================
(defun swint-persp-start ()
  (interactive)
  (persp/tex)
  (persp/reference)
  (persp/coding)
  (persp/org))
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
(global-set-key (kbd "C-1") '(lambda () (interactive) (persp-switch "1")))
(global-set-key (kbd "C-2") '(lambda () (interactive) (persp-switch "2")))
(global-set-key (kbd "C-3") '(lambda () (interactive) (persp-switch "3")))
(global-set-key (kbd "C-4") '(lambda () (interactive) (persp-switch "4")))
(global-set-key (kbd "C-5") 'persp-push-all-buffer-to-init)
(global-set-key (kbd "C-!") '(lambda () (interactive) (persp-push-current-buffer "1")))
(global-set-key (kbd "C-@") '(lambda () (interactive) (persp-push-current-buffer "2")))
(global-set-key (kbd "C-#") '(lambda () (interactive) (persp-push-current-buffer "3")))
(global-set-key (kbd "C-$") '(lambda () (interactive) (persp-push-current-buffer "4")))
(global-set-key (kbd "C-%") '(lambda () (interactive) (persp-push-current-buffer "i")))
(global-set-key (kbd "C-`") 'senny-persp-last)
(global-set-key (kbd "C-~") 'persp-push-current-buffer-to-last)
(global-set-key (kbd "M-s s") 'swint-persp-start)
(global-set-key (kbd "M-s a") 'persp-add-buffer)
(global-set-key (kbd "M-s k") 'persp-remove-buffer)
(fset 'swint-persp-all
      [?\M-s ?s
             ?% ?n ?\\ ?* ?s ?c ?r ?a ?t ?c ?h ?\\ ?* ?  ?\( ?1 ?\C-m ?% ?f ?~ ?/ ?. ?e ?m ?a ?c ?s ?. ?d ?\C-m ?% ?f ?s ?w ?i ?n ?t ?/ ?. ?e ?m ?a ?c ?s ?. ?d ?\C-m ?% ?f ?~ ?/ ?o ?r ?g ?\C-m ?% ?f ?s ?w ?i ?n ?t ?/ ?o ?r ?g ?\C-m ?% ?f ?~ ?/ ?M ?u ?s ?i ?c ?\C-m ?% ?f ?s ?w ?i ?n ?t ?/ ?M ?u ?s ?i ?c ?\C-m ?% ?f ?~ ?/ ?P ?i ?c ?t ?u ?r ?e ?s ?\C-m ?% ?f ?s ?w ?i ?n ?t ?/ ?P ?i ?c ?t ?u ?r ?e ?s ?\C-m ?% ?f ?~ ?/ ?D ?o ?w ?n ?l ?o ?a ?d ?s ?\C-m ?% ?f ?s ?w ?i ?n ?t ?/ ?D ?o ?w ?n ?l ?o ?a ?d ?s ?\C-m ?% ?f ?~ ?/ ?m ?y ?f ?i ?l ?e ?\C-m ?% ?f ?s ?w ?i ?n ?t ?/ ?m ?y ?f ?i ?l ?e ?\C-m ?% ?f ?~ ?/ ?D ?r ?o ?p ?b ?o ?x ?\C-m ?% ?f ?s ?w ?i ?n ?t ?/ ?D ?r ?o ?p ?b ?o ?x ?\C-m ?A ?\M-, ?\C-, ?\C-m ?\C-. ?\C-m
             ?\C-2 M-delete ?\C-m ?% ?n ?\\ ?* ?s ?c ?r ?a ?t ?c ?h ?\\ ?* ?  ?\( ?2 ?\C-m ?% ?f ?~ ?/ ?D ?o ?c ?u ?m ?e ?n ?t ?s ?\C-m ?% ?f ?s ?w ?i ?n ?t ?/ ?D ?o ?c ?u ?m ?e ?n ?t ?s ?\C-m ?A ?\M-, ?\C-, ?\C-m ?\C-. ?\C-m
             ?\C-3 M-delete ?\C-m ?% ?n ?\\ ?* ?s ?c ?r ?a ?t ?c ?h ?\\ ?* ?  ?\( ?3 ?\C-m ?% ?f ?~ ?/ ?b ?o ?o ?k ?\C-m ?% ?f ?~ ?/ ?p ?a ?p ?e ?r ?s ?\C-m ?% ?f ?~ ?/ ?l ?i ?n ?u ?x ?\C-m ?% ?f ?s ?w ?i ?n ?t ?/ ?b ?o ?o ?k ?\C-m ?% ?f ?s ?w ?i ?n ?t ?/ ?p ?a ?p ?e ?r ?s ?\C-m ?% ?f ?s ?w ?i ?n ?t ?/ ?l ?i ?n ?u ?x ?\C-m ?A ?\M-, ?\C-, ?\C-m ?\C-. ?\C-m
             ?\C-4 M-delete ?\C-m ?% ?n ?\\ ?* ?s ?c ?r ?a ?t ?c ?h ?\\ ?* ?  ?\( ?4 ?\C-m ?% ?f ?~ ?/ ?t ?e ?x ?\C-m ?% ?f ?s ?w ?i ?n ?t ?/ ?t ?e ?x ?\C-m ?A ?\M-, ?\C-, ?\C-m ?\C-. ?\C-m ?\C-c ?` ?q ?\C-1
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
