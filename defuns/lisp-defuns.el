;; ====================eval-expression=====================
(global-set-key (kbd "C-c =") 'replace-last-sexp)
(defun replace-last-sexp ()
  (interactive)
  (let ((value (eval (preceding-sexp))))
    (kill-sexp -1)
    (insert (format "%s" value))))
;; ====================eval-expression=====================
;; ===============解决C-n卡顿的问题===============
;; 参考http://www.tuicool.com/articles/q6j263
;; 牺牲了对图片的支持，换取换行速度，注释掉(message "%s" "executed?")。
(defun line-move (arg &optional noerror to-end try-vscroll)
  (unless (and auto-window-vscroll try-vscroll
               ;; Only vscroll for single line moves
               (= (abs arg) 1)
               ;; But don't vscroll in a keyboard macro.
               (not defining-kbd-macro)
               (not executing-kbd-macro)
               (line-move-partial arg noerror to-end)
               )
    ;; (message "%s" "executed?")
    (set-window-vscroll nil 0 t)
    (if (and line-move-visual
             ;; Display-based column are incompatible with goal-column.
             (not goal-column)
             ;; When the text in the window is scrolled to the left,
             ;; display-based motion doesn't make sense (because each
             ;; logical line occupies exactly one screen line).
             (not (> (window-hscroll) 0)))
        (line-move-visual arg noerror)
      (line-move-1 arg noerror to-end))))
(global-set-key "\C-n" '(lambda () (interactive) (line-move 1 nil)))
;; ===============解决C-n卡顿的问题===============
