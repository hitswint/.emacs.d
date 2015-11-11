;; ====================ace-jump=========================
(eval-after-load 'pinyin-search '(require 'ace-jump-mode))
;; (setq ace-jump-mode-gray-background nil)
;; (setq ace-jump-mode-move-keys
;;       (nconc (loop for i from ?a to ?z collect i)
;;              (loop for i from ?0 to ?9 collect i)
;;              (loop for i from ?A to ?Z collect i)))
(global-set-key (kbd "C-h") 'swint-ace-jump-char-mode)
(global-set-key (kbd "C-c C-h") 'ace-jump-mode)
(global-set-key (kbd "C-M-h") 'ace-jump-line-mode)
;; C-u C-h ace-jume-char-mode; C-u C-u C-h ace-jump-line-mode
;; If you also use viper mode:
;; (define-key viper-vi-global-user-map (kbd "SPC") 'ace-jump-mode)
(defun swint-ace-jump-search-candidate (re-query-string visual-area-list)
  "Search the RE-QUERY-STRING in current view, and return the candidate position list.
RE-QUERY-STRING should be an valid regex used for `search-forward-regexp'.
You can control whether use the case sensitive or not by `ace-jump-mode-case-fold'.
Every possible `match-beginning' will be collected.
The returned value is a list of `aj-position' record."
  (loop for va in visual-area-list
        append (let* ((current-window (aj-visual-area-window va))
                      (start-point (window-start current-window))
                      (end-point   (window-end   current-window t)))
                 (with-selected-window current-window
                   (save-excursion
                     (goto-char start-point)
                     (let ((case-fold-search ace-jump-mode-case-fold)
                           re-query-string-all) ;定义包括中英文字符串
                       (if (string-empty-p (pinyin-search--pinyin-to-regexp re-query-string))
                           (setq re-query-string-all (concat "[" re-query-string "]")) ;无法跳转.*+?等正则表达式使用的符号
                         (setq re-query-string-all (concat "[" (substring (pinyin-search--pinyin-to-regexp re-query-string) 1 -1) re-query-string "]")))
                       (loop while (re-search-forward re-query-string-all nil t) ;使用pinyin-search实现中文ace-jump
                             until (or
                                    (> (point) end-point)
                                    (eobp))
                             if (or ace-jump-allow-invisible (not (invisible-p (match-beginning 0))))
                             collect (make-aj-position :offset (match-beginning 0)
                                                       :visual-area va)
                             ;; when we use "^" to search line mode,
                             ;; re-search-backward will not move one
                             ;; char after search success, as line
                             ;; begin is not a valid visible char.
                             ;; We need to help it to move forward.
                             do (if (string-equal re-query-string "^")
                                    (goto-char (1+ (match-beginning 0)))))))))))
(defun swint-ace-jump-do (re-query-string)
  "The main function to start the AceJump mode.
QUERY-STRING should be a valid regexp string, which finally pass to `search-forward-regexp'.
You can constrol whether use the case sensitive via `ace-jump-mode-case-fold'."
  ;; we check the move key to make it valid, cause it can be customized by user
  (if (or (null ace-jump-mode-move-keys)
          (< (length ace-jump-mode-move-keys) 2)
          (not (every #'characterp ace-jump-mode-move-keys)))
      (error "[AceJump] Invalid move keys: check ace-jump-mode-move-keys"))
  ;; search candidate position
  (let* ((visual-area-list (ace-jump-list-visual-area/exclude-pdf-view)) ;exclude pdf-view buffer
         (candidate-list (swint-ace-jump-search-candidate re-query-string visual-area-list)))
    (cond
     ;; cannot find any one
     ((null candidate-list)
      (setq ace-jump-current-mode nil)
      (error "[AceJump] No one found"))
     ;; we only find one, so move to it directly
     ((eq (cdr candidate-list) nil)
      (ace-jump-push-mark)
      (run-hooks 'ace-jump-mode-before-jump-hook)
      (ace-jump-jump-to (car candidate-list))
      (message "[AceJump] One candidate, move to it directly")
      (run-hooks 'ace-jump-mode-end-hook))
     ;; more than one, we need to enter AceJump mode
     (t
      ;; make indirect buffer for those windows that show the same buffer
      ;; (setq ace-jump-recover-visual-area-list
      ;;       (ace-jump-mode-make-indirect-buffer visual-area-list))
      ;; create background for each visual area
      (if ace-jump-mode-gray-background
          (setq ace-jump-background-overlay-list
                (loop for va in visual-area-list
                      collect (let* ((w (aj-visual-area-window va))
                                     (b (aj-visual-area-buffer va))
                                     (ol (make-overlay (window-start w)
                                                       (window-end w)
                                                       b)))
                                (overlay-put ol 'face 'ace-jump-face-background)
                                ol))))
      ;; construct search tree and populate overlay into tree
      (setq ace-jump-search-tree
            (ace-jump-tree-breadth-first-construct (length candidate-list)
                                                   (length ace-jump-mode-move-keys)))
      (ace-jump-populate-overlay-to-search-tree ace-jump-search-tree
                                                candidate-list)
      (swint-ace-jump-update-overlay-in-search-tree ace-jump-search-tree
                                                    ace-jump-mode-move-keys)
      ;; do minor mode configuration
      (cond
       ((eq ace-jump-current-mode 'ace-jump-char-mode)
        (setq ace-jump-mode " AceJump - Char"))
       ((eq ace-jump-current-mode 'ace-jump-word-mode)
        (setq ace-jump-mode " AceJump - Word"))
       ((eq ace-jump-current-mode 'ace-jump-line-mode)
        (setq ace-jump-mode " AceJump - Line"))
       (t
        (setq ace-jump-mode " AceJump")))
      (force-mode-line-update)
      ;; override the local key map
      (setq overriding-local-map
            (let ( (map (make-keymap)) )
              (dolist (key-code ace-jump-mode-move-keys)
                (define-key map (make-string 1 key-code) 'swint-ace-jump-move))
              (define-key map (kbd "C-c C-c") 'ace-jump-quick-exchange)
              (define-key map [t] 'ace-jump-done)
              map))
      (add-hook 'mouse-leave-buffer-hook 'ace-jump-done)
      (add-hook 'kbd-macro-termination-hook 'ace-jump-done)))))
(defun swint-ace-jump-char-mode (query-char)
  "AceJump char mode"
  (interactive (list (read-char "Query Char:")))
  ;; We should prevent recursion call this function.  This can happen
  ;; when you trigger the key for ace jump again when already in ace
  ;; jump mode.  So we stop the previous one first.
  (if ace-jump-current-mode (ace-jump-done))
  (if (eq (ace-jump-char-category query-char) 'other)
      (error "[AceJump] Non-printable character"))
  ;; others : digit , alpha, punc
  (setq ace-jump-query-char query-char)
  (setq ace-jump-current-mode 'ace-jump-char-mode)
  (swint-ace-jump-do (regexp-quote (make-string 1 query-char))))
(defun swint-ace-jump-update-overlay-in-search-tree (tree keys)
  "Update overlay 'display property using each name in keys"
  (lexical-let* (;; create dynamic variable for following function
                 (key ?\0)
                 ;; populdate each leaf node to be the specific key,
                 ;; this only update 'display' property of overlay,
                 ;; so that user can see the key from screen and select
                 (func-update-overlay
                  (lambda (node)
                    (let ((ol (cdr node)))
                      (overlay-put
                       ol
                       'display
                       (concat (make-string 1 key)
                               (let* ((pos (overlay-get ol 'aj-data))
                                      (subs (ace-jump-buffer-substring pos)))
                                 (cond
                                  ;; when tab, we use more space to prevent screen
                                  ;; from messing up
                                  ((string-equal subs "\t")
                                   (make-string (1- tab-width) ? ))
                                  ;; when enter, we need to add one more enter
                                  ;; to make the screen not change
                                  ((string-equal subs "\n")
                                   "\n")
                                  ((string-match "\\cc" subs) ;判断是否为汉字
                                   (make-string (- tab-width 7) ? )) ;对于中文ace-jump，在英文后增加一个空格，一个tab-width是8
                                  (t
                                   "")))))))))
    (loop for k in keys
          for n in (cdr tree)
          do (progn
               ;; update "key" variable so that the function can use
               ;; the correct context
               (setq key k)
               (if (eq (car n) 'branch)
                   (ace-jump-tree-preorder-traverse n
                                                    func-update-overlay)
                 (funcall func-update-overlay n))))))
(defun swint-ace-jump-move ()
  "move cursor based on user input"
  (interactive)
  (let* ((index (let ((ret (position (aref (this-command-keys) 0)
                                     ace-jump-mode-move-keys)))
                  (if ret ret (length ace-jump-mode-move-keys))))
         (node (nth index (cdr ace-jump-search-tree))))
    (cond
     ;; we do not find key in search tree. This can happen, for
     ;; example, when there is only three selections in screen
     ;; (totally five move-keys), but user press the forth move key
     ((null node)
      (message "No such position candidate.")
      (ace-jump-done))
     ;; this is a branch node, which means there need further
     ;; selection
     ((eq (car node) 'branch)
      (let ((old-tree ace-jump-search-tree))
        ;; we use sub tree in next move, create a new root node
        ;; whose child is the sub tree nodes
        (setq ace-jump-search-tree (cons 'branch (cdr node)))
        (swint-ace-jump-update-overlay-in-search-tree ace-jump-search-tree
                                                      ace-jump-mode-move-keys)
        ;; this is important, we need remove the subtree first before
        ;; do delete, we set the child nodes to nil
        (setf (cdr node) nil)
        (ace-jump-delete-overlay-in-search-tree old-tree)))
     ;; if the node is leaf node, this is the final one
     ((eq (car node) 'leaf)
      ;; need to save aj data, as `ace-jump-done' will clean it
      (let ((aj-data (overlay-get (cdr node) 'aj-data)))
        (ace-jump-done)
        (ace-jump-push-mark)
        (run-hooks 'ace-jump-mode-before-jump-hook)
        (ace-jump-jump-to aj-data))
      (run-hooks 'ace-jump-mode-end-hook))
     (t
      (ace-jump-done)
      (error "[AceJump] Internal error: tree node type is invalid")))))
;; ace-jump-list-visual-area/exclude-pdf-view
;; win中pdf-tools暂时不可用。
(defun ace-jump-list-visual-area/exclude-pdf-view ()
  "Exclude pdf view area to improve performance."
  (loop for f in (frame-list)
        append (loop for w in (remove-if (lambda (x) (eq (buffer-mode (window-buffer x)) 'pdf-view-mode)) (window-list f))
                     collect (make-aj-visual-area :buffer (window-buffer w)
                                                  :window w
                                                  :frame f))))
;; ====================ace-jump=========================
(provide 'setup_ace_jump)
