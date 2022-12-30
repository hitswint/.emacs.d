;;; sdcv
;; ==================sdcv====================
;;;###autoload
(define-derived-mode sdcv-mode org-mode nil
  "Major mode for sdcv."
  (when swint-fcitx-setup-done
    (fcitx--sdcv-maybe-deactivate))
  (local-set-key (kbd "q") #'(lambda () (interactive)
                               (swint-kill-buffer)
                               (jump-to-register :sdcv)
                               (when swint-fcitx-setup-done
                                 (fcitx--sdcv-maybe-activate)))))
(defvar sdcv-dictionary-list '("懒虫简明英汉词典"
                               "懒虫简明汉英词典"
                               "新世纪英汉科技大词典"
                               "新世纪汉英科技大词典"
                               "21世纪英汉汉英双向词典"
                               "简明英汉字典增强版"))
(defun sdcv-search-with-dictionary (word dictionary-list &optional to-buffer)
  "Search some WORD with dictionary list."
  (mapconcat (lambda (dict)
               (replace-regexp-in-string "^Nothing similar to.*\n" ""
                                         (shell-command-to-string
                                          (format "sdcv -n -e %s \"%s\""
                                                  (concat "-u " dict) word))))
             dictionary-list
             (if to-buffer "\n")))
(defun sdcv-output-cleaner ()
  (outline-show-all)
  (goto-char (point-min))
  (while (re-search-forward "Found\\ .*\\ items,\\ similar\\ to\\ \\(.*\\)\\.\n-->\\(.*\\)\n-->\\(.*\\)" nil t)
    ;; (replace-match "*** \\2-(\\1) \n**** \\3")
    (replace-match "*** \\2-(\\1)")
    )
  ;; sdcv使用-e选项，只有精确匹配，删除标题行
  ;; (goto-char (point-min))
  ;; (while (re-search-forward "-->\\(.*\\)\n-->\\(.*\\)" nil t)
  ;;   (replace-match "**** \\2"))
  (goto-char (point-min))
  (while (re-search-forward "^\n" nil t)
    (replace-match ""))
  (goto-char (point-min))
  (while (re-search-forward "\n+" nil t)
    (replace-match "\n"))
  (indent-region (point-min) (point-max) 0)
  (goto-char (point-min)))
;;;; 优先纵向分割窗口
;; ============优先纵向分割窗口==============
;; 设置split-xxx-threshold无法达到要求。
;; (setq split-height-threshold nil)
;; (setq split-width-threshold 0)
;; 设置display-buffer-function会影响其他package。
;; (setq display-buffer-function 'display-new-buffer)
(defun display-new-buffer (buffer force-other-window)
  "If BUFFER is visible, select it.
If it's not visible and there's only one window, split the
current window and select BUFFER in the new window. If the
current window (before the split) is more than 100 columns wide,
split horizontally(left/right), else split vertically(up/down).
If the current buffer contains more than one window, select
BUFFER in the least recently used window.
This function returns the window which holds BUFFER.
FORCE-OTHER-WINDOW is ignored."
  (or (get-buffer-window buffer)
      (if (one-window-p)
          (let ((new-win
                 (if (> (window-width) 100)
                     (split-window-horizontally)
                   (split-window-vertically))))
            (set-window-buffer new-win buffer)
            new-win)
        (let ((new-win (get-lru-window)))
          (set-window-buffer new-win buffer)
          new-win))))
(add-to-list 'display-buffer-alist '("\\`\\*sdcv\\*\\'" display-new-buffer))
(add-to-list 'display-buffer-alist '("\\`\\*bing-google\\*\\'" display-new-buffer))
;; ============优先纵向分割窗口==============
;;;###autoload
(defun swint-sdcv-to-tip (arg &optional _word)
  "Search WORD simple translate result."
  (interactive "P")
  (let* ((word (or _word (swint-get-words-at-point)))
         (sdcv-result (sdcv-search-with-dictionary word sdcv-dictionary-list t)))
    (if (string-match-p  "\\`[ \t\n\r]*\\'" sdcv-result)
        (message "Nothing")
      (if arg
          (pos-tip-show
           (replace-regexp-in-string "-->\\(.*\\)\n-->\\(.*\\)\n" "\\1：\\2"
                                     (replace-regexp-in-string
                                      "\\(^Found\\ [[:digit:]]+\\ items,\\ similar\\ to \\(.*\\)\\.\n\\)" ""
                                      (sdcv-search-with-dictionary word sdcv-dictionary-list)))
           nil nil nil 0)
        (with-current-buffer (get-buffer-create "*sdcv*")
          (let ((inhibit-read-only t))
            (buffer-disable-undo)
            (erase-buffer)
            (sdcv-mode)
            (insert sdcv-result)
            (sdcv-output-cleaner)))
        (posframe-show "*sdcv*"
                       :left-fringe 8
                       :right-fringe 8
                       :internal-border-color (face-foreground 'default)
                       :internal-border-width 1)
        (unwind-protect
            (let ((curr-event (read-event)))
              (while (member curr-event '(134217840 134217838))
                (cond ((eq curr-event 134217838)
                       (posframe-funcall "*sdcv*" #'(lambda () (ignore-errors (scroll-up-command)))))
                      ((eq curr-event 134217840)
                       (posframe-funcall "*sdcv*" #'(lambda () (ignore-errors (scroll-down-command))))))
                (setq curr-event (read-event)))
              (push curr-event unread-command-events))
          (progn
            (posframe-delete "*sdcv*")
            (other-frame 0)))))))
;;;###autoload
(defun swint-sdcv-to-buffer (&optional _word)
  (interactive)
  (let* ((word (or _word (swint-get-words-at-point)))
         (sdcv-result (sdcv-search-with-dictionary word sdcv-dictionary-list t)))
    (if (string-match-p  "\\`[ \t\n\r]*\\'" sdcv-result)
        (message "Nothing")
      (unless (member (buffer-name) '("*sdcv*" "*online*"))
        (window-configuration-to-register :sdcv))
      (delete-other-windows)
      (switch-to-buffer "*sdcv*")
      (set-buffer "*sdcv*")
      (buffer-disable-undo)
      (erase-buffer)
      (sdcv-mode)
      (insert sdcv-result)
      (sdcv-output-cleaner))))
;; ==================sdcv====================
;;; online
;; =================online===================
(defun online-output-cleaner ()
  (outline-show-all)
  (goto-char (point-min))
  (save-excursion
    (while (re-search-forward ".*Translate\\ from.*to.*:\n" nil t)
      (replace-match "")))
  (save-excursion
    (while (re-search-forward "\\\<1\\\. " nil t)
      (org-shiftmetaright)
      (org-shiftmetaright)
      (org-shiftmetaright)))
  (save-excursion
    (while (re-search-forward "^*\\ .*" nil t)
      (org-metaright)
      (org-metaright)
      (org-metaright)))
  (save-excursion
    (while (re-search-forward "^\n" nil t)
      (replace-match "")))
  (indent-region (point-min) (point-max))
  (goto-char (point-min)))
;;;###autoload
(defun swint-online-to-buffer (&optional _word)
  (interactive)
  (unless (member (buffer-name) '("*sdcv*" "*online*"))
    (window-configuration-to-register :sdcv))
  (let* ((word (or _word (swint-get-words-at-point)))
         (bing-result (ignore-errors (bing-dict-brief word t)))
         (youdao-result (ignore-errors (youdao-dictionary--format-result
                                        (youdao-dictionary--request word))))
         (baidu-result (ignore-errors
                         (baidu-translate-at-point word)
                         (buffer-substring-no-properties (point-min) (point-max)))))
    (delete-other-windows)
    (switch-to-buffer "*online*")
    (set-buffer "*online*")
    (buffer-disable-undo)
    (erase-buffer)
    (sdcv-mode)
    (insert "*** Google Translate\n")
    (unless (ignore-errors
              (if (pyim-string-match-p "\\cC" word)
                  (google-translate-translate "zh-CN" "en" word 'current-buffer)
                (google-translate-translate "en" "zh-CN" word 'current-buffer)))
      (insert "Nothing"))
    (insert "\n\n*** Bing Dict\n")
    (insert (or bing-result "Nothing"))
    (insert "\n\n*** Youdao Dictionary\n")
    (insert (or youdao-result "Nothing"))
    (insert "\n\n*** Baidu Translate\n")
    (insert (or baidu-result "Nothing"))
    (online-output-cleaner)))
;; =================online===================
