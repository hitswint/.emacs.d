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
;; 设置split-xxx-threshold无法达到要求
;; (setq split-height-threshold nil)
;; (setq split-width-threshold 0)
;; 设置display-buffer-function会影响其他package
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
(defun posframe-scroll-or-switch (buffer)
  (let (switch-to-posframe-buffer)
    (unwind-protect
        (let ((curr-event (read-event)))
          (setq switch-to-posframe-buffer (catch 'break
                                            (while (member curr-event '(134217840 134217838 134217829))
                                              (cond ((eq curr-event 134217838) ;M-n
                                                     (posframe-funcall buffer #'(lambda () (ignore-errors (scroll-up-command)))))
                                                    ((eq curr-event 134217840) ;M-p
                                                     (posframe-funcall buffer #'(lambda () (ignore-errors (scroll-down-command)))))
                                                    ((eq curr-event 134217829) ;M-e
                                                     (throw 'break t)))
                                              (setq curr-event (read-event)))))
          (unless switch-to-posframe-buffer
            (push curr-event unread-command-events)))
      (progn
        (if switch-to-posframe-buffer
            (progn (posframe-delete-frame buffer)
                   (other-frame 0)
                   (switch-to-buffer buffer)
                   (setq-local cursor-type t)
                   (setq-local cursor-in-non-selected-windows t))
          (posframe-delete buffer)
          (other-frame 0))))))
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
        (posframe-scroll-or-switch "*sdcv*")))))
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
  (let ((word (or _word (swint-get-words-at-point)))
        (dict-list (helm-comp-read "Select files with order: " '("Google" "Bing" "Youdao" "Baidu" "Lingva")
                                   :marked-candidates t
                                   :buffer (concat "*helm dired converter-swint*"))))
    (delete-other-windows)
    (switch-to-buffer "*online*")
    (set-buffer "*online*")
    (buffer-disable-undo)
    (erase-buffer)
    (sdcv-mode)
    (cl-loop for dict in dict-list do
             (let ((result (ignore-errors (save-excursion
                                            (cond ((equal dict "Google")
                                                   (google-translate-translate_chieng word 'kill-ring)
                                                   (current-kill 0))
                                                  ((equal dict "Bing")
                                                   (bing-dict-brief word t))
                                                  ((equal dict "Youdao")
                                                   (youdao-dictionary--format-result (youdao-dictionary--request word)))
                                                  ((equal dict "Baidu")
                                                   (baidu-translate-at-point word)
                                                   (buffer-substring-no-properties (point-min) (point-max)))
                                                  ((equal dict "Lingva")
                                                   (lingva-translate-at-point word)
                                                   (while (not (get-buffer "*lingva*"))
                                                     (sit-for 1))
                                                   (current-kill 0)))))))
               (insert (format "\n\n*** %s\n" dict))
               (insert (or result "Nothing"))))
    (online-output-cleaner)
    (cl-loop for b in '("*baidu-translate*" "*lingva*")
             do (when (get-buffer b)
                  (kill-buffer b)))))
;; =================online===================
