;;; sdcv
;; ==================sdcv====================
(defvar sdcv-dictionary-list '("懒虫简明英汉词典"
                               "懒虫简明汉英词典"
                               "新世纪英汉科技大词典"
                               "新世纪汉英科技大词典"
                               "21世纪英汉汉英双向词典"
                               "简明英汉字典增强版"))
;;;###autoload
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
    (replace-match "*** \\2-(\\1)"))
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
(add-to-list 'display-buffer-alist '("\\`\\*ydcv\\*\\'" display-new-buffer))
;; 需设置为t，switch-to-buffer才会遵从display-buffer-alist设定
(setq switch-to-buffer-obey-display-actions nil)
;; ============优先纵向分割窗口==============
;;;###autoload
(defun swint-display-dict (buffer result &optional cleaner to-buffer)
  (with-current-buffer (get-buffer-create buffer)
    (let ((inhibit-read-only t))
      (buffer-disable-undo)
      (erase-buffer)
      (org-mode)
      (insert result)
      (when cleaner (funcall cleaner))))
  (if (or to-buffer (not (posframe-workable-p)))
      (posframe-setup-buffer buffer)
    (posframe-show buffer
                   :border-color "red"
                   :border-width 2
                   :background-color "black"
                   :max-width (window-width))
    (posframe-scroll-or-switch buffer)))
;;;###autoload
(defun swint-sdcv-to-tip (&optional _word)
  "Search WORD simple translate result."
  (interactive "P")
  (let* ((word (or _word (swint-get-words-at-point)))
         (sdcv-result (sdcv-search-with-dictionary word sdcv-dictionary-list t)))
    (if (string-match-p  "\\`[ \t\n\r]*\\'" sdcv-result)
        (bing-dict-brief-cb-at-point word)
      ;; (pos-tip-show
      ;;  (replace-regexp-in-string "-->\\(.*\\)\n-->\\(.*\\)\n" "\\1：\\2"
      ;;                            (replace-regexp-in-string
      ;;                             "\\(^Found\\ [[:digit:]]+\\ items,\\ similar\\ to \\(.*\\)\\.\n\\)" ""
      ;;                             (sdcv-search-with-dictionary word sdcv-dictionary-list)))
      ;;  nil nil nil 0)
      (swint-display-dict "*sdcv*" sdcv-result 'sdcv-output-cleaner))))
;;;###autoload
(defun swint-sdcv-to-buffer (&optional _word)
  (interactive)
  (let* ((word (or _word (swint-get-words-at-point)))
         (sdcv-result (sdcv-search-with-dictionary word sdcv-dictionary-list t)))
    (if (string-match-p  "\\`[ \t\n\r]*\\'" sdcv-result)
        (bing-dict-brief-cb-at-point word)
      (swint-display-dict "*sdcv*" sdcv-result 'sdcv-output-cleaner t))))
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
  (let* ((word (or _word (swint-get-words-at-point)))
         (dict-list (helm-comp-read "Select files with order: " '("Google" "Bing" "Youdao" "Baidu" "Lingva")
                                    :marked-candidates t
                                    :buffer "*helm dired converter-swint*"))
         (online-result (cl-loop for dict in dict-list
                                 concat (let ((result (ignore-errors (save-excursion
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
                                          (format "\n\n*** %s\n%s" dict (or result "Nothing"))))))
    (swint-display-dict "*online*" online-result 'online-output-cleaner t)
    (cl-loop for b in '("*baidu-translate*" "*lingva*")
             do (when (get-buffer b)
                  (kill-buffer b)))))
;;;###autoload
(defun dict-load-config (file key)
  (with-temp-buffer
    (insert-file-contents (expand-file-name file))
    (when (string-match (concat key "=\\(.*\\)") (buffer-string))
      (match-string 1 (buffer-string)))))
;; =================online===================
