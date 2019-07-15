;;; sdcv
;; ==================sdcv====================
;;;###autoload
(define-derived-mode sdcv-mode org-mode nil
  "Major mode for sdcv."
  (fcitx--sdcv-maybe-deactivate)
  (local-set-key (kbd "q") '(lambda () (interactive)
                              (swint-kill-buffer)
                              (jump-to-register :sdcv)
                              (fcitx--sdcv-maybe-activate))))
(defvar sdcv-dictionary-list '("朗道英汉字典5.0"
                               "朗道汉英字典5.0"
                               "XDICT英汉辞典"
                               "XDICT汉英辞典"
                               "新世纪英汉科技大词典"
                               "新世纪汉英科技大词典"
                               "Collins\\ Cobuild\\ English\\ Dictionary"))
(defun sdcv-search-with-dictionary (word dictionary-list &optional to-buffer)
  "Search some WORD with dictionary list."
  (mapconcat (lambda (dict)
               (replace-regexp-in-string "^对不起，没有发现和.*\n" ""
                                         (shell-command-to-string
                                          (format "sdcv -n %s %s"
                                                  (concat "-u " dict) word))))
             (if to-buffer dictionary-list
               (butlast dictionary-list)) (if to-buffer "\n")))
(defun sdcv-output-cleaner ()
  (outline-show-all)
  (goto-char (point-min))
  (while (re-search-forward "Found\\ .*\\ items,\\ similar\\ to\\ \\(.*\\)\\.\n-->\\(.*\\)\n-->\\(.*\\)" nil t)
    (replace-match "*** \\2-(\\1) \n**** \\3"))
  (goto-char (point-min))
  (while (re-search-forward "-->\\(.*\\)\n-->\\(.*\\)" nil t)
    (replace-match "**** \\2"))
  (goto-char (point-min))
  (while (re-search-forward "^\n" nil t)
    (replace-match ""))
  (goto-char (point-min))
  (while (re-search-forward "\n+" nil t)
    (replace-match "\n"))
  (indent-region (point-min) (point-max))
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
(defun swint-sdcv-to-tip (&optional _word)
  "Search WORD simple translate result."
  (interactive)
  (let ((word (or _word (swint-get-words-at-point))))
    (pos-tip-show
     (replace-regexp-in-string "-->\\(.*\\)\n-->\\(.*\\)\n" "\\1：\\2"
                               (replace-regexp-in-string
                                "\\(^Found\\ [[:digit:]]+\\ items,\\ similar\\ to \\(.*\\)\\.\n\\)" ""
                                (sdcv-search-with-dictionary word sdcv-dictionary-list)))
     nil nil nil 0)))
;;;###autoload
(defun swint-sdcv-to-buffer (&optional _word)
  (interactive)
  (let ((word (or _word (swint-get-words-at-point))))
    (unless (member (buffer-name) '("*sdcv*" "*online*"))
      (window-configuration-to-register :sdcv))
    (delete-other-windows)
    (switch-to-buffer "*sdcv*")
    (set-buffer "*sdcv*")
    (buffer-disable-undo)
    (erase-buffer)
    (sdcv-mode)
    (insert (sdcv-search-with-dictionary word sdcv-dictionary-list t))
    (sdcv-output-cleaner)))
;; ==================sdcv====================
;;; online
;; =================online===================
(defun online-output-cleaner ()
  (outline-show-all)
  (goto-char (point-min))
  (while (re-search-forward ".*Translate\\ from.*to.*:\n" nil t)
    (replace-match ""))
  (while (re-search-forward "\\\<1\\\. " nil t)
    (org-shiftmetaright)
    (org-shiftmetaright)
    (org-shiftmetaright))
  (while (re-search-forward "^*\\ .*" nil t)
    (org-shiftmetaright)
    (org-shiftmetaright)
    (org-shiftmetaright))
  (goto-char (point-min))
  (while (re-search-forward "^\n" nil t)
    (replace-match ""))
  (indent-region (point-min) (point-max))
  (goto-char (point-min)))
;;;###autoload
(defun swint-online-to-buffer (&optional _word)
  (interactive)
  (let ((word (or _word (swint-get-words-at-point))))
    (unless (member (buffer-name) '("*sdcv*" "*online*"))
      (window-configuration-to-register :sdcv))
    (delete-other-windows)
    (switch-to-buffer "*online*")
    (set-buffer "*online*")
    (buffer-disable-undo)
    (erase-buffer)
    (sdcv-mode)
    ;; 插入google-translate结果。
    (ignore-errors
      (insert (concat "*** Google Translate\n"))
      (if (pyim-string-match-p "\\cc" word)
          (google-translate-translate "zh-CN" "en" word 'current-buffer)
        (google-translate-translate "en" "zh-CN" word 'current-buffer)))
    ;; 插入bing-dict结果。
    (ignore-errors (bing-dict-brief word))
    ;; 插入youdao-dictionary结果。
    (ignore-errors
      (insert (concat "*** Youdao Dictionary\n"))
      (insert (youdao-dictionary--format-result word)))
    (online-output-cleaner)))
;; =================online===================
