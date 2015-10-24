;; ===================Anzu====================
;; 在win上使用elpa安装，但是在lin上无法安装，因为版本不满足要求。使用elpa安装cl-lib-0.5，然后手动安装anzu。
(require 'anzu)
(global-anzu-mode +1)
(global-set-key (kbd "M-*") 'anzu-query-replace)
(global-set-key (kbd "C-M-*") 'anzu-query-replace-regexp)
(set-face-attribute 'anzu-mode-line nil
                    :foreground "green" :weight 'bold)
(custom-set-variables
 '(anzu-mode-lighter "")
 '(anzu-deactivate-region t)
 '(anzu-search-threshold 1000)
 '(anzu-replace-to-string-separator " => "))
;; ===================Anzu====================
;; ===================pinyin-search=====================
;; 安装时由于isearch-mode-map中已经设定M-s为helm-swoop，导致快捷键设定失败，修改之。
(require 'pinyin-search)
(define-key isearch-mode-map (kbd "C-q") #'isearch-toggle-pinyin)
(global-set-key (kbd "C-s") 'isearch-forward-pinyin)
(global-set-key (kbd "C-r") 'isearch-backward-pinyin)
;; 同时搜索中英文，与ace-jump一样，对于.*+?等正则表达式使用的符号无效
(defun swint-pinyin-search--pinyin-to-regexp (string)
  "Wrap for Pinyin searching."
  (let ((swint-regexp ""))
    (if (or (string-match "[iuv]" string) ;当字符串中有iuv时，不转换string
            (string-empty-p (pinyin-search--pinyin-to-regexp string))) ;当搜索中文或符号时，不转换string
        (setq swint-regexp string)
      (setq swint-regexp (concat string "\\|" (pinyin-search--pinyin-to-regexp string))))
    swint-regexp))
(defun isearch-function-with-pinyin ()
  "Wrap for Pinyin searching."
  (if pinyin-search-activated
      ;; Return the function to use for pinyin search
      `(lambda (string bound noerror)
         (funcall (if ,isearch-forward
                      're-search-forward
                    're-search-backward)
                  (swint-pinyin-search--pinyin-to-regexp string) bound noerror))
    ;; Return default function
    (isearch-search-fun-default)))
(add-hook 'isearch-mode-end-hook
          (lambda ()
            (setq pinyin-search-activated nil)))
;; ===================pinyin-search=====================
;; ==============extended isearch==================
(defun symbol-name-at-point ()
  (let ((symbol (symbol-at-point)))
    (if symbol
        (symbol-name symbol)
      "")))
(defun current-thing ()
  "Return the current \"thing\":
- if the region is active, return the region's text and deactivate the mark
- else return the symbol at point or the empty string."
  (let ((thing (if (region-active-p)
                   (buffer-substring (region-beginning) (region-end))
                 (symbol-name-at-point))))
    (deactivate-mark)
    thing))
(defun isearch-thing ()
  "Search for the current \"thing\":
- if the region is active, return the region's text and deactivate the mark
- else return the symbol at point or the empty string."
  (interactive)
  (isearch-yank-string (current-thing)))
(define-key isearch-mode-map (kbd "C-t") 'isearch-thing)
;; ==============extended isearch==================
;; ==============拼音首字母搜索===============
;; 使用pinyin-search替代，但这个可以同时搜索中英文，保留之。
(defun swint-pinyin-search-forward (&optional bound noerror count)
  (interactive)
  (let ((swint-current-buffer (str-unicode-to-pinyin-initial (buffer-substring-no-properties (point-min) (point-max))))
        (current-point (point))
        (string (car minibuffer-history)))
    (setq string (read-string (format  "Pinyin search(default %s): " string)
                              nil nil string))
    (with-temp-buffer
      (insert swint-current-buffer)
      (goto-char current-point)
      (if (string-match-p string (buffer-substring-no-properties current-point (point-max)))
          (re-search-forward string bound noerror count)
        (progn
          (goto-char (point-min))
          (re-search-forward string bound noerror count)))
      (setq swint-match-end (match-end 0)))
    (goto-char swint-match-end)))
(defun swint-pinyin-search-backward (&optional bound noerror count)
  (interactive)
  (let ((swint-current-buffer (str-unicode-to-pinyin-initial (buffer-substring-no-properties (point-min) (point-max))))
        (current-point (point))
        (string (car minibuffer-history)))
    (setq string (read-string (format  "Pinyin search(default %s): " string)
                              nil nil string))
    (with-temp-buffer
      (insert swint-current-buffer)
      (goto-char current-point)
      (if (string-match-p string (buffer-substring-no-properties (point-min) current-point))
          (re-search-backward string bound noerror count)
        (progn
          (goto-char (point-max))
          (re-search-backward string bound noerror count)))
      (setq swint-match-beginning (match-beginning 0)))
    (goto-char swint-match-beginning)))
(global-set-key (kbd "C-S-s") 'swint-pinyin-search-forward)
(global-set-key (kbd "C-S-r") 'swint-pinyin-search-backward)
(define-key minibuffer-local-map (kbd "C-S-s") 'exit-minibuffer)
(define-key minibuffer-local-map (kbd "C-S-r") 'exit-minibuffer)
;; ==============拼音首字母搜索===============
(provide 'setup_isearch)
