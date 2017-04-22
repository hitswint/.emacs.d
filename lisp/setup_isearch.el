;;; Anzu
;; =====================Anzu=======================
(use-package anzu
  ;; Enabled at commands.
  :defer t
  :after pinyin-search
  :bind (("M-s M-r" . anzu-query-replace)
         ("M-s M-R" . anzu-query-replace-regexp))
  :config
  (global-anzu-mode 1)
  (set-face-attribute 'anzu-mode-line nil
                      :foreground "green" :weight 'bold)
  (custom-set-variables
   '(anzu-mode-lighter "")
   '(anzu-deactivate-region t)
   '(anzu-search-threshold 1000)
   '(anzu-replace-to-string-separator " => ")))
;; =====================Anzu=======================
;;; pinyin-search
;; ==================pinyin-search=================
(use-package pinyin-search
  ;; Enabled at commands.
  :defer t
  :commands (pinyin-search--pinyin-to-regexp symbol-name-at-point)
  :bind (("C-s" . isearch-forward-pinyin)
         ("C-r" . isearch-backward-pinyin))
  :config
  (defun symbol-name-at-point ()
    (let ((symbol (symbol-at-point)))
      (if symbol
          (symbol-name symbol)
        "")))
  (defun isearch-thing ()
    "Search for the current thing."
    (interactive)
    (let ((swint-isearch-current-thing
           (if (region-active-p)
               (buffer-substring (region-beginning) (region-end))
             (symbol-name-at-point))))
      (deactivate-mark)
      (isearch-yank-string swint-isearch-current-thing)))
  (define-key isearch-mode-map (kbd "C-t") 'isearch-thing)
  (define-key isearch-mode-map (kbd "C-q") #'isearch-toggle-pinyin)
  ;; 同时搜索中英文，与ace-jump一样，对于.*+?等正则表达式使用的符号无效。
  (defun swint-pinyin-search--pinyin-to-regexp (fn string)
    "Wrap for Pinyin searching."
    (let ((swint-regexp string)
          (string-converted (funcall fn string)))
      ;; 当搜索中文、符号或包含iuv时，不转换string。
      (unless (or (string-match "[iuv]" string)
                  (string-empty-p string-converted))
        (setq swint-regexp (concat string "\\|" string-converted)))
      swint-regexp))
  (advice-add 'pinyin-search--pinyin-to-regexp :around #'swint-pinyin-search--pinyin-to-regexp)
  (add-hook 'isearch-mode-end-hook (lambda () (setq pinyin-search-activated nil))))
;; ==================pinyin-search=================
;;; 拼音首字母搜索
;; =================拼音首字母搜索=================
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
;; =================拼音首字母搜索=================
(provide 'setup_isearch)
