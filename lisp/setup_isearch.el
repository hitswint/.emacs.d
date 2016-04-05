;; =====================Anzu=======================
(use-package anzu
  ;; Enabled at commands.
  :defer t
  :after pinyin-search
  :bind (("M-*" . anzu-query-replace)
         ("C-M-*" . anzu-query-replace-regexp))
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
;; ==================pinyin-search=================
;; 安装时由于isearch-mode-map中已经设定M-s为helm-swoop，导致快捷键设定失败，修改之。
(use-package pinyin-search
  ;; Enabled at commands.
  :defer t
  :commands pinyin-search--pinyin-to-regexp
  :bind (("C-s" . swint-isearch-forward)
         ("C-r" . swint-isearch-backward))
  :config
  (defun swint-isearch-forward ()
    (interactive)
    (if (region-active-p)
        (setq swint-isearch-current-thing (buffer-substring (region-beginning) (region-end)))
      (setq swint-isearch-current-thing (symbol-name-at-point)))
    (isearch-forward-pinyin))
  (defun swint-isearch-backward ()
    (interactive)
    (if (region-active-p)
        (setq swint-isearch-current-thing (buffer-substring (region-beginning) (region-end)))
      (setq swint-isearch-current-thing (symbol-name-at-point)))
    (isearch-backward-pinyin))
  ;; ==========Isearch thing at point===========
  (defvar swint-isearch-current-thing nil)
  (defun symbol-name-at-point ()
    (let ((symbol (symbol-at-point)))
      (if symbol
          (symbol-name symbol)
        "")))
  (defun isearch-thing ()
    "Search for the current thing."
    (interactive)
    (if (region-active-p)
        (deactivate-mark))
    (isearch-yank-string swint-isearch-current-thing))
  (define-key isearch-mode-map (kbd "C-t") 'isearch-thing)
  ;; ==========Isearch thing at point===========
  (define-key isearch-mode-map (kbd "C-q") #'isearch-toggle-pinyin)
  ;; 同时搜索中英文，与ace-jump一样，对于.*+?等正则表达式使用的符号无效。
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
              (setq pinyin-search-activated nil))))
;; ==================pinyin-search=================
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
