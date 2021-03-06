;;; Anzu
;; =====================Anzu=======================
(def-package! anzu
  :diminish anzu-mode
  :bind (("M-s M-r" . anzu-query-replace)
         ("M-s M-R" . anzu-query-replace-regexp))
  :config
  (global-anzu-mode 1)
  (setq anzu-deactivate-region t
        anzu-replace-to-string-separator " => "
        anzu-search-threshold 1000)
  (set-face-attribute 'anzu-mode-line nil :foreground "green" :weight 'bold))
;; =====================Anzu=======================
;;; pinyin-search
;; ==================pinyin-search=================
(def-package! pinyin-search
  :commands symbol-name-at-point
  :bind (("C-s" . isearch-forward-pinyin)
         ("C-r" . isearch-backward-pinyin))
  :config
  (require 'anzu)
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
  (define-key isearch-mode-map (kbd "C-q") 'isearch-toggle-pinyin)
  ;; 转换后的regexp可能超过长度限制，导致re-search-forward产生"Regular expression too big"错误。
  (defun pinyin-search--pinyin-to-regexp/override (pinyin)
    "Wrap for Pinyin searching."
    (let ((string-converted (pinyinlib-build-regexp-string pinyin nil nil nil)))
      (condition-case nil
          (progn (save-excursion (re-search-forward string-converted nil t)) string-converted)
        (error pinyin))))
  (advice-add 'pinyin-search--pinyin-to-regexp :override #'pinyin-search--pinyin-to-regexp/override)
  (add-hook 'isearch-mode-end-hook (lambda () (setq pinyin-search-activated nil))))
;; ==================pinyin-search=================
(provide 'setup_isearch)
