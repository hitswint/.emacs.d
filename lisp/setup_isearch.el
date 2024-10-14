;;; isearch
;; =====================isearch====================
(use-package isearch
  ;; C-w/C-M-w isearch-yank-word-or-char/isearch-yank-symbol-or-char
  ;; C-y/M-y isearch-yank-kill/isearch-yank-pop-only
  ;; C-M-y/C-M-d/C-M-z/C-M-i isearch-yank-char/isearch-del-char/isearch-yank-until-char/isearch-complete
  ;; M-e isearch-edit-string，编辑模式下C-f/backspace等同于C-M-y/C-M-d
  ;; M-n/M-p isearch-ring-advance/isearch-ring-retreat
  ;; M-c/M-r isearch-toggle-case-fold/isearch-toggle-regexp
  ;; (M-s) M-</M->/C-e isearch-beginning-of-buffer/isearch-end-of-buffer/isearch-yank-line
  :diminish isearch-mode
  :custom
  (search-upper-case t)
  (search-whitespace-regexp ".*?")
  (isearch-lax-whitespace t)
  (isearch-regexp-lax-whitespace nil)
  (isearch-lazy-highlight t)
  (isearch-lazy-count t)
  (lazy-count-invisible-format " [%s]")
  (lazy-count-prefix-format nil)
  (lazy-count-suffix-format "   %s/%s"))
;; =====================isearch====================
;;; pinyin-search
;; ==================pinyin-search=================
(use-package pinyin-search
  :commands (isearch-current-thing isearch-function-with-pinyin)
  ;; 搜索时M-s p(isearch-toggle-pinyin)切换拼音搜索
  :bind (("C-s" . isearch-forward-pinyin)
         ("C-r" . isearch-backward-pinyin))
  :init
  (setq isearch-search-fun-function 'isearch-function-with-pinyin)
  :config
  (setq pinyin-search-message-prefix (propertize "[P] " 'face 'font-lock-keyword-face))
  (bind-key "C-t" 'isearch-yank-current isearch-mode-map)
  (bind-key "C-M-;" 'isearch-mark-current isearch-mode-map)
  (add-hook 'isearch-mode-end-hook (lambda () (setq pinyin-search-activated nil)))
  (defun isearch-yank-current ()
    (interactive)
    (let* ((current-match (if isearch-other-end
                              (if (< isearch-other-end (point))
                                  (buffer-substring-no-properties isearch-other-end (point))
                                (buffer-substring-no-properties (point) isearch-other-end))
                            ""))
           (current-thing (swint-get-current-thing))
           (isearch-string-new (if (string= current-match isearch-string)
                                   (if (string-match-p (regexp-quote isearch-string) current-thing)
                                       current-thing
                                     (if isearch-forward
                                         (concat isearch-string current-thing)
                                       (concat current-thing isearch-string)))
                                 current-match)))
      (unless (string-empty-p isearch-string)
        (isearch-del-char (length isearch-string)))
      (isearch-yank-string isearch-string-new)))
  (defun isearch-current-thing ()
    (prog1 (if isearch-regexp
               isearch-string
             (regexp-quote isearch-string))
      (let ((search-nonincremental-instead nil))
        (isearch-exit))))
  (defun isearch-mark-current ()
    (interactive)
    ;; (let ((start (overlay-start isearch-overlay))
    ;;       (end (overlay-end isearch-overlay)))
    ;;   (push-mark start t t)
    ;;   (goto-char end))
    (push-mark isearch-other-end t t)
    (isearch-exit))
  ;; 转换后的regexp可能超过长度限制，导致re-search-forward产生"Regular expression too big"错误
  (defun pinyin-search--pinyin-to-regexp/override (pinyin)
    "Wrap for Pinyin searching."
    (let ((string-converted (pinyinlib-build-regexp-string pinyin nil nil nil)))
      (condition-case nil
          (progn (save-excursion (re-search-forward string-converted nil t)) string-converted)
        (error pinyin))))
  (advice-add 'pinyin-search--pinyin-to-regexp :override #'pinyin-search--pinyin-to-regexp/override))
;; ==================pinyin-search=================
(provide 'setup_isearch)
