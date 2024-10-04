;;; 从xsel复制粘贴
;; =================从xsel复制粘贴=================
;; 通过xsel与其他程序进行复制粘贴
;;;###autoload
(defun xsel-paste-primary()
  (interactive)
  (insert (shell-command-to-string "xsel -o -p </dev/null")))
;; =================从xsel复制粘贴=================
;;; WordsCount
;; ===================WordsCount===================
(defvar wc-regexp-chinese-char-and-punc
  (rx (category chinese)))
(defvar wc-regexp-chinese-punc
  "[。，！？；：「」『』（）、【】《》〈〉※—]")
(defvar wc-regexp-english-word
  "[a-zA-Z0-9-]+")
;;;###autoload
(defun swint-count-words-region ()
  (interactive)
  (let* ((words-to-be-counted (if mark-active
                                  (buffer-substring-no-properties (region-beginning) (region-end))
                                (buffer-substring-no-properties (point-min) (point-max)))) ;取全文或mark区域
         (v-buffer-string
          (progn
            (if (eq major-mode 'org-mode) ;去掉org文件的OPTIONS
                (setq v-buffer-string (replace-regexp-in-string "^#\\+.+" ""
                                                                words-to-be-counted))
              (setq v-buffer-string words-to-be-counted))
            (replace-regexp-in-string (format "^ *%s *.+" comment-start) "" v-buffer-string))) ;把注释行删掉
         (chinese-char-and-punc 0)
         (chinese-punc 0)
         (english-word 0)
         (chinese-char 0))
    (with-temp-buffer
      (insert v-buffer-string)
      (goto-char (point-min))
      ;; 中文（含標點、片假名）
      (while (re-search-forward wc-regexp-chinese-char-and-punc nil :no-error)
        (setq chinese-char-and-punc (1+ chinese-char-and-punc)))
      ;; 中文標點符號
      (goto-char (point-min))
      (while (re-search-forward wc-regexp-chinese-punc nil :no-error)
        (setq chinese-punc (1+ chinese-punc)))
      ;; 英文字數（不含標點）
      (goto-char (point-min))
      (while (re-search-forward wc-regexp-english-word nil :no-error)
        (setq english-word (1+ english-word))))
    (setq chinese-char (- chinese-char-and-punc chinese-punc))
    (message
     (format "中文字數(不含标点)：%s
中文字數(包含标点)：%s
英文字數(不含标点)：%s
========================
中英文合計(不含标点)：%s
中英文合計(包含标点)：%s"
             chinese-char chinese-char-and-punc english-word
             (+ chinese-char english-word)
             (+ chinese-char-and-punc english-word)))))
;; ===================WordsCount===================
;;; get-words-at-point
;; ===============get-words-at-point===============
;;;###autoload
(defun swint-get-words-at-point ()
  "Get words at point, use pyim-get-words-list-at-point to deal with chinese."
  (interactive)
  (let* ((orig-kill (current-kill 0))
         (selected-text (cond ((derived-mode-p 'pdf-view-mode)
                               (when (pdf-view-active-region-p)
                                 (prog1 (mapconcat 'identity (pdf-view-active-region-text) " ")
                                   (pdf-view-deactivate-region))))
                              ((eq major-mode 'eaf-mode)
                               (if (equal eaf--buffer-app-name "pdf-viewer")
                                   (eaf-call-sync "execute_function" eaf--buffer-id "get_select")
                                 (execute-kbd-macro (kbd "M-w"))
                                 (sit-for 1)
                                 (unless (equal orig-kill (current-kill 0))
                                   (current-kill 0))))
                              (t (if mark-active
                                     (prog1 (buffer-substring-no-properties (region-beginning) (region-end))
                                       (deactivate-mark))
                                   (or (cl-remove-duplicates (mapcar 'car (if (equal (point) (point-at-eol))
                                                                              (pyim-cstring-words-at-point)
                                                                            (pyim-cstring-words-at-point t))))
                                       (remove nil (list (thing-at-point 'word t))))))))
         (target-text (if (and (stringp selected-text) (not (string-empty-p selected-text)))
                          selected-text
                        (if (<= (length selected-text) 1)
                            (let ((default-word (if (consp selected-text)
                                                    (car selected-text)
                                                  orig-kill)))
                              ;; M-n可调出并修改default-word
                              (read-string (format "Get Words(default %s): " (propertize default-word 'face 'font-lock-variable-name-face))
                                           nil nil default-word))
                          (ivy-read "Get Words:" selected-text)))))
    ;; 当read-string使eaf窗口改变大小时，posframe窗口闪现即消失
    ;; 使用sit-for等待一段时间，使eaf窗口恢复大小
    (when (eq major-mode 'eaf-mode)
      (sit-for 0.2))
    (replace-regexp-in-string "\n" " " (string-trim target-text))))
;;;###autoload
(defun swint-get-current-thing ()
  (if (region-active-p)
      (prog1 (buffer-substring (region-beginning) (region-end))
        (deactivate-mark))
    (symbol-name-at-point)))
(defun symbol-name-at-point ()
  (let ((symbol (symbol-at-point)))
    (if symbol
        (symbol-name symbol)
      "")))
;; ===============get-words-at-point===============
;;; show-some-last-messages
;; ============show-some-last-messages=============
(defvar default-messages-to-show 10
  "Default number of messages for `show-some-last-messages'.")
;;;###autoload
(defun show-some-last-messages (count)
  "Show COUNT last lines of the `*Messages*' buffer."
  (interactive "P")
  (setq count (if count (prefix-numeric-value count)
                default-messages-to-show))
  (with-current-buffer "*Messages*"
    (let ((prev-point-max (point-max-marker))
          (inhibit-read-only t))
      (message "%s"
               (progn
                 (set-buffer "*Messages*")
                 (buffer-substring-no-properties
                  (progn
                    (goto-char (point-max))
                    (unless (bolp)
                      (insert "\n"))
                    (forward-line (- count))
                    (point))
                  (point-max))))
      (delete-region (point-max) prev-point-max))))
;;;###autoload
(defun switch-to-messages-buffer ()
  "Show COUNT last lines of the `*Messages*' buffer."
  (interactive)
  (swint-persp-switch "i")
  (delete-other-windows)
  (switch-to-buffer "*Messages*")
  (goto-char (point-max)))
;; ============show-some-last-messages=============
