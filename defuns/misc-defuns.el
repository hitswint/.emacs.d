;;; 从xsel复制粘贴
;; =================从xsel复制粘贴=================
;; 通过xsel与其他程序进行复制粘贴。
(defun xsel-paste-primary()
  (interactive)
  (insert (shell-command-to-string "xsel -o -p </dev/null")))
(global-set-key (kbd "C-x C-y") 'xsel-paste-primary)
;; =================从xsel复制粘贴=================
;;; WordsCount
;; ===================WordsCount===================
(defvar wc-regexp-chinese-char-and-punc
  (rx (category chinese)))
(defvar wc-regexp-chinese-punc
  "[。，！？；：「」『』（）、【】《》〈〉※—]")
(defvar wc-regexp-english-word
  "[a-zA-Z0-9-]+")
(defun swint-count-words-region ()
  "「較精確地」統計中/日/英文字數。
- 文章中的註解不算在字數內。
- 平假名與片假名亦包含在「中日文字數」內，每個平/片假名都算單獨一個字（但片假
  名不含連音「ー」）。
- 英文只計算「單字數」，不含標點。
- 韓文不包含在內。
※計算標準太多種了，例如英文標點是否算入、以及可能有不太常用的標點符號沒算入等
。且中日文標點的計算標準要看 Emacs 如何定義特殊標點符號如ヴァランタン・アルカン
中間的點也被 Emacs 算為一個字而不是標點符號。"
  (interactive)
  (let* ((words-to-be-counted (if mark-active
                                  (buffer-substring-no-properties (region-beginning) (region-end))
                                (buffer-substring-no-properties (point-min) (point-max)))) ;取全文或mark区域
         (v-buffer-string
          (progn
            (if (eq major-mode 'org-mode) ; 去掉 org 文件的 OPTIONS（以#+開頭）
                (setq v-buffer-string (replace-regexp-in-string "^#\\+.+" ""
                                                                words-to-be-counted))
              (setq v-buffer-string words-to-be-counted))
            (replace-regexp-in-string (format "^ *%s *.+" comment-start) "" v-buffer-string))) ; 把註解行刪掉（不把註解算進字數）。
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
(global-set-key (kbd "M-g =") 'swint-count-words-region)
;; ===================WordsCount===================
;;; get-words-at-point
;; ===============get-words-at-point===============
(defun swint-get-words-at-point ()
  "Get words at point, use pyim-get-words-list-at-point to deal with chinese."
  (interactive)
  (if mark-active
      (buffer-substring-no-properties (region-beginning) (region-end))
    (let ((words-at-point (if (equal (point) (point-at-eol))
                              (pyim-cwords-at-point)
                            (pyim-cwords-at-point t))))
      (if (<= (length words-at-point) 1)
          (read-string (format "Get Words (default %s): " (car (car words-at-point)))
                       nil nil (car (car words-at-point)))
        (ivy-read "Get Words:" (remove-duplicates (mapcar 'car words-at-point)))))))
;; ===============get-words-at-point===============
;;; swint-pandoc-output
;; ==============swint-pandoc-output===============
(defun swint-pandoc-output ()
  (interactive)
  (let ((output-format (read-string "Output format: ")))
    (mapcar #'(lambda (x) (shell-command (concat "pandoc -o " (file-name-base x)
                                                 "." output-format " " (file-name-nondirectory x))))
            (if (eq major-mode 'dired-mode)
                (dired-get-marked-files)
              (list (buffer-file-name))))))
(global-set-key (kbd "M-g o") 'swint-pandoc-output)
;; ==============swint-pandoc-output===============
;;; swint-pdftk-output
;; ==============swint-pdftk-output================
(defun swint-pdftk-output ()
  (interactive)
  (let ((args (read-string "Pdftk args(1-2west 4 5-end): ")))
    (mapcar #'(lambda (pdf-file)
                (shell-command (concat "pdftk " (file-name-nondirectory pdf-file) " cat "
                                       args " output " (concat (file-name-base pdf-file) "-new.pdf"))))
            (remove-if-not #'(lambda (x) (string-equal (downcase (file-name-extension x)) "pdf"))
                           (if (eq major-mode 'dired-mode)
                               (dired-get-marked-files)
                             (list (buffer-file-name)))))))
(global-set-key (kbd "M-g t") 'swint-pdftk-output)
;; ==============swint-pdftk-output================
;;; show-some-last-messages
;; ============show-some-last-messages=============
(defcustom default-messages-to-show 4
  "Default number of messages for `show-some-last-messages'.")
(defun show-some-last-messages (count)
  "Show COUNT last lines of the `*Messages*' buffer."
  (interactive "P")
  (setq count (if count (prefix-numeric-value count)
                default-messages-to-show))
  (save-excursion
    (set-buffer "*Messages*")
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
(global-set-key (kbd "M-s M-e") 'show-some-last-messages)
;; ============show-some-last-messages=============
