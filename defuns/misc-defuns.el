;; ==============从xsel剪切板粘贴===============
;; 解决无法从llpp中粘贴的问题，但未写入kill-ring。
(defun xsel-paste-function()
  (interactive)
  (insert (shell-command-to-string "xsel -o")))
(global-set-key (kbd "<S-insert>") 'xsel-paste-function)
;; ==============从xsel剪切板粘贴===============
;; ==============WordsCount===============
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
(global-set-key (kbd "M-=") 'swint-count-words-region)
;; ==============WordsCount===============
