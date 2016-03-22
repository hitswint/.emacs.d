;; ==================stardict====================
;; Major mode for sdcv.
(define-derived-mode sdcv-mode org-mode
  (interactive)
  (setq major-mode 'sdcv-mode)
  (setq mode-name "sdcv")
  (local-set-key (kbd "n") 'next-line)
  (local-set-key (kbd "p") 'previous-line)
  (local-set-key (kbd "SPC") 'scroll-up)
  (local-set-key (kbd "DEL") 'scroll-down)
  (local-set-key (kbd "d") 'kid-sdcv-to-buffer)
  (local-set-key (kbd "q") 'kill-buffer-and-window)
  (run-hooks 'sdcv-mode-hook))
(defun swint-get-words-at-points ()
  "Get words at point, use pyim-get-words-list-at-point to deal with chinese."
  (interactive)
  (if mark-active
      (buffer-substring-no-properties (region-beginning) (region-end))
    (let ((words-at-point (if (equal (point) (point-at-eol))
                              (pyim-get-words-list-at-point)
                            (pyim-get-words-list-at-point t))))
      (if (<= (length words-at-point) 1)
          (read-string (format "Search the dictionary for (default %s): " (car (car words-at-point)))
                       nil nil (car (car words-at-point)))
        (ido-completing-read "Get Words:" (mapcar 'car words-at-point))))))
(defun swint-sdcv-to-buffer ()
  (interactive)
  (let ((word (swint-get-words-at-points)))
    (cond
     (is-lin
      (set-buffer (get-buffer-create "*sdcv*"))
      (buffer-disable-undo)
      (erase-buffer)
      (let ((process (start-process-shell-command "sdcv" "*sdcv*" "sdcv" "-n --data-dir ~/.stardict/dict" word)))
        (set-process-sentinel
         process
         (lambda (process signal)
           (when (memq (process-status process) '(exit signal))
             (unless (equal (buffer-name) "*sdcv*")
               (switch-to-buffer-other-window "*sdcv*"))
             (yasdcv--output-cleaner:common)
             (sdcv-mode)
             (show-all)
             (while (re-search-forward "*** Collins Cobuild English Dictionary " nil t)
               (hide-entry))
             (indent-region (point-min) (point-max))
             (goto-char (point-min)))))))
     (is-win
      (w32-shell-execute "open" "sdcv" (concat "--data-dir c:/Users/swint/.stardict " word " stardict"))))))
(defun yasdcv--output-cleaner:common ()
  "从yasdcv借来的函数。"
  (goto-char (point-min))
  (while (re-search-forward "-->\\(.*\\)\n-->\\(.*\\)" nil t)
    (replace-match "*** \\1 (\\2)"))
  (goto-char (point-min))
  (while (re-search-forward "\n+" nil t)
    (replace-match "\n"))
  (goto-char (point-min))
  (when is-lin
    (kill-line 1)))
;; ==================stardict====================
;; ==================bing-dict===================
(use-package bing-dict
  ;; Enabled at commands.
  :defer t
  :bind ("C-M-@" . swint-bing-google-translate)
  :config
  (defun swint-bing-google-translate ()
    (interactive)
    (let ((word (swint-get-words-at-points)))
      (when (internet-active-p)
        (unless (equal (buffer-name) "*bing-google*")
          (switch-to-buffer-other-window "*bing-google*"))
        (set-buffer "*bing-google*")
        (buffer-disable-undo)
        (erase-buffer)
        (sdcv-mode)
        ;; 输入google-translate结果。
        (save-excursion (if (pyim-string-match-p "\\cc" word)
                            (google-translate-translate "zh-CN" "en" word)
                          (google-translate-translate "en" "zh-CN" word)))
        (goto-char (point-max))
        (insert (concat "*** Google Translate" " (" word ")\n"
                        (with-current-buffer "*Google Translate*"
                          (buffer-substring (point-at-bol 3) (point-max)))))
        (kill-buffer "*Google Translate*")
        ;; 输入bing-dict结果。
        (swint-bing-dict-brief word)
        (show-all)
        (while (re-search-forward "\\\<1\\\. " nil t)
          (org-shiftmetaright)
          (org-shiftmetaright)
          (org-shiftmetaright))
        (indent-region (point-min) (point-max)))))
  (defvar swint-bing-dict-result nil)
  (defun swint-bing-dict-brief-cb (status keyword)
    (set-buffer-multibyte t)
    (bing-dict--delete-response-header)
    (condition-case nil
        (if (buffer-live-p (get-buffer "*bing-google*"))
            (if (bing-dict--has-machine-translation-p)
                (with-current-buffer "*bing-google*"
                  (goto-char (point-max))
                  (insert (concat "*** Bing Dict" " (" keyword ")\n"))
                  (indent-for-tab-command)
                  (insert (concat (propertize (bing-dict--machine-translation)
                                              'face
                                              'font-lock-doc-face))))
              (let ((query-word (propertize keyword 'face 'font-lock-keyword-face))
                    (pronunciation (bing-dict--pronunciation))
                    (short-exps (mapconcat 'identity (bing-dict--definitions)
                                           (propertize " | "
                                                       'face
                                                       'font-lock-builtin-face))))
                (with-current-buffer "*bing-google*"
                  (goto-char (point-max))
                  (insert (concat "*** Bing Dict" " (" keyword ")\n"))
                  (indent-for-tab-command)
                  (if short-exps
                      (insert (concat pronunciation short-exps))
                    (insert "No results")))))
          (bing-dict-brief keyword))
      (error
       (with-current-buffer "*bing-google*"
         (goto-char (point-max))
         (insert (concat "*** Bing Dict" " (" keyword ")\n"))
         (indent-for-tab-command)
         (insert "No results")))))
  (defun swint-bing-dict-brief (&optional word)
    (interactive)
    (let ((keyword (or word (read-string
                             "Search Bing dict: "
                             (if (use-region-p)
                                 (buffer-substring-no-properties
                                  (region-beginning) (region-end))
                               (thing-at-point 'word))))))
      (url-retrieve (concat "http://www.bing.com/dict/search?q="
                            (url-hexify-string keyword))
                    'swint-bing-dict-brief-cb
                    `(,(decode-coding-string keyword 'utf-8))
                    t t))))
;; ==================bing-dict===================
;; ===============google-translate===============
(use-package google-translate
  ;; Enabled at commands.
  :defer t
  :commands google-translate-translate
  :init
  (setq google-translate-base-url
        "http://translate.google.cn/translate_a/single")
  (setq google-translate-listen-url
        "http://translate.google.cn/translate_tts")
  (setq google-translate-translation-directions-alist
        '(("en" . "zh-CN") ("zh-CN" . "en"))))
;; ===============google-translate===============
;; ====================youdao====================
(defun youdao-sample-sentences ()
  (interactive)
  (let ((word (swint-get-words-at-points)))
    (browse-url
     (concat "http://dict.youdao.com/search?le=eng&q=lj%3A"
             (cond
              (is-lin word)
              ;; 解决w3m无法解析网址的问题
              (is-win (w3m-url-encode-string word 'utf-8)))
             "&keyfrom=dict.top"))))
(global-set-key (kbd "M-@") 'youdao-sample-sentences)
;; ====================youdao====================
(provide 'setup_dict)
