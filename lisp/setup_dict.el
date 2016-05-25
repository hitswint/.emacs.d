;;; stardict
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
(defvar sdcv-dictionary-list '("朗道英汉字典5.0"
                               "朗道汉英字典5.0"
                               "XDICT英汉辞典"
                               "XDICT汉英辞典"
                               "新世纪英汉科技大词典"
                               "新世纪汉英科技大词典"
                               "Collins\\ Cobuild\\ English\\ Dictionary"))
(defun sdcv-search-with-dictionary (word dictionary-list)
  "Search some WORD with dictionary list."
  (mapconcat (lambda (dict)
               (replace-regexp-in-string "^对不起，没有发现和.*\n" ""
                                         (shell-command-to-string
                                          (format "sdcv -n %s %s"
                                                  (concat "-u " dict) word))))
             dictionary-list "\n"))
(defun swint-sdcv-to-buffer ()
  (interactive)
  (let ((word (swint-get-words-at-point)))
    (cond
     (is-lin
      (unless (equal (buffer-name) "*sdcv*")
        (switch-to-buffer-other-window "*sdcv*"))
      (set-buffer "*sdcv*")
      (buffer-disable-undo)
      (erase-buffer)
      (insert (sdcv-search-with-dictionary word sdcv-dictionary-list))
      (yasdcv--output-cleaner:common)
      (sdcv-mode)
      (show-all)
      (indent-region (point-min) (point-max))
      (goto-char (point-min)))
     (is-win
      (w32-shell-execute "open" "sdcv" (concat "--data-dir c:/Users/swint/.stardict " word " stardict"))))))
(defun yasdcv--output-cleaner:common ()
  "从yasdcv借来的函数。"
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
  (goto-char (point-min)))
;; ==================stardict====================
;;; bing-dict
;; ==================bing-dict===================
(use-package bing-dict
  ;; Enabled at commands.
  :defer t
  :bind ("C-M-@" . swint-bing-google-translate)
  :config
  (defun swint-bing-google-translate ()
    (interactive)
    (let ((word (swint-get-words-at-point)))
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
;;; google-translate
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
(provide 'setup_dict)
