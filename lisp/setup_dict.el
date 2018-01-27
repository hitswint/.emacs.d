;;; stardict
;; ==================stardict====================
(define-derived-mode sdcv-mode org-mode nil
  "Major mode for sdcv."
  (when is-lin (fcitx--sdcv-maybe-deactivate))
  (local-set-key (kbd "q") '(lambda () (interactive)
                              (ignore-errors (kill-process "ec_sleep"))
                              (swint-kill-this-buffer)
                              (jump-to-register :sdcv) (when is-lin (fcitx--sdcv-maybe-activate)))))
(defvar sdcv-dictionary-list '("朗道英汉字典5.0"
                               "朗道汉英字典5.0"
                               "XDICT英汉辞典"
                               "XDICT汉英辞典"
                               "新世纪英汉科技大词典"
                               "新世纪汉英科技大词典"
                               "Collins\\ Cobuild\\ English\\ Dictionary"))
(defun sdcv-search-with-dictionary (word dictionary-list &optional to-buffer)
  "Search some WORD with dictionary list."
  (mapconcat (lambda (dict)
               (replace-regexp-in-string "^对不起，没有发现和.*\n" ""
                                         (shell-command-to-string
                                          (format "sdcv -n %s %s"
                                                  (concat "-u " dict) word))))
             (if to-buffer dictionary-list
               (butlast dictionary-list)) (if to-buffer "\n")))
(global-set-key (kbd "M-s d") 'swint-sdcv-to-tip)
(global-set-key (kbd "M-s D") 'swint-sdcv-to-buffer)
(defun swint-sdcv-to-tip (&optional _word)
  "Search WORD simple translate result."
  (interactive)
  (let ((word (or _word (swint-get-words-at-point))))
    (pos-tip-show
     (replace-regexp-in-string "-->\\(.*\\)\n-->\\(.*\\)\n" "\\1：\\2"
                               (replace-regexp-in-string
                                "\\(^Found\\ [[:digit:]]+\\ items,\\ similar\\ to \\(.*\\)\\.\n\\)" ""
                                (sdcv-search-with-dictionary word sdcv-dictionary-list)))
     nil nil nil 0)))
(defun swint-sdcv-to-buffer (&optional _word)
  (interactive)
  (let ((word (or _word (swint-get-words-at-point))))
    (cond
     (is-lin
      (unless (member (buffer-name) '("*sdcv*" "*online*"))
        (window-configuration-to-register :sdcv))
      (delete-other-windows)
      (switch-to-buffer "*sdcv*")
      (set-buffer "*sdcv*")
      (buffer-disable-undo)
      (erase-buffer)
      (sdcv-mode)
      (insert (sdcv-search-with-dictionary word sdcv-dictionary-list t))
      (sdcv-output-cleaner))
     (is-win
      (w32-shell-execute "open" "sdcv"
                         (concat "--data-dir " (expand-file-name "~/") ".stardict " word " stardict"))))))
(defun sdcv-output-cleaner ()
  (show-all)
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
  (indent-region (point-min) (point-max))
  (goto-char (point-min)))
;; ==================stardict====================
;;; online
;; ===================online=====================
(global-set-key (kbd "M-s M-d") 'hydra-online-dict/body)
(global-set-key (kbd "M-s M-D") 'swint-online-to-buffer)
(defhydra hydra-online-dict (:color blue)
  "Online Dict"
  ("b" bing-dict-brief-cb-at-point "Bing Dict")
  ("g" google-translate-to-tip "Google Translate")
  ("y" youdao-dictionary-to-tip "Youdao Dict"))
(defun swint-online-to-buffer (&optional _word)
  (interactive)
  (let ((word (or _word (swint-get-words-at-point))))
    (unless (member (buffer-name) '("*sdcv*" "*online*"))
      (window-configuration-to-register :sdcv))
    (delete-other-windows)
    (switch-to-buffer "*online*")
    (set-buffer "*online*")
    (buffer-disable-undo)
    (erase-buffer)
    (sdcv-mode)
    ;; 插入google-translate结果。
    (insert (concat "*** Google Translate\n"))
    (if (pyim-string-match-p "\\cc" word)
        (google-translate-translate "zh-CN" "en" word 'current-buffer)
      (google-translate-translate "en" "zh-CN" word 'current-buffer))
    ;; 插入bing-dict结果。
    (bing-dict-brief word)
    ;; 插入youdao-dictionary结果。
    (insert (concat "*** Youdao Dictionary\n"))
    (insert (youdao-dictionary--format-result word))
    (online-output-cleaner)))
(defun online-output-cleaner ()
  (show-all)
  (goto-char (point-min))
  (while (re-search-forward ".*Translate\\ from.*to.*:\n" nil t)
    (replace-match ""))
  (while (re-search-forward "\\\<1\\\. " nil t)
    (org-shiftmetaright)
    (org-shiftmetaright)
    (org-shiftmetaright))
  (while (re-search-forward "^*\\ .*" nil t)
    (org-shiftmetaright)
    (org-shiftmetaright)
    (org-shiftmetaright))
  (goto-char (point-min))
  (while (re-search-forward "^\n" nil t)
    (replace-match ""))
  (indent-region (point-min) (point-max))
  (goto-char (point-min)))
;; ===================online=====================
;;; bing-dict
;; ==================bing-dict===================
(use-package bing-dict
  :commands (bing-dict-brief bing-dict-brief-cb-at-point)
  :config
  (defun bing-dict-brief-cb-action (&rest args)
    "Output translation to pos-tip or *online* buffer."
    (if (get-buffer "*online*")
        (with-current-buffer "*online*"
          (goto-char (point-min))
          (insert (concat "*** Bing Dict\n"))
          (insert (concat (current-message) "\n")))
      (pos-tip-show (current-message) nil nil nil 0)))
  (advice-add 'bing-dict-brief-cb :after #'bing-dict-brief-cb-action)
  (defun bing-dict-brief-cb-at-point (&optional _word)
    "Search word at point."
    (interactive)
    (let ((word (or _word (swint-get-words-at-point))))
      (bing-dict-brief word))))
;; ==================bing-dict===================
;;; google-translate
;; ===============google-translate===============
(use-package google-translate
  :commands (google-translate-translate google-translate-to-tip)
  :init
  (setq google-translate-base-url
        "http://translate.google.cn/translate_a/single")
  (setq google-translate-listen-url
        "http://translate.google.cn/translate_tts")
  (setq google-translate--tkk-url
        "http://translate.google.cn/")
  (setq google-translate-translation-directions-alist
        '(("en" . "zh-CN") ("zh-CN" . "en")))
  :config
  (defun google-translate-current-buffer-output-translation-with-detail (gtos)
    "Output translation to current buffer."
    (google-translate-buffer-insert-translation gtos))
  (advice-add 'google-translate-current-buffer-output-translation :override
              #'google-translate-current-buffer-output-translation-with-detail)
  (defun google-translate-to-tip (&optional _word)
    "Search WORD simple translate result."
    (interactive)
    (let ((word (or _word (swint-get-words-at-point))))
      (if (pyim-string-match-p "\\cc" word)
          (google-translate-translate "zh-CN" "en" word 'popup)
        (google-translate-translate "en" "zh-CN" word 'popup)))))
;; ===============google-translate===============
;;; youdao-dictionary
;; ===============youdao-dictionary==============
(use-package youdao-dictionary
  :commands (youdao-dictionary--format-result youdao-dictionary-to-tip)
  :config
  (defun youdao-dictionary-to-tip (&optional _word)
    "Search word at point and display result with pos-tip."
    (interactive)
    (let ((word (or _word (swint-get-words-at-point))))
      (youdao-dictionary--pos-tip (youdao-dictionary--format-result word)))))
;; ===============youdao-dictionary==============
(provide 'setup_dict)
