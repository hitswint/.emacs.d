;;; bing-dict
;; ==================bing-dict===================
(def-package! bing-dict
  :commands (bing-dict-brief
             bing-dict-brief-cb-at-point)
  :init
  (smartrep-define-key global-map "M-s d"
    '(("b" . bing-dict-brief-cb-at-point)
      ("g" . google-translate-to-tip)
      ("y" . youdao-dictionary-to-tip)))
  :config
  (defun bing-dict-brief-cb/after (&rest args)
    "Output translation to pos-tip or *online* buffer."
    (if (get-buffer "*online*")
        (with-current-buffer "*online*"
          (goto-char (point-min))
          (insert (concat "*** Bing Dict\n"))
          (insert (concat (current-message) "\n")))
      (pos-tip-show (current-message) nil nil nil 0)))
  (advice-add 'bing-dict-brief-cb :after #'bing-dict-brief-cb/after)
  (defun bing-dict-brief-cb-at-point (&optional _word)
    "Search word at point."
    (interactive)
    (let ((word (or _word (swint-get-words-at-point))))
      (bing-dict-brief word))))
;; ==================bing-dict===================
;;; google-translate
;; ===============google-translate===============
(def-package! google-translate
  :commands (google-translate-translate
             google-translate-to-tip)
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
  (defun google-translate-current-buffer-output-translation/override (gtos)
    "Output translation to current buffer."
    (google-translate-buffer-insert-translation gtos))
  (advice-add 'google-translate-current-buffer-output-translation :override
              #'google-translate-current-buffer-output-translation/override)
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
(def-package! youdao-dictionary
  :commands (youdao-dictionary--format-result
             youdao-dictionary-to-tip)
  :config
  (defun youdao-dictionary-to-tip (&optional _word)
    "Search word at point and display result with pos-tip."
    (interactive)
    (let ((word (or _word (swint-get-words-at-point))))
      (youdao-dictionary--pos-tip (youdao-dictionary--format-result word)))))
;; ===============youdao-dictionary==============
(provide 'setup_dict)
