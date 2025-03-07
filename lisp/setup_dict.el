;;; bing-dict
;; ==================bing-dict===================
(use-package bing-dict
  :commands (bing-dict-brief
             bing-dict-brief-cb-at-point)
  :init
  (smartrep-define-key global-map "M-s d"
    '(("b" . bing-dict-brief-cb-at-point)
      ("g" . google-translate-translate_chieng)
      ("y" . youdao-dictionary-to-tip)
      ("d" . baidu-translate-at-point)
      ("l" . lingva-translate-at-point)
      ("i" . insert-translated-name-insert)
      ("I" . insert-translated-name-replace)
      ("w" . w3m-youdao-sample-sentences)
      ("e" . ellama-translate-at-point)))
  :config
  (defun bing-dict-brief-cb-at-point (&optional _word sync-p)
    "Search word at point."
    (interactive)
    (let ((word (or _word (swint-get-words-at-point))))
      (bing-dict-brief word sync-p))))
;; ==================bing-dict===================
;;; google-translate
;; ===============google-translate===============
(use-package google-translate
  :commands (google-translate-translate
             google-translate-translate_chieng)
  :init
  (setq google-translate-translation-directions-alist
        '(("en" . "zh-CN") ("zh-CN" . "en")))
  :config
  (setq google-translate-backend-method 'wget)
  (setq google-translate-output-destination 'echo-area)
  (defun google-translate-current-buffer-output-translation/override (gtos)
    "Output translation to current buffer."
    (google-translate-buffer-insert-translation gtos)
    (message "Translated text was added to current buffer."))
  (advice-add 'google-translate-current-buffer-output-translation :override
              #'google-translate-current-buffer-output-translation/override)
  (defun google-translate-translate_chieng (&optional _word _dest)
    "Search WORD simple translate result."
    (interactive)
    (let ((word (or _word (swint-get-words-at-point)))
          (dest (or _dest 'popup)))
      ;; \\cc匹配中文字符，包括全角标点，\\cC匹配汉字
      (if (string-match-p "\\cC" word)
          (google-translate-translate "zh-CN" "en" word dest)
        (google-translate-translate "en" "zh-CN" word dest)))))
;; ===============google-translate===============
;;; youdao-dictionary
;; ===============youdao-dictionary==============
(use-package youdao-dictionary
  :commands (youdao-dictionary--format-result
             youdao-dictionary--request
             youdao-dictionary-to-tip)
  :config
  (defvar youdao-dictionary-config "~/.ydcv")
  (setq youdao-dictionary-secret-key (dict-load-config youdao-dictionary-config "YDAPPSEC"))
  (setq youdao-dictionary-app-key (dict-load-config youdao-dictionary-config "YDAPPID"))
  (defun youdao-dictionary-to-tip (&optional _word)
    (interactive)
    (let* ((word (or _word (swint-get-words-at-point)))
           (ydcv-result (youdao-dictionary--format-result
                         (youdao-dictionary--request word))))
      (swint-display-dict "*ydcv*" ydcv-result))))
;; ===============youdao-dictionary==============
;;; baidu-translate
;; ================baidu-translate===============
(use-package baidu-translate
  :commands baidu-translate-at-point
  :config
  (use-package unicode-escape :after baidu-translate)
  (defvar baidu-translate-config "~/.bdcv")
  (setq baidu-translate-security (dict-load-config baidu-translate-config "BDAPPSEC"))
  (setq baidu-translate-appid (dict-load-config baidu-translate-config "BDAPPID"))
  (defun baidu-translate-at-point (&optional _word)
    (interactive)
    (let ((word (or _word (swint-get-words-at-point))))
      (if (string-match-p "\\cC" word)
          (baidu-translate-string word "auto" "en")
        (baidu-translate-string word "auto" "zh")))))
;; ================baidu-translate===============
;;; lingva
;; =====================lingva===================
(use-package lingva
  :commands lingva-translate-at-point
  :config
  (setq lingva-instance "https://translate.plausibility.cloud")  ;默认https://lingva.ml
  (defun lingva-translate-at-point (&optional _word)
    (interactive)
    (let* ((word (or _word (swint-get-words-at-point)))
           (chinese-p (string-match-p "\\cC" word))
           (lingva-source (if chinese-p "zh" "en"))
           (lingva-target (if chinese-p "en" "zh")))
      (lingva-translate nil word))))
;; =====================lingva===================
(provide 'setup_dict)
