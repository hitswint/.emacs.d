;; ==================stardict====================
(define-derived-mode sdcv-mode org-mode
  ;; "Major mode for sdcv."
  (interactive)
  (setq major-mode 'sdcv-mode)
  (setq mode-name "sdcv")
  (local-set-key (kbd "n") 'next-line)
  (local-set-key (kbd "j") 'next-line)
  (local-set-key (kbd "p") 'previous-line)
  (local-set-key (kbd "k") 'previous-line)
  (local-set-key (kbd "SPC") 'scroll-up)
  (local-set-key (kbd "DEL") 'scroll-down)
  (local-set-key (kbd "d") 'kid-sdcv-to-buffer)
  (local-set-key (kbd "q") 'sdcv-quit)
  (run-hooks 'sdcv-mode-hook))
(defun sdcv-quit ()
  "Whether to close sdcv window based on current window number."
  (interactive)
  (if (> current-window-number 1)
      (kill-buffer)
    (progn (kill-buffer)
           (unless (null (cdr (window-list)))
             (delete-window)))))
(defvar current-window-number nil)
(defun kid-sdcv-to-buffer ()
  (interactive)
  (let ((word (if mark-active
                  (buffer-substring-no-properties (region-beginning) (region-end))
                (current-word nil t))))
    (setq current-window-number (length (window-list)))
    (setq word (read-string (format "Search the dictionary for (default %s): " word)
                            nil nil word))
    (when is-win
      (w32-shell-execute "open" "sdcv" (concat "--data-dir c:/Users/swint/.stardict " word " stardict")))
    (set-buffer (get-buffer-create "*sdcv*"))
    (buffer-disable-undo)
    (erase-buffer)
    (cond
     (is-lin
      (let ((process (start-process-shell-command "sdcv" "*sdcv*" "sdcv" "-n --data-dir ~/.stardict/dict" word)))
        (set-process-sentinel
         process
         (lambda (process signal)
           (when (memq (process-status process) '(exit signal))
             (unless (string= (buffer-name) "*sdcv*")
               (setq kid-sdcv-window-configuration (current-window-configuration))
               ;; (split-horizontally-not-vertically) ;改变窗口分割方式，一个窗口时，横向分割；多个窗口时，纵向分割。
               ;; 但是同时有其他程序和emacs时不适用，注释掉，并删除这个函数。
               (switch-to-buffer-other-window "*sdcv*")
               (when (featurep 'org)
                 (yasdcv--output-cleaner:common)
                 (sdcv-mode)
                 (show-all)               ;显示所有outline
                 (indent-region (point-min) (point-max))
                 (while (re-search-forward "*** Collins Cobuild English Dictionary " nil t)
                   (hide-entry))          ;隐藏柯林斯辞典选项
                 )))))))
     (is-win
      ;; 在cygwin的shell中启动sdcv只能显示数目，无法显示结果。
      ;; (let ((process
      ;;        (start-process-shell-command
      ;;         "sdcv" "*sdcv*" "sdcv" (concat "--data-dir c:/Users/swint/.stardict " word))
      ;;        ))
      ;;   (set-process-sentinel
      ;;    process
      ;;    (lambda (process signal)
      ;;      (when (memq (process-status process) '(exit signal))
      ;;        (unless (string= (buffer-name) "*sdcv*")
      ;;          (setq kid-sdcv-window-configuration (current-window-configuration))
      ;;          (switch-to-buffer-other-window "*sdcv*")
      ;;          (when (featurep 'org)
      ;;            (yasdcv--output-cleaner:common)
      ;;            (sdcv-mode)
      ;;            (show-all)               ;显示所有outline
      ;;            (indent-region (point-min) (point-max))
      ;;            (while (re-search-forward "*** Collins Cobuild English Dictionary " nil t)
      ;;              (hide-entry))          ;隐藏柯林斯辞典选项
      ;;            ))))))
      (setq kid-sdcv-window-configuration (current-window-configuration))
      (switch-to-buffer-other-window "*sdcv*")
      (when (featurep 'org)
        (yasdcv--output-cleaner:common)
        (sdcv-mode)
        (show-all)               ;显示所有outline
        (indent-region (point-min) (point-max))
        (while (re-search-forward "*** Collins Cobuild English Dictionary " nil t)
          (hide-entry))          ;隐藏柯林斯辞典选项
        )))
    (goto-char (point-min))
    (insert (concat "*** Bing Dict" " (" word ")\n"))
    (indent-for-tab-command)
    (swint-bing-dict-brief word)))
(defun yasdcv--output-cleaner:common ()
  ;; 从yasdcv借来的函数
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
(require 'bing-dict)
(defvar swint-bing-dict-result nil)
(defun swint-bing-dict-brief-cb (status keyword)
  (set-buffer-multibyte t)
  (bing-dict--delete-response-header)
  (condition-case nil
      (if (buffer-live-p (get-buffer "*sdcv*"))
          (if (bing-dict--has-machine-translation-p)
              (with-current-buffer "*sdcv*"
                (goto-char (point-max))
                (insert (concat (propertize (bing-dict--machine-translation)
                                            'face
                                            'font-lock-doc-face))))
            (let ((query-word (propertize keyword 'face 'font-lock-keyword-face))
                  (pronunciation (bing-dict--pronunciation))
                  (short-exps (mapconcat 'identity (bing-dict--definitions)
                                         (propertize " | "
                                                     'face
                                                     'font-lock-builtin-face))))
              (with-current-buffer "*sdcv*"
                (goto-char (point-max))
                (if short-exps
                    (insert (concat pronunciation short-exps))
                  (insert "No results")))))
        (bing-dict-brief keyword))
    (error
     (with-current-buffer "*sdcv*"
       (goto-char (point-max))
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
                  t
                  t)))
;; ==================bing-dict===================
;; ====================youdao====================
(defun youdao-sample-sentences ()
  (interactive)
  (let ((word (if mark-active
                  (buffer-substring-no-properties (region-beginning) (region-end))
                (current-word nil t))))
    (setq word (read-string (format "Sample Sentences for (default %s): " word)
                            nil nil word))
    (browse-url
     (concat "http://dict.youdao.com/search?le=eng&q=lj%3A"
             (cond
              (is-lin word)
              ;; 解决w3m无法解析网址的问题
              (is-win (w3m-url-encode-string word 'utf-8)))
             "&keyfrom=dict.top"))))
(global-set-key (kbd "C-M-@") 'youdao-sample-sentences)
;; ====================youdao====================
;; ===============google-translate===============
(use-package google-translate
  :defer t
  :bind ("M-@" . google-translate-smooth-translate)
  :init
  (setq google-translate-base-url
        "http://translate.google.cn/translate_a/single")
  (setq google-translate-listen-url
        "http://translate.google.cn/translate_tts")
  (setq google-translate-translation-directions-alist
        '(("en" . "zh-CN") ("zh-CN" . "en")))
  :config
  (use-package google-translate-smooth-ui))
;; ===============google-translate===============
(provide 'setup_dict)
