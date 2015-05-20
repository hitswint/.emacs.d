;; ====================stardict=====================
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
  (local-set-key (kbd "q") '(lambda ()
                              (interactive)
                              (kill-buffer)
                              (unless (null (cdr (window-list)))
                                (delete-window))
                              ))
  (run-hooks 'sdcv-mode-hook))
(defun kid-sdcv-to-buffer ()
  (interactive)
  (let ((word (if mark-active
                  (buffer-substring-no-properties (region-beginning) (region-end))
                (current-word nil t))))
    (setq word (read-string (format "Search the dictionary for (default %s): " word)
                            nil nil word))
    (set-buffer (get-buffer-create "*sdcv*"))
    (buffer-disable-undo)
    (erase-buffer)
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
               ))))))
    (goto-char (point-min))
    (insert (concat "*** Bing Dict" " (" word ")\n"))
    (indent-for-tab-command)
    (swint-bing-dict-brief word)
    ))
(defun yasdcv--output-cleaner:common ()
  ;; 从yasdcv借来的函数
  (goto-char (point-min))
  (while (re-search-forward "-->\\(.*\\)\n-->\\(.*\\)" nil t)
    (replace-match "*** \\1 (\\2)"))
  (goto-char (point-min))
  (while (re-search-forward "\n+" nil t)
    (replace-match "\n"))
  (goto-char (point-min))
  (kill-line 1))
;; ====================stardict=====================
;; ===================bing-dict=====================
(require 'bing-dict)
(defvar swint-bing-dict-result nil)
(defun swint-bing-dict-brief-cb (status keyword)
  (set-buffer-multibyte t)
  (bing-dict--delete-response-header)
  (condition-case nil
      (if (bing-dict--has-result-p)
          (if (bing-dict--definitions-exist-p)
              (let ((query-word (propertize keyword 'face 'font-lock-keyword-face))
                    (pronunciation (bing-dict--pronunciation))
                    (short-exps (mapconcat 'identity (bing-dict--definitions)
                                           (propertize " | "
                                                       'face
                                                       'font-lock-builtin-face))))
                (with-current-buffer "*sdcv*"
                  (goto-char (point-max))
                  (insert (concat pronunciation short-exps))))
            (with-current-buffer "*sdcv*"
              (goto-char (point-max))
              (insert (concat (propertize (bing-dict--machine-translation)
                                          'face
                                          'font-lock-doc-face)))))
        (with-current-buffer "*sdcv*"
          (goto-char (point-max))
          (insert "No results")))
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
(defun youdao-sample-sentences ()
  (interactive)
  (let ((word (if mark-active
                  (buffer-substring-no-properties (region-beginning) (region-end))
                (current-word nil t))))
    (setq word (read-string (format "Sample Sentences for (default %s): " word)
                            nil nil word))
    (browse-url
     (concat "http://dict.youdao.com/search?le=eng&q=lj%3A"
             word
             "&keyfrom=dict.top"))))
(global-set-key (kbd "C-M-@") 'youdao-sample-sentences)
;; ===================bing-dict=====================
(provide 'setup_stardict)
