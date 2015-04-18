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
    (let ((process (start-process-shell-command "sdcv" "*sdcv*" "sdcv" "-n" word)))
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
               ))
           (goto-char (point-min))))))))
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
(provide 'setup_stardict)
