;;; Mew
;; ========================Mew=========================
(use-package mew
  ;; Enabled at commands.
  :defer t
  :bind ("C-M-7" . mew)
  :config
  (autoload 'mew "mew" nil t)
  (autoload 'mew-send "mew" nil t)
  (autoload 'mew-user-agent-compose "mew" nil t)
  (when (boundp 'read-mail-command)
    (setq read-mail-command 'mew))
  (when (fboundp 'define-mail-user-agent)
    (define-mail-user-agent
      'mew-user-agent
      'mew-user-agent-compose
      'mew-draft-send-message
      'mew-draft-kill
      'mew-send-hook))
  (when (boundp 'mail-user-agent)
    (setq mail-user-agent 'mew-user-agent))
  (when (boundp 'utf-translate-cjk)
    (setq utf-translate-cjk t))
  (when (fboundp 'utf-translate-cjk-mode)
    (utf-translate-cjk-mode 1))
  (setq user-full-name "Guiqiang Wang")
  (setq user-mail-address "wgq_hit@126.com")
  (setq mew-refile-guess-alist '(("To:"
                                  ("wgq_713@163.com" . "+Mail/netease")
                                  ("278064399@qq.com" . "+Mail/qq")
                                  ("wguiqiang@hotmail.com" . "+Mail/hotmail"))
                                 ("Cc:"
                                  ("wgq_713@163.com" . "+Mail/netease")
                                  ("278064399@qq.com" . "+Mail/qq")
                                  ("wguiqiang@hotmail.com" . "+Mail/hotmail"))
                                 (nil . "+inbox")))
  ;; Gmail的引用格式。
  (setq mew-cite-fields '("Date:"  "From:"))
  (setq mew-cite-format "On %s %s wrote:\n\n")
  ;; 密码设置，win提示密码错误。
  (when is-lin (setq mew-use-master-passwd t))
  (setq mew-use-cached-passwd t)
  (setq mew-passwd-timer-unit 60)
  (setq mew-passwd-lifetime 24)
  ;; 编码设置。
  (setq mew-charset-m17n "utf-8")
  (setq mew-internal-utf-8p t)
  ;; Html设置。
  (use-package mew-w3m
    :load-path "site-lisp/mew-w3m/"
    :config
    (setq mew-use-w3m-minor-mode t)
    (add-hook 'mew-message-hook 'mew-w3m-minor-mode-setter)
    (define-key mew-summary-mode-map (kbd "M-s") nil)
    (define-key mew-summary-mode-map (kbd "T") 'mew-w3m-view-inline-image)
    ;; Press "T":Toggle the visibility of the images included its message only.
    ;; Press "C-uT":Display the all images included its Text/Html part.
    (setq mew-w3m-auto-insert-image t)
    (setq w3m-default-display-inline-images t)
    (setq mew-prog-html '(mew-mime-text/html-w3m nil nil))
    (setq mew-mime-multipart-alternative-list '("Text/Html" "Text/Plan" ".*")))
  ;; Biff设置(邮件提醒)。
  (setq mew-use-biff t)
  (setq mew-use-biff-bell t)
  (setq mew-biff-interval 5)
  (setq mew-pop-biff-interval 3)
  ;; 其他。
  ;; (setq mew-debug t)
  (setq mew-pop-delete nil)
  (setq mew-pop-size 0)
  (setq mew-use-unread-mark t)
  (setq toolbar-mail-reader 'Mew)
  (set-default 'mew-decode-quoted 't)
  (setq sml/active-background-color "#222222"))
;; ========================Mew=========================
(provide 'setup_mew)
