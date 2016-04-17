;;; Mew
;; ========================Mew=========================
(use-package mew
  ;; Enabled at commands.
  :defer t
  :bind ("C-M-7" . mew)
  :config
  (autoload 'mew "mew" nil t)
  (autoload 'mew-send "mew" nil t)
  ;; Optional setup (Read Mail menu for Emacs 21):
  (if (boundp 'read-mail-command)
      (setq read-mail-command 'mew))
  ;; Optional setup (e.g. C-xm for sending a message):
  (autoload 'mew-user-agent-compose "mew" nil t)
  (if (boundp 'mail-user-agent)
      (setq mail-user-agent 'mew-user-agent))
  (if (fboundp 'define-mail-user-agent)
      (define-mail-user-agent
        'mew-user-agent
        'mew-user-agent-compose
        'mew-draft-send-message
        'mew-draft-kill
        'mew-send-hook))
  (when (boundp 'utf-translate-cjk)
    (setq utf-translate-cjk t)
    (custom-set-variables
     '(utf-translate-cjk t)))
  (if (fboundp 'utf-translate-cjk-mode)
      (utf-translate-cjk-mode 1))
  (setq user-full-name "Guiqiang Wang")
  (setq user-mail-address "wguiqiang@hotmail.com")
  ;; Gmail的引用格式
  (setq mew-cite-fields '("Date:"  "From:"))
  (setq mew-cite-format "On %s %s wrote:\n\n")
  ;; 密码设置
  (when is-lin
    (setq mew-use-master-passwd t))     ;使用主密码，win提示主密码错误
  (setq mew-use-cached-passwd t)
  (setq mew-passwd-timer-unit 60)
  (setq mew-passwd-lifetime 24)         ;timer-unit x 24 = 24 hours
  ;; (setq mew-passwd-alist '(("wgq_713@163.com" "xxx" 0)
  ;;                          ("wgq_hit@126.com" "xxx" 0)))
  ;; 编码设置
  (setq mew-charset-m17n "utf-8")
  (setq mew-internal-utf-8p t)
  ;; html设置
  (use-package mew-w3m
    :load-path "site-lisp/mew-w3m/")    ;需要w3m支持，看html邮件
  (setq mew-use-w3m-minor-mode t)
  (add-hook 'mew-message-hook 'mew-w3m-minor-mode-setter)
  (define-key mew-summary-mode-map "T" 'mew-w3m-view-inline-image)
  ;; Press "T":Toggle the visibility of the images included its message only.
  ;; Press "C-uT":Display the all images included its Text/Html part.
  (setq mew-w3m-auto-insert-image t)
  (setq w3m-default-display-inline-images t)
  (setq mew-prog-html '(mew-mime-text/html-w3m nil nil))
  (setq mew-mime-multipart-alternative-list '("Text/Html" "Text/Plan" ".*"))
  ;; biff设置(邮件提醒)
  (setq mew-use-biff t)
  (setq mew-use-biff-bell t)
  (setq mew-biff-interval 5) ;这个值一定要小于下面的timer-unit和lifetime值，这个可以使用
  (setq mew-pop-biff-interval 3)
  ;; 其他
  (setq mew-pop-delete nil)
  (setq mew-pop-size 0)
  (setq mew-use-unread-mark t)
  (setq toolbar-mail-reader 'Mew)
  (set-default 'mew-decode-quoted 't)
  (setq sml/active-background-color "#222222")
  ;; (setq mew-debug t)
  )
;; ========================Mew=========================
(provide 'setup_mew)
