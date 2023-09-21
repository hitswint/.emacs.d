;;; mu4e
;; =======================mu4e=========================
(use-package mu4e
  :load-path "/usr/share/emacs/site-lisp/elpa-src/mu4e-1.8.14"
  :bind (("M-o m" . mu4e)
         ("M-o M" . mu4e-compose-new))
  :config
  (setq mu4e-change-filenames-when-moving t
        mu4e-get-mail-command "mbsync -a" ; offlineimap
        mu4e-confirm-quit nil
        mu4e-update-interval 600
        message-kill-buffer-on-exit t
        mu4e-attachment-dir "~/Downloads"
        mu4e-completing-read-function 'completing-read
        mu4e-maildir-shortcuts '(("/Default/Inbox" . ?d)
                                 ("/Netease/Inbox" . ?n)
                                 ("/SJZU/Inbox" . ?j)
                                 ("/Hotmail/Inbox" . ?h)
                                 ("/QQ/Inbox" . ?q))
        send-mail-function 'smtpmail-send-it
        message-send-mail-function 'smtpmail-send-it)
  (setq mu4e-context-policy 'pick-first
        mu4e-contexts
        `(,(make-mu4e-context
            :name "Default"
            :enter-func (lambda () (mu4e-message "Switch to the Default context"))
            :match-func (lambda (msg) (when msg (mu4e-message-contact-field-matches msg :to "wgq_hit@126.com")))
            :vars '((user-mail-address . "wgq_hit@126.com" )
                    (user-full-name . "guiq.wang" )
                    (mu4e-sent-folder . "/Default/已发送")
                    (mu4e-drafts-folder . "/Default/草稿箱")
                    (mu4e-trash-folder . "/Default/已删除")
                    (smtpmail-default-smtp-server . "smtp.126.com")
                    (smtpmail-smtp-server . "smtp.126.com")
                    (smtpmail-smtp-service . 25)))
          ,(make-mu4e-context
            :name "Netease"
            :enter-func (lambda () (mu4e-message "Switch to the Netease context"))
            :match-func (lambda (msg) (when msg (mu4e-message-contact-field-matches msg :to "wgq_713@163.com")))
            :vars '((user-mail-address . "wgq_713@163.com" )
                    (user-full-name . "guiq.wang" )
                    (mu4e-sent-folder . "/Netease/已发送")
                    (mu4e-drafts-folder . "/Netease/草稿箱")
                    (mu4e-trash-folder . "/Netease/已删除")
                    (smtpmail-default-smtp-server . "smtp.163.com")
                    (smtpmail-smtp-server . "smtp.163.com")
                    (smtpmail-smtp-service . 25)))
          ,(make-mu4e-context
            :name "SJZU"
            :enter-func (lambda () (mu4e-message "Switch to the SJZU context"))
            :match-func (lambda (msg) (when msg (mu4e-message-contact-field-matches msg :to "guiq.wang@sjzu.edu.cn")))
            :vars '((user-mail-address . "guiq.wang@sjzu.edu.cn" )
                    (user-full-name . "guiq.wang" )
                    (mu4e-sent-folder . "/SJZU/已发送")
                    (mu4e-drafts-folder . "/SJZU/草稿箱")
                    (mu4e-trash-folder . "/SJZU/已删除")
                    (smtpmail-default-smtp-server . "smtphz.qiye.163.com")
                    (smtpmail-smtp-server . "smtphz.qiye.163.com")
                    (smtpmail-smtp-service . 465)))
          ,(make-mu4e-context
            :name "Hotmail"
            :enter-func (lambda () (mu4e-message "Switch to the Hotmail context"))
            :match-func (lambda (msg) (when msg (mu4e-message-contact-field-matches msg :to "wguiqiang@hotmail.com")))
            :vars '((user-mail-address . "wguiqiang@hotmail.com" )
                    (user-full-name . "guiq.wang" )
                    (mu4e-sent-folder . "/Hotmail/Sent")
                    (mu4e-drafts-folder . "/Hotmail/Drafts")
                    (mu4e-trash-folder . "/Hotmail/Deleted")
                    (smtpmail-default-smtp-server . "smtp-mail.outlook.com")
                    (smtpmail-smtp-server . "smtp-mail.outlook.com")
                    (smtpmail-smtp-service . 25)))
          ,(make-mu4e-context
            :name "QQ"
            :enter-func (lambda () (mu4e-message "Switch to the QQ context"))
            :match-func (lambda (msg) (when msg (mu4e-message-contact-field-matches msg :to "278064399@qq.com")))
            :vars '((user-mail-address . "278064399@qq.com")
                    (user-full-name . "guiq.wang")
                    (mu4e-sent-folder . "/QQ/Sent Messages")
                    (mu4e-drafts-folder . "/QQ/Drafts")
                    (mu4e-trash-folder . "/QQ/Deleted Messages")
                    (smtpmail-default-smtp-server . "smtp.qq.com")
                    (smtpmail-smtp-server . "smtp.qq.com")
                    (smtpmail-smtp-service . 587))))))
;; =======================mu4e=========================
;;;; mu4e-alert
;; ====================mu4e-alert======================
(use-package mu4e-alert
  :after mu4e
  :config
  (setq mu4e-alert-interesting-mail-query
        (concat "flag:unread maildir:/Default/Inbox"
                " OR "
                "flag:unread maildir:/Netease/Inbox"
                " OR "
                "flag:unread maildir:/SJZU/Inbox"
                " OR "
                "flag:unread maildir:/Hotmail/Inbox"
                " OR "
                "flag:unread maildir:/QQ/Inbox"))
  ;; notifications or libnotify.
  (mu4e-alert-set-default-style 'libnotify)
  (mu4e-alert-enable-notifications)
  (mu4e-alert-enable-mode-line-display))
;; ====================mu4e-alert======================
;;;; mu4e-views
;; ====================mu4e-views======================
(use-package mu4e-views
  :after mu4e
  :bind (:map mu4e-headers-mode-map
              ("v" . mu4e-views-mu4e-view-as-nonblocked-html)
              ("c" . mu4e-views-mu4e-select-view-msg-method))
  :config
  ;; 默认html方法使用过滤器屏蔽外部内容，可临时查看(v)或选择html-nonblock(c)
  (bind-key "M-n" 'mu4e-views-cursor-msg-view-window-down mu4e-headers-mode-map)
  (bind-key "M-p" 'mu4e-views-cursor-msg-view-window-up mu4e-headers-mode-map)
  (bind-key "f" 'mu4e-views-toggle-auto-view-selected-message mu4e-headers-mode-map)
  (setq mu4e-views-completion-method 'ivy)
  (setq mu4e-views-default-view-method "gnus")
  (mu4e-views-mu4e-use-view-msg-method "gnus")
  (setq mu4e-views-next-previous-message-behaviour 'stick-to-current-window)
  (setq mu4e-views-auto-view-selected-message t))
;; ====================mu4e-views======================
;;; helm-mu
;; ======================helm-mu=======================
(use-package helm-mu
  :bind (("M-o M-m" . helm-mu)
         ("M-o M-M" . helm-mu-contacts)))
;; ======================helm-mu=======================
(provide 'setup_email)
