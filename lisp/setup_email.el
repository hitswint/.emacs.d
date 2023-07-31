;;; mu4e
;; =======================mu4e=========================
(use-package mu4e
  :load-path "/usr/share/emacs/site-lisp/mu4e"
  :bind (("M-o m" . mu4e)
         ("M-o M" . mu4e-compose-new))
  :config
  (use-package mu4e-alert
    :config
    (setq mu4e-alert-interesting-mail-query
          (concat "flag:unread maildir:/Default/INBOX"
                  " OR "
                  "flag:unread maildir:/Netease/INBOX"
                  " OR "
                  "flag:unread maildir:/SJZU/INBOX"
                  " OR "
                  "flag:unread maildir:/Hotmail/Inbox"
                  " OR "
                  "flag:unread maildir:/QQ/INBOX"))
    ;; notifications or libnotify.
    (mu4e-alert-set-default-style 'libnotify)
    (mu4e-alert-enable-notifications)
    (mu4e-alert-enable-mode-line-display))
  (use-package org-mu4e
    :config
    (setq org-mu4e-convert-to-html t)
    (define-key mu4e-view-mode-map (kbd "C-c o") 'mu4e-org-mode)
    (define-key mu4e-compose-mode-map (kbd "C-c o") 'org-mu4e-compose-org-mode))
  (setq mu4e-change-filenames-when-moving t
        mu4e-view-prefer-html t
        mu4e-view-show-images t
        mu4e-get-mail-command "mbsync -a" ; offlineimap
        mu4e-confirm-quit nil
        mu4e-update-interval 600
        message-kill-buffer-on-exit t
        mu4e-attachment-dir "~/Downloads"
        mu4e-completing-read-function 'completing-read
        mu4e-maildir-shortcuts '(("/Default/INBOX" . ?d)
                                 ("/Netease/INBOX" . ?n)
                                 ("/SJZU/INBOX" . ?j)
                                 ("/Hotmail/Inbox" . ?h)
                                 ("/QQ/INBOX" . ?q))
        send-mail-function 'smtpmail-send-it
        message-send-mail-function 'smtpmail-send-it)
  (setq mu4e-contexts
        `(,(make-mu4e-context
            :name "Default"
            :enter-func (lambda () (mu4e-message "Switch to the Default context"))
            :match-func (lambda (msg) (when msg (mu4e-message-contact-field-matches msg :to "wgq_hit@126.com")))
            :vars '((user-mail-address . "wgq_hit@126.com" )
                    (user-full-name . "guiq.wang" )
                    (mu4e-sent-folder . "/Default/&XfJT0ZAB-")
                    (mu4e-drafts-folder . "/Default/&g0l6P3ux-")
                    (mu4e-trash-folder . "/Default/&XfJSIJZk-")
                    (smtpmail-default-smtp-server . "smtp.126.com")
                    (smtpmail-smtp-server . "smtp.126.com")
                    (smtpmail-smtp-service . 25)))
          ,(make-mu4e-context
            :name "Netease"
            :enter-func (lambda () (mu4e-message "Switch to the Netease context"))
            :match-func (lambda (msg) (when msg (mu4e-message-contact-field-matches msg :to "wgq_713@163.com")))
            :vars '((user-mail-address . "wgq_713@163.com" )
                    (user-full-name . "guiq.wang" )
                    (mu4e-sent-folder . "/Netease/&XfJT0ZAB-")
                    (mu4e-drafts-folder . "/Netease/&g0l6P3ux-")
                    (mu4e-trash-folder . "/Netease/&XfJSIJZk-")
                    (smtpmail-default-smtp-server . "smtp.163.com")
                    (smtpmail-smtp-server . "smtp.163.com")
                    (smtpmail-smtp-service . 25)))
          ,(make-mu4e-context
            :name "SJZU"
            :enter-func (lambda () (mu4e-message "Switch to the SJZU context"))
            :match-func (lambda (msg) (when msg (mu4e-message-contact-field-matches msg :to "guiq.wang@sjzu.edu.cn")))
            :vars '((user-mail-address . "guiq.wang@sjzu.edu.cn" )
                    (user-full-name . "guiq.wang" )
                    (mu4e-sent-folder . "/SJZU/&XfJT0ZAB-")
                    (mu4e-drafts-folder . "/SJZU/&g0l6P3ux-")
                    (mu4e-trash-folder . "/SJZU/&XfJSIJZk-")
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
;;; helm-mu
;; ======================helm-mu=======================
(use-package helm-mu
  :bind (("M-o M-m" . helm-mu)
         ("M-o M-M" . helm-mu-contacts)))
;; ======================helm-mu=======================
(provide 'setup_email)
