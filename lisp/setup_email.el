;;; Mew
;; ========================Mew=========================
(use-package mew
  ;; Enabled at commands.
  :if is-win
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
  ;; +/$/%/-对应local/pop/imap/nntp。
  (setq mew-refile-guess-alist '(("To:"
                                  ("wgq_713@163.com" . "+inbox_netease")
                                  ("wguiqiang@hotmail.com" . "+inbox_hotmail")
                                  ("278064399@qq.com" . "+inbox_qq"))
                                 ("Cc:"
                                  ("wgq_713@163.com" . "+inbox_netease")
                                  ("wguiqiang@hotmail.com" . "+inbox_hotmail")
                                  ("278064399@qq.com" . "+inbox_qq"))
                                 (nil . "+inbox")))
  ;; Gmail的引用格式。
  (setq mew-cite-fields '("Date:"  "From:"))
  (setq mew-cite-format "On %s %s wrote:\n\n")
  ;; 使用主密码，win会提示主密码错误。
  ;; (setq mew-use-master-passwd t)
  (setq mew-use-cached-passwd t)
  (setq mew-passwd-timer-unit 60)
  (setq mew-passwd-lifetime 24)
  (setq mew-prog-ssl-arg "CAfile=/usr/ssl/certs/ca-bundle.crt\n")
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
;;; mu4e
;; =======================mu4e=========================
(use-package mu4e
  ;; Enabled at idle.
  :if is-lin
  :load-path "/usr/share/emacs/site-lisp/mu4e"
  :defer t
  :bind (("C-M-7" . mu4e)
         ("C-x M" . mu4e-compose-new))
  :config
  (use-package mu4e-alert
    :config
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
        mu4e-get-mail-command "mbsync -a; offlineimap"
        mu4e-confirm-quit nil
        mu4e-update-interval 600
        message-kill-buffer-on-exit t
        mu4e-attachment-dir "~/Downloads"
        mu4e-completing-read-function 'completing-read
        mu4e-maildir-shortcuts '(("/Default/INBOX" . ?d)
                                 ("/Netease/INBOX" . ?n)
                                 ("/Hotmail/Inbox" . ?h)
                                 ("/QQ/INBOX" . ?q))
        send-mail-function 'smtpmail-send-it
        message-send-mail-function 'smtpmail-send-it)
  (setq mu4e-contexts
        `(,(make-mu4e-context
            :name "Default"
            :enter-func (lambda () (mu4e-message "Switch to the Default context"))
            :match-func (lambda (msg)
                          (when msg
                            (mu4e-message-contact-field-matches msg
                                                                :to "wgq_hit@126.com")))
            :vars '((user-mail-address . "wgq_hit@126.com" )
                    (user-full-name . "Guiqiang Wang" )
                    (mu4e-sent-folder . "/Default/已发送")
                    (mu4e-drafts-folder . "/Default/草稿箱")
                    (mu4e-trash-folder . "/Default/已删除")
                    (smtpmail-default-smtp-server . "smtp.126.com")
                    (smtpmail-smtp-server . "smtp.126.com")
                    (smtpmail-smtp-service . 25)))
          ,(make-mu4e-context
            :name "Netease"
            :enter-func (lambda () (mu4e-message "Switch to the Netease context"))
            :match-func (lambda (msg)
                          (when msg
                            (mu4e-message-contact-field-matches msg
                                                                :to "wgq_713@163.com")))
            :vars '((user-mail-address . "wgq_713@163.com" )
                    (user-full-name . "Guiqiang Wang" )
                    (mu4e-sent-folder . "/Netease/已发送")
                    (mu4e-drafts-folder . "/Netease/草稿箱")
                    (mu4e-trash-folder . "/Netease/已删除")
                    (smtpmail-default-smtp-server . "smtp.163.com")
                    (smtpmail-smtp-server . "smtp.163.com")
                    (smtpmail-smtp-service . 25)))
          ,(make-mu4e-context
            :name "Hotmail"
            :enter-func (lambda () (mu4e-message "Switch to the Hotmail context"))
            :match-func (lambda (msg)
                          (when msg
                            (mu4e-message-contact-field-matches msg
                                                                :to "wguiqiang@hotmail.com")))
            :vars '((user-mail-address . "wguiqiang@hotmail.com" )
                    (user-full-name . "Guiqiang Wang" )
                    (mu4e-sent-folder . "/Hotmail/Sent")
                    (mu4e-drafts-folder . "/Hotmail/Drafts")
                    (mu4e-trash-folder . "/Hotmail/Deleted")
                    (smtpmail-default-smtp-server . "smtp-mail.outlook.com")
                    (smtpmail-smtp-server . "smtp-mail.outlook.com")
                    (smtpmail-smtp-service . 25)))
          ,(make-mu4e-context
            :name "QQ"
            :enter-func (lambda () (mu4e-message "Switch to the QQ context"))
            :match-func (lambda (msg)
                          (when msg
                            (mu4e-message-contact-field-matches msg
                                                                :to "278064399@qq.com")))
            :vars '((user-mail-address . "278064399@qq.com")
                    (user-full-name . "Guiqiang Wang")
                    (mu4e-sent-folder . "/QQ/Sent Messages")
                    (mu4e-drafts-folder . "/QQ/Drafts")
                    (mu4e-trash-folder . "/QQ/Deleted Messages")
                    (smtpmail-default-smtp-server . "smtp.qq.com")
                    (smtpmail-smtp-server . "smtp.qq.com")
                    (smtpmail-smtp-service . 587)))))
  (defun get-auth-user (host)
    (require 'netrc)
    (let* ((netrc (netrc-parse (expand-file-name "~/.authinfo.gpg")))
           (hostentry (netrc-machine netrc host)))
      (when hostentry (netrc-get hostentry "login"))))
  (defun get-auth-pass (host)
    (require 'netrc)
    (let* ((netrc (netrc-parse (expand-file-name "~/.authinfo.gpg")))
           (hostentry (netrc-machine netrc host)))
      (when hostentry (netrc-get hostentry "password")))))
;; =======================mu4e=========================
;;; helm-mu
;; ======================helm-mu=======================
(use-package helm-mu
  ;; Enabled at commands.
  :if is-lin
  :defer t
  :bind (("M-s m" . helm-mu)
         ("M-s M" . helm-mu-contacts)))
;; ======================helm-mu=======================
(provide 'setup_email)
