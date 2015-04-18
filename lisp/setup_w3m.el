;; ===================w3m=====================
(require 'w3m)
(setq w3m-use-form t)
(setq w3m-tab-width 8)
(setq w3m-use-cookies t)
(setq w3m-use-toolbar t)
(setq w3m-use-mule-ucs t)
(setq w3m-fill-column 120)
(setq w3m-default-display-inline-image t)
(setq w3m-default-toggle-inline-images t)
(setq w3m-home-page "about:blank")
(setq browse-url-browser-function 'w3m-browse-url)
(setq w3m-view-this-url-new-session-in-background t)
(setq w3m-command-arguments '("-cookie" "-F"))
(defun w3m-open-site-current-session (site)
  "Open site in current session with ‘http://’ appended"
  (interactive
   (list (read-string "Enter website address(default: w3m-home):" nil nil "www.baidu.com" nil )))
  (w3m-goto-url
   (concat "http://" site)))
(defun w3m-open-site-new-session (site)
  "Open site in new w3m session with ‘http://’ appended"
  (interactive
   (list (read-string "Enter website address(default: w3m-home):" nil nil "www.baidu.com" nil )))
  (w3m-goto-url-new-session
   (concat "http://" site)))
(defun w3m-new-tab ()
  (interactive)
  (w3m-copy-buffer nil nil nil t))
;;为了在w3m下使用meta n快捷键，必须先取消w3m下原有的设定
(add-hook 'w3m-mode-hook
          '(lambda ()
             (define-key w3m-mode-map [(\h)] 'w3m-previous-buffer)
             (define-key w3m-mode-map [(\l)] 'w3m-next-buffer)
             (define-key w3m-mode-map [(meta \s)] nil)
             (define-key w3m-mode-map [(meta \n)] nil)
             (define-key w3m-mode-map [(\q)] 'w3m-delete-buffer)
             (define-key w3m-mode-map (kbd "C-c o") 'w3m-open-site-current-session)
             (define-key w3m-mode-map (kbd "C-c t") 'w3m-open-site-new-session)
             (define-key w3m-mode-map (kbd "C-o b") '(lambda () (interactive)
                                                       (w3m-new-tab)
                                                       (w3m-browse-url "cn.bing.com")))
             (define-key w3m-mode-map (kbd "C-o g") '(lambda () (interactive)
                                                       (w3m-new-tab)
                                                       (w3m-browse-url "www.google.com")))
             (define-key w3m-mode-map (kbd "'") 'helm-w3m-bookmarks)))
;; ===================w3m=====================
(provide 'setup_w3m)
