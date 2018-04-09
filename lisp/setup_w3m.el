;;; w3m
;; ===================w3m=====================
(def-package! w3m
  :bind (("C-M-5" . w3m)
         ("C-M-%" . w3m-youdao-sample-sentences))
  :config
  ;; Use w3m to display youdao sample sentences.
  (defun w3m-youdao-sample-sentences (&optional _word)
    (interactive)
    (let ((word (or _word (swint-get-words-at-point))))
      (browse-url
       (concat "http://dict.youdao.com/search?le=eng&q=lj%3A"
               word "&keyfrom=dict.top"))))
  (setq w3m-use-form t
        w3m-tab-width 8
        w3m-use-cookies t
        w3m-use-toolbar t
        w3m-use-mule-ucs t
        w3m-fill-column 120
        w3m-default-display-inline-image t
        w3m-default-toggle-inline-images t
        w3m-home-page "about:blank"
        browse-url-browser-function 'w3m-browse-url
        w3m-view-this-url-new-session-in-background t
        w3m-command-arguments '("-cookie" "-F"))
  (set-face-attribute 'w3m-bold nil :foreground "red" :weight 'bold)
  (defun w3m-open-site-current-session (site)
    "Open site in current session with‘http://’appended."
    (interactive
     (list (read-string "Enter website address(default: w3m-home):" nil nil "www.baidu.com" nil )))
    (w3m-goto-url
     (concat "http://" site)))
  (defun w3m-open-site-new-session (site)
    "Open site in new w3m session with‘http://’appended."
    (interactive
     (list (read-string "Enter website address(default: w3m-home):" nil nil "www.baidu.com" nil )))
    (w3m-goto-url-new-session
     (concat "http://" site)))
  (defun w3m-new-tab ()
    (interactive)
    (w3m-copy-buffer nil nil nil t))
  (add-hook 'w3m-mode-hook
            '(lambda ()
               (define-key w3m-mode-map [(meta \s)] nil)
               (define-key w3m-mode-map [(meta \n)] nil)
               (define-key w3m-mode-map [(\p)] 'w3m-previous-buffer)
               (define-key w3m-mode-map [(\n)] 'w3m-next-buffer)
               (define-key w3m-mode-map [(\q)] 'w3m-delete-buffer)
               (define-key w3m-mode-map [(\Q)] '(lambda () (interactive) (w3m-quit 1)))
               (define-key w3m-mode-map (kbd "o") 'w3m-open-site-current-session)
               (define-key w3m-mode-map (kbd "O") 'w3m-open-site-new-session)
               (define-key w3m-mode-map (kbd "C-o b") '(lambda () (interactive)
                                                         (w3m-new-tab)
                                                         (w3m-browse-url "cn.bing.com")))
               (define-key w3m-mode-map (kbd "C-o g") '(lambda () (interactive)
                                                         (w3m-new-tab)
                                                         (w3m-browse-url "www.google.com"))))))
;; ===================w3m=====================
;;; helm-firefox
;; ===============helm-firefox================
(def-package! helm-firefox
  :commands helm-firefox-bookmarks
  :init
  (add-hook 'w3m-mode-hook (lambda ()
                             (bind-key "'" 'helm-firefox-bookmarks w3m-mode-map))))
;; ===============helm-firefox================
(provide 'setup_w3m)
