;;; w3m
;; ===================w3m=====================
(use-package w3m
  :commands w3m-youdao-sample-sentences
  :bind ("M-o w" . w3m)
  :config
  (use-package w3m-search
    :config
    (add-to-list 'w3m-search-engine-alist
                 '("bing" "https://www.bing.com/search?q=%s" utf-8)))
  (defun w3m-youdao-sample-sentences (&optional _word)
    (interactive)
    (let ((word (or _word (swint-get-words-at-point))))
      (w3m-browse-url
       (concat "http://dict.youdao.com/search?le=eng&q=lj%3A"
               word "&keyfrom=dict.top"))))
  (setq w3m-use-form t
        w3m-tab-width 8
        w3m-use-cookies t
        w3m-use-toolbar t
        w3m-fill-column 120
        w3m-default-display-inline-images nil
        w3m-toggle-inline-images-permanently nil
        w3m-home-page "about:blank"
        ;; browse-url-browser-function 'w3m-browse-url
        w3m-new-session-in-background nil
        w3m-confirm-leaving-secure-page nil
        w3m-command-arguments '("-cookie" "-F")
        w3m-search-default-engine "bing")
  (set-face-attribute 'w3m-bold nil :foreground "red" :weight 'bold)
  (defun w3m-open-site-current-session (site)
    "Open site in current session with‘http://’appended."
    (interactive
     (list (read-string "Enter website address(default: w3m-home):" nil nil "www.bing.com" nil)))
    (w3m-goto-url
     (concat "http://" site)))
  (defun w3m-open-site-new-session (site)
    "Open site in new w3m session with‘http://’appended."
    (interactive
     (list (read-string "Enter website address(default: w3m-home):" nil nil "www.bing.com" nil)))
    (w3m-goto-url-new-session
     (concat "http://" site)))
  (defun w3m-new-tab ()
    (interactive)
    (w3m-copy-buffer nil nil nil t))
  (define-key w3m-mode-map [(meta \s)] nil)
  (define-key w3m-mode-map [(meta \n)] nil)
  (define-key w3m-mode-map [(\p)] 'w3m-previous-buffer)
  (define-key w3m-mode-map [(\n)] 'w3m-next-buffer)
  (define-key w3m-mode-map [(\q)] 'w3m-delete-buffer)
  (define-key w3m-mode-map [(\Q)] #'(lambda () (interactive) (w3m-quit 1)))
  (define-key w3m-mode-map (kbd "o") 'w3m-open-site-current-session)
  (define-key w3m-mode-map (kbd "O") 'w3m-open-site-new-session)
  (define-key w3m-mode-map (kbd "` b") #'(lambda () (interactive) (w3m-browse-url "https://www.bing.com/" t)))
  (define-key w3m-mode-map (kbd "` d") #'(lambda () (interactive) (w3m-browse-url "http://www.baidu.com/" t)))
  (define-key w3m-mode-map (kbd "` g") #'(lambda () (interactive) (w3m-browse-url "https://www.google.com/" t))))
;; ===================w3m=====================
;;; eww
;; ===================eww=====================
(use-package eww
  :bind ("M-o W" . eww)
  :config
  (setq eww-search-prefix "https://www.bing.com/search?q=")
  (define-key eww-mode-map [(meta \p)] nil)
  (define-key eww-mode-map [(meta \n)] nil)
  (define-key eww-mode-map (kbd "P") 'eww-previous-bookmark)
  (define-key eww-mode-map (kbd "N") 'eww-next-bookmark))
;; ===================eww=====================
;;; helm-firefox
;; ===============helm-firefox================
(use-package helm-firefox
  :commands (helm-firefox-bookmarks
             helm-firefox-history)
  :init
  (add-hook 'w3m-mode-hook (lambda ()
                             (bind-key "'" 'helm-firefox-bookmarks w3m-mode-map)
                             (bind-key "\"" 'helm-firefox-history w3m-mode-map)))
  :config
  (defvar helm-firefox-history-alist nil)
  (defvar helm-source-firefox-history
    (helm-build-sync-source "Firefox History"
      :init (lambda ()
              (setq helm-firefox-history-alist
                    (split-string (shell-command-to-string
                                   ;; 直接读取places.sqlite会导致Error: database is locked，将其拷贝到/tmp下读取
                                   (format "cp %s /tmp/; sqlite3 -separator '||' /tmp/places.sqlite \"select title,url from moz_places where last_visit_date is not null order by last_visit_date desc\" | head -1000"
                                           (expand-file-name "places.sqlite" (helm-get-firefox-user-init-dir helm-firefox-default-directory))))
                                  "\n")))
      :candidates (lambda ()
                    (cl-loop for f in helm-firefox-history-alist
                             collect (replace-regexp-in-string "\\(||\\)http.*\\'" "\n" f nil nil 1)))
      :multiline t
      :filtered-candidate-transformer
      '(helm-adaptive-sort
        helm-firefox-highlight-bookmarks)
      :action (helm-make-actions
               "Browse Url"
               (lambda (candidate)
                 (helm-browse-url candidate))
               "Copy Url"
               (lambda (url)
                 (kill-new url)
                 (message "`%s' copied to kill-ring" url)))))
  (defun helm-firefox-history ()
    (interactive)
    (helm :sources `(helm-source-firefox-history
                     ,(helm-build-dummy-source "DuckDuckgo"
                        :action (lambda (candidate)
                                  (helm-browse-url
                                   (format helm-surfraw-duckduckgo-url
                                           (url-hexify-string candidate))))))
          :truncate-lines t
          :buffer "*Helm Firefox History*")))
;; ===============helm-firefox================
(provide 'setup_browser)
