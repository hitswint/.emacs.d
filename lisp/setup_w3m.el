;;; w3m
;; ===================w3m=====================
(use-package w3m
  ;; Enabled at commands.
  :defer t
  :bind (("C-M-5" . w3m)
         ("C-M-%" . w3m-youdao-sample-sentences))
  :init
  (when is-win
    (setq w3m-command "c:/Program Files (x86)/w3m/w3m.exe"))
  :config
  ;; Use w3m to display youdao sample sentences.
  (defun w3m-youdao-sample-sentences (&optional _word)
    (interactive)
    (let ((word (or _word (swint-get-words-at-point))))
      (browse-url
       (concat "http://dict.youdao.com/search?le=eng&q=lj%3A"
               (cond
                (is-lin word)
                ;; 解决w3m无法解析网址的问题。
                (is-win (w3m-url-encode-string word 'utf-8)))
               "&keyfrom=dict.top"))))
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
               (define-key w3m-mode-map [(\h)] 'w3m-previous-buffer)
               (define-key w3m-mode-map [(\l)] 'w3m-next-buffer)
               (define-key w3m-mode-map [(\q)] 'w3m-delete-buffer)
               (define-key w3m-mode-map [(\Q)] '(lambda () (interactive) (w3m-quit 1)))
               (define-key w3m-mode-map (kbd "C-c o") 'w3m-open-site-current-session)
               (define-key w3m-mode-map (kbd "C-c t") 'w3m-open-site-new-session)
               (define-key w3m-mode-map (kbd "C-o b") '(lambda () (interactive)
                                                         (w3m-new-tab)
                                                         (w3m-browse-url "cn.bing.com")))
               (define-key w3m-mode-map (kbd "C-o g") '(lambda () (interactive)
                                                         (w3m-new-tab)
                                                         (w3m-browse-url "www.google.com")))
               (define-key w3m-mode-map (kbd "'") 'helm-firefox-bookmarks))))
;; ===================w3m=====================
;;; helm-firefox
;; ===============helm-firefox================
(use-package helm-firefox
  ;; Enabled at commands.
  :defer t
  :commands (helm-firefox-bookmarks helm-get-firefox-user-init-dir)
  :config
  (when is-win
    (setq helm-firefox-default-directory "~/AppData/Roaming/Mozilla/Firefox/")))
;; ===============helm-firefox================
(provide 'setup_w3m)
