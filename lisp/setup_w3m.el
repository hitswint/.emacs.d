;; ===================w3m=====================
;; (add-to-list 'exec-path "c:/Program Files (x86)/w3m") ;;指定w3m可执行程序，所在的执行路径
;; 上面那句不好用，无法确定w3m的程序执行路径。customize-group选择w3m，在W3m Command中添加c:/Program Files (x86)/w3m/w3m.exe，确定w3m程序。
;; 还是不行，因为anything-config.el中居然默认修改w3m-command为/usr/bin/w3m，导致上述修改在重启后失效。
;; 修改anything-config.el中的(defvar w3m-command "/usr/bin/w3m")为(defvar w3m-command "c:/Program Files (x86)/w3m/w3m.exe")，问题解决。
;; 在lin上面通过elpa安装，在win上直接使用.emacs.d/w3m中下载的已经编译好的代码。
(use-package w3m
  ;; Enabled at commands.
  :defer t
  :bind ("C-M-5" . w3m)
  :init
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
  :config
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
               (define-key w3m-mode-map [(\Q)] '(lambda () (interactive)
                                                  (w3m-quit 1)))
               (define-key w3m-mode-map (kbd "C-c o") 'w3m-open-site-current-session)
               (define-key w3m-mode-map (kbd "C-c t") 'w3m-open-site-new-session)
               (define-key w3m-mode-map (kbd "C-o b") '(lambda () (interactive)
                                                         (w3m-new-tab)
                                                         (w3m-browse-url "cn.bing.com")))
               (define-key w3m-mode-map (kbd "C-o g") '(lambda () (interactive)
                                                         (w3m-new-tab)
                                                         (w3m-browse-url "www.google.com")))
               (define-key w3m-mode-map (kbd "'") 'helm-firefox-bookmarks)))
  (use-package helm-firefox
    :defer t
    :commands helm-firefox-bookmarks
    :init
    (bind-key "'" 'helm-firefox-bookmarks w3m-mode-map)
    (when is-win
      (setq helm-firefox-default-directory "~/AppData/Roaming/Mozilla/Firefox/"))))
;; ===================w3m=====================
(provide 'setup_w3m)
