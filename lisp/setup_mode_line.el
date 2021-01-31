;;; smart-mode-line
;; ================smart-mode-line=================
(def-package! smart-mode-line
  :config
  (setq sml/no-confirm-load-theme t)
  ;; 设定theme为dark/light/respectful/automatic。
  (sml/setup)
  (sml/apply-theme nil)
  (setq column-number-mode t)
  (setq-default mode-line-format
                ;; 去除vc-mode显示，mode-line-modes显示major/minor-mode
                (delete '(vc-mode vc-mode) mode-line-format))
  (setq sml/col-number-format "%3c"
        sml/directory-truncation-string ""
        sml/line-number-format "%4l"
        sml/mode-width 'full
        sml/name-width 64
        sml/new-mail-background-color "black"
        sml/position-percentage-format ""
        sml/shorten-modes nil
        sml/shorten-mode-string "")
  (custom-set-faces '(sml/col-number ((t (:foreground "lawn green"))))
                    '(sml/filename ((t (:inherit sml/global :foreground "yellow" :weight bold))))
                    '(sml/line-number ((t (:foreground "gold"))))
                    '(sml/modes ((t (:inherit sml/global :foreground "gray50"))))
                    '(sml/numbers-separator ((t (:inherit sml/global))))
                    '(sml/position-percentage ((t (:foreground "yellow"))))
                    '(sml/prefix ((t (:inherit sml/global :foreground "deep sky blue" :weight bold))))
                    '(sml/projectile ((t (:foreground "deep sky blue" :weight bold))))
                    '(sml/read-only ((t (:inherit sml/not-modified :foreground "gray50")))))
  ;; Replacer for path.
  (add-to-list 'sml/replacer-regexp-list '("^~/linux/" ":Lin:"))
  (add-to-list 'sml/replacer-regexp-list '("^~/book/" ":Book:"))
  (add-to-list 'sml/replacer-regexp-list '("^~/papers/" ":Pap:"))
  (add-to-list 'sml/replacer-regexp-list '("^~/tex/" ":TeX:"))
  (add-to-list 'sml/replacer-regexp-list '("^~/myfile/" ":MY:"))
  (add-to-list 'sml/replacer-regexp-list '("^~/Downloads/" ":DL:"))
  (add-to-list 'sml/replacer-regexp-list '("^~/git-repo/" ":Git:"))
  (add-to-list 'sml/replacer-regexp-list '("^~/OpenFOAM/" ":OF:"))
  (add-to-list 'sml/replacer-regexp-list '("^~/Pictures/" ":Pic:"))
  (add-to-list 'sml/replacer-regexp-list '("^~/Zotero/" ":Zot:"))
  ;; Hide minor modes.
  (add-to-list 'rm-blacklist " Fill")
  (add-to-list 'rm-blacklist " iImg")
  (add-to-list 'rm-blacklist " Isearch")
  (add-to-list 'rm-blacklist " OrgTbl")
  (add-to-list 'rm-blacklist " Omit")
  (add-to-list 'rm-blacklist " ElDoc")
  (add-to-list 'rm-blacklist " Helm"))
;; ================smart-mode-line=================
;;; nyan-mode
;; ===================nyan-mode====================
(def-package! nyan-mode
  :config
  (nyan-mode t)
  (setq nyan-bar-length 16))
;; ===================nyan-mode====================
(provide 'setup_mode_line)
