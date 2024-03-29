;;; chinese-fonts-setup
;; ==========chinese-fonts-setup===========
(use-package chinese-fonts-setup
  :load-path "site-lisp/chinese-fonts-setup/"
  :config
  (setq cfs--current-profile-name "profile-lin")
  (defun swint-cfs-set-font-with-saved-size (&optional frame)
    (with-selected-frame (or frame (selected-frame))
      (when (display-graphic-p frame)
        (cfs--set-font 11.5 1.2))))
  ;; 需设置LC_CTYPE=zh_CN.UTF-8才能正确设置字体大小
  (if (and (fboundp 'daemonp) (daemonp))
      (add-hook 'after-make-frame-functions
                'swint-cfs-set-font-with-saved-size)
    (add-hook 'window-setup-hook 'swint-cfs-set-font-with-saved-size)))
;; ==========chinese-fonts-setup===========
;;; pyim
;; ==================pyim==================
(use-package pyim
  :commands pyim-hanzi2pinyin-capitalize
  :bind (("C-x SPC" . pyim-convert-string-at-point)
         ("C-x S-SPC" . pyim-punctuation-translate-at-point))
  :init
  (setq default-input-method "pyim")
  (bind-key "C-S-SPC" 'toggle-input-method)
  :config
  ;; 使用pyim-fuzzy-pinyin-alist设置模糊音
  (setq pyim-page-tooltip 'popup)       ;posframe/popup/minibuffer
  ;; (pyim-isearch-mode 1)                 ;开启拼音搜索功能
  ;; (setq pyim-backends '(dcache-personal dcache-common pinyin-chars pinyin-shortcode pinyin-znabc)) ;设置词条获取方式
  ;; 使用探针(probe)函数判断当前语境以确定当前输入法和标点形式
  (setq-default pyim-english-input-switch-functions
                '(pyim-probe-program-mode
                  pyim-probe-org-speed-commands
                  pyim-probe-isearch-mode
                  pyim-probe-org-structure-template
                  pyim-probe-dynamic-english
                  ;; 在minibuffer关闭中文输入
                  window-minibuffer-p))
  (setq-default pyim-punctuation-half-width-functions
                '(pyim-probe-punctuation-line-beginning
                  pyim-probe-punctuation-after-punctuation))
  ;; 转化拼音首字母大写
  (defun pyim-hanzi2pinyin-capitalize (string &optional shou-zi-mu separator
                                              return-list ignore-duo-yin-zi adjust-duo-yin-zi)
    (let ((orig-fun (symbol-function 'pyim-cchar2pinyin-get)))
      (cl-letf (((symbol-function 'pyim-cchar2pinyin-get)
                 (lambda (arg)
                   (mapcar #'capitalize (funcall orig-fun arg)))))
        (pyim-hanzi2pinyin string shou-zi-mu separator
                           return-list ignore-duo-yin-zi adjust-duo-yin-zi))))
  (use-package pyim-basedict
    :config
    (pyim-basedict-enable)))
(use-package pyim-cstring-utils
  :commands pyim-cstring-words-at-point)
;; ==================pyim==================
;;; pinyinlib
;; ===============pinyinlib================
(use-package pinyinlib
  :commands pinyinlib-build-regexp-string)
;; ===============pinyinlib================
(provide 'setup_chinese)
