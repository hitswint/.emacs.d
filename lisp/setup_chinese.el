;;; chinese-fonts-setup
;; ==========chinese-fonts-setup===========
(def-package! chinese-fonts-setup
  :load-path "site-lisp/chinese-fonts-setup/"
  :config
  (setq cfs--current-profile-name "profile-lin")
  ;; Emacs启动时自动设定fontsize。
  (defun swint-cfs-set-font-with-saved-size (frame)
    (with-selected-frame frame
      (when (display-graphic-p frame)
        (cfs--set-font 11.5 1.2))))
  (if (and (fboundp 'daemonp) (daemonp))
      (add-hook 'after-make-frame-functions
                'swint-cfs-set-font-with-saved-size)
    (add-hook 'window-setup-hook
              (lambda () (swint-cfs-set-font-with-saved-size (selected-frame))))))
;; ==========chinese-fonts-setup===========
;;; pyim
;; ==================pyim==================
(def-package! pyim
  :commands (pyim-cwords-at-point
             pyim-string-match-p
             pyim-hanzi2pinyin-capitalize)
  :bind (("C-x SPC" . pyim-convert-string-at-point)
         ("C-x S-SPC" . pyim-punctuation-translate-at-point))
  :init
  (setq default-input-method "pyim")
  (bind-key "C-S-SPC" 'toggle-input-method)
  :config
  ;; 使用pyim-fuzzy-pinyin-alist设置模糊音。
  ;; 设置选词框显示方式posframe/popup/minibuffer。
  (setq pyim-page-tooltip 'popup)
  ;; 开启拼音搜索功能。
  ;; (pyim-isearch-mode 1)
  ;; 设置词条获取方式。
  ;; (setq pyim-backends '(dcache-personal dcache-common pinyin-chars pinyin-shortcode pinyin-znabc))
  ;; 使用探针(probe)函数判断当前语境以确定当前输入法和标点形式。
  (setq-default pyim-english-input-switch-functions
                '(pyim-probe-program-mode
                  pyim-probe-org-speed-commands
                  pyim-probe-isearch-mode
                  pyim-probe-org-structure-template
                  pyim-probe-dynamic-english
                  ;; 在minibuffer关闭中文输入。
                  window-minibuffer-p))
  (setq-default pyim-punctuation-half-width-functions
                '(pyim-probe-punctuation-line-beginning
                  pyim-probe-punctuation-after-punctuation))
  ;; 转化拼音首字母大写。
  (defun pyim-hanzi2pinyin-capitalize (string &optional shou-zi-mu separator
                                              return-list ignore-duo-yin-zi adjust-duo-yin-zi)
    (let ((orig-fun (symbol-function 'pyim-cchar2pinyin-get)))
      (letf (((symbol-function 'pyim-cchar2pinyin-get)
              (lambda (arg)
                (mapcar #'capitalize (funcall orig-fun arg)))))
        (pyim-hanzi2pinyin string shou-zi-mu separator
                           return-list ignore-duo-yin-zi adjust-duo-yin-zi))))
  (def-package! pyim-basedict
    :config
    (pyim-basedict-enable)))
;; ==================pyim==================
;;; pinyinlib
;; ===============pinyinlib================
(def-package! pinyinlib
  :commands pinyinlib-build-regexp-string)
;; ===============pinyinlib================
(provide 'setup_chinese)
