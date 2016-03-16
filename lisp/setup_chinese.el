;; ==================chinese-fonts-setup===================
(use-package chinese-fonts-setup
  ;; Enabled automatically.
  :load-path "site-lisp/chinese-fonts-setup/"
  :config
  (cond
   (is-lin (setq cfs--current-profile-name "profile-lin"))
   (is-win (setq cfs--current-profile-name "profile-win")) ;win下需要安装libreoffice。
   (is-mac (setq cfs--current-profile-name "profile-mac")))
  ;; emacs启动时自动设定fontsize。
  (defun swint-cfs-set-font-with-saved-size ()
    (let* ((profile-name cfs--current-profile-name))
      (when (display-graphic-p)
        (cond
         (is-lin (cfs--set-font 11.5 1.2))
         (is-win (cfs--set-font 11.5 1.14))))))
  (if (and (fboundp 'daemonp) (daemonp))
      (add-hook 'after-make-frame-functions
                (lambda (frame)
                  (with-selected-frame frame
                    (swint-cfs-set-font-with-saved-size))))
    (add-hook 'window-setup-hook
              'swint-cfs-set-font-with-saved-size)))
;; ==================chinese-fonts-setup===================
;; =============chinese-pyim===============
(use-package chinese-pyim
  :defer 2
  :commands pyim-get-words-list-at-point
  :bind ("C-M-SPC" . pyim-convert-pinyin-at-point)
  :config
  (setq default-input-method "chinese-pyim")
  (global-set-key (kbd "C-x SPC") 'toggle-input-method)
  (global-set-key (kbd "S-SPC") 'pyim-punctuation-translate-at-point)
  (define-key pyim-mode-map "," 'pyim-previous-page)
  (define-key pyim-mode-map "." 'pyim-next-page)
  ;; (global-set-key (kbd "M-f") 'pyim-forward-word)
  ;; (global-set-key (kbd "M-b") 'pyim-backward-word)
  ;; pyim-delete-word-from-personal-buffer
  ;; 关闭使用双拼。
  ;; (setq pyim-default-pinyin-scheme 'pyim-shuangpin)
  (defun swint-pyim-probe-dynamic-english ()
    "Changed the behaviour of chinese-pyim at beginning of line."
    (if (or (= (- (point) (point-at-bol)) (current-indentation))
            (= (point) (point-at-bol)))
        (not (or (pyim-string-match-p "\\cc" (save-excursion
                                               ;; 查找前一个非空格字符。
                                               (if (re-search-backward "[^[:space:]]" nil t)
                                                   (char-to-string (char-after (point))))))
                 (> (length pyim-current-key) 0)))
      (pyim-probe-dynamic-english)))
  (setq-default pyim-english-input-switch-functions
                '(pyim-probe-org-speed-commands
                  pyim-probe-org-structure-template
                  pyim-probe-isearch-mode
                  pyim-probe-program-mode
                  swint-pyim-probe-dynamic-english
                  ;; 在minibuffer关闭中文输入。
                  window-minibuffer-p))
  (setq-default pyim-punctuation-half-width-functions
                '(pyim-probe-punctuation-line-beginning
                  pyim-probe-punctuation-after-punctuation))
  ;; 关闭拼音搜索功能。
  ;; (setq pyim-isearch-enable-pinyin-search t)
  ;; 设置选词框显示方式popup/pos-tip/nil。
  ;; 通过修改~/.emacs.d/gtkrc改变pos-tip字体。
  (setq pyim-use-tooltip 'pos-tip
        pyim-page-length 5)
  ;; lin下可以使用gtk绘制选词框。
  (when is-lin
    (setq x-gtk-use-system-tooltips t))
  ;; 使用pyim-fuzzy-pinyin-alist设置模糊音。
  ;; 使用pyim-enable-words-predict设置词语联想方式。
  ;; 其中guess-word需要安装guessdict词库。
  (cond
   (is-lin (setq pyim-dicts '((:name "bigdict" :file "/home/swint/.emacs.d/pyim/dicts/pyim-bigdict.pyim"
                                     :coding utf-8-unix :dict-type pinyin-dict)
                              (:name "guessdict" :file "/home/swint/.emacs.d/pyim/dicts/pyim-guessdict.gpyim"
                                     :coding utf-8-unix :dict-type guess-dict))))
   (is-win (setq pyim-dicts '((:name "bigdict" :file "c:/Users/swint/.emacs.d/pyim/dicts/pyim-bigdict.pyim"
                                     :coding utf-8-unix :dict-type pinyin-dict)
                              (:name "guessdict" :file "c:/Users/swint/.emacs.d/pyim/dicts/pyim-guessdict.gpyim"
                                     :coding utf-8-unix :dict-type guess-dict)))))
  ;; 仅加载chinese-pyim时并未生成pyim-cchar2pinyin-cache，导致使用pyim-get-words-list-at-point函数错误。
  (unless pyim-cchar2pinyin-cache
    (pyim-cchar2pinyin-create-cache))
  ;; 添加company支持，使其可以更加准确的补全。
  (require 'chinese-pyim-company)
  (setq pyim-company-max-length 10))
;; =============chinese-pyim===============
(provide 'setup_chinese)
