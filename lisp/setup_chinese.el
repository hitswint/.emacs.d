;;; chinese-fonts-setup
;; ==========chinese-fonts-setup===========
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
;; ==========chinese-fonts-setup===========
;;; chinese-pyim
;; =============chinese-pyim===============
(use-package chinese-pyim
  ;; Enabled at commands.
  :defer t
  :commands pyim-get-words-list-at-point
  :bind (("C-M-SPC" . pyim-convert-pinyin-at-point)
         ("S-SPC" . pyim-punctuation-translate-at-point))
  :init
  (bind-key "C-S-SPC" 'toggle-input-method)
  :config
  (setq default-input-method "chinese-pyim")
  ;; 使用pyim-fuzzy-pinyin-alist设置模糊音。
  ;; 设置选词框显示方式popup/pos-tip/nil。
  (setq pyim-use-tooltip 'pos-tip)
  ;; lin下使用gtk绘制选词框，通过修改~/.emacs.d/gtkrc改变pos-tip字体。
  (when is-lin
    (setq x-gtk-use-system-tooltips t))
  ;; 设置词语联想方式。guess-word需要安装guess-dict词库。
  (setq pyim-enable-words-predict
        '(dabbrev pinyin-shouzimu pinyin-similar pinyin-znabc)) ;guess-words
  ;; 设置词库文件路径。guess-word速度较慢，暂停使用。
  ;; (:name "guessdict" :file "/home/swint/.emacs.d/pyim/dicts/pyim-guessdict.gpyim"
  ;;     :coding utf-8-unix :dict-type guess-dict)
  (cond
   (is-lin (setq pyim-dicts '((:name "bigdict" :file "/home/swint/.emacs.d/pyim/dicts/pyim-bigdict.pyim"
                                     :coding utf-8-unix :dict-type pinyin-dict))))
   (is-win (setq pyim-dicts '((:name "bigdict" :file "c:/Users/swint/.emacs.d/pyim/dicts/pyim-bigdict.pyim"
                                     :coding utf-8-unix :dict-type pinyin-dict)))))
  ;; 使用探针(probe)函数判断当前语境以确定当前输入法和标点形式。
  (setq-default pyim-english-input-switch-functions
                '(pyim-probe-org-speed-commands
                  pyim-probe-org-structure-template
                  pyim-probe-isearch-mode
                  pyim-probe-program-mode
                  pyim-probe-dynamic-english
                  ;; 在minibuffer关闭中文输入。
                  window-minibuffer-p))
  (setq-default pyim-punctuation-half-width-functions
                '(pyim-probe-punctuation-line-beginning
                  pyim-probe-punctuation-after-punctuation))
  ;; 添加company支持，使其可以更加准确的补全。
  (require 'chinese-pyim-company)
  (setq pyim-company-max-length 10)
  ;; 关闭拼音搜索功能并取消其对isearch-search-fun-function的设置。
  (setq pyim-isearch-enable-pinyin-search nil)
  (setq isearch-search-fun-function 'isearch-function-with-pinyin))
;; =============chinese-pyim===============
(provide 'setup_chinese)
