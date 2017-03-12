;;; chinese-fonts-setup
;; ==========chinese-fonts-setup===========
(use-package chinese-fonts-setup
  ;; Enabled automatically.
  :load-path "site-lisp/chinese-fonts-setup/"
  :config
  (cond
   (is-lin (setq cfs--current-profile-name "profile-lin"))
   (is-win (setq cfs--current-profile-name "profile-win")) ;Win下需要安装libreoffice。
   (is-mac (setq cfs--current-profile-name "profile-mac")))
  ;; Emacs启动时自动设定fontsize。
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
  :commands pyim-cwords-at-point
  :bind (("C-M-SPC" . pyim-convert-code-at-point)
         ("S-SPC" . pyim-punctuation-translate-at-point))
  :init
  (bind-key "C-S-SPC" 'toggle-input-method)
  :config
  (setq default-input-method "chinese-pyim")
  ;; 使用pyim-fuzzy-pinyin-alist设置模糊音。
  ;; 设置选词框显示方式popup/pos-tip/nil。
  (setq pyim-page-tooltip 'pos-tip)
  ;; Lin下使用gtk绘制选词框，通过修改~/.emacs.d/gtkrc改变pos-tip字体。
  (when is-lin
    (setq x-gtk-use-system-tooltips t))
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
  ;; 关闭拼音搜索功能并取消其对isearch-search-fun-function的设置。
  (setq pyim-isearch-enable-pinyin-search nil)
  (setq isearch-search-fun-function 'isearch-function-with-pinyin))
;;;; chinese-pyim-basedict
(use-package chinese-pyim-basedict
  ;; Enabled after features.
  :after chinese-pyim
  :config
  (chinese-pyim-basedict-enable))
;;;; chinese-pyim-greatdict
(use-package chinese-pyim-greatdict
  ;; Enabled after features.
  :after chinese-pyim
  :config
  (chinese-pyim-greatdict-enable))
;; =============chinese-pyim===============
(provide 'setup_chinese)
