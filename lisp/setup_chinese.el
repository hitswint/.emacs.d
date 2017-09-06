;;; chinese-fonts-setup
;; ==========chinese-fonts-setup===========
(use-package chinese-fonts-setup
  ;; Enabled automatically.
  :load-path "site-lisp/chinese-fonts-setup/"
  :config
  (cond
   (is-lin (setq cfs--current-profile-name "profile-lin"))
   (is-win (setq cfs--current-profile-name "profile-win"))
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
;;; pyim
;; ==================pyim==================
(use-package pyim
  ;; Enabled at commands.
  :defer t
  :commands (pyim-cwords-at-point pyim-string-match-p)
  :bind (("C-x SPC" . pyim-convert-code-at-point)
         ("C-x S-SPC" . pyim-punctuation-translate-at-point))
  :init
  (bind-key "C-S-SPC" 'toggle-input-method)
  ;; Lin下使用Gtk+ tooltip，修改~/.emacs.d/gtkrc配置字体。
  (when is-lin (setq x-gtk-use-system-tooltips t))
  :config
  (setq default-input-method "pyim")
  ;; 使用pyim-fuzzy-pinyin-alist设置模糊音。
  ;; 设置选词框显示方式popup/pos-tip/nil。
  (setq pyim-page-tooltip 'pos-tip)
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
  (use-package pyim-basedict
    :config
    (pyim-basedict-enable)))
;; ==================pyim==================
(provide 'setup_chinese)
