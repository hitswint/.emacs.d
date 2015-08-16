;; ==================chinese-fonts-setup===================
(require 'chinese-fonts-setup)
(cond
 (is-lin (setq cfs--current-profile-name "profile-lin"))
 (is-win (setq cfs--current-profile-name "profile-win"))
 (is-mac (setq cfs--current-profile-name "profile-mac")))
;; emacs启动时自动设定fontsize
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
            'swint-cfs-set-font-with-saved-size))
;; ==================chinese-fonts-setup===================
(provide 'setup_chinese_fonts_setup)
