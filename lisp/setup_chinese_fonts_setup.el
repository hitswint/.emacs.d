;; ==================chinese-fonts-setup===================
(require 'chinese-fonts-setup)
;; (setq cfs-profiles
;;     '("program" "org-mode" "read-book"))
;; emacs启动时自动设定fontsize
(defun swint-cfs-set-font-with-saved-size ()
  (let* ((profile-name cfs--current-profile-name))
    (when (display-graphic-p)
      (cfs--set-font 11.5 1.2))))
(if (and (fboundp 'daemonp) (daemonp))
    (add-hook 'after-make-frame-functions
              (lambda (frame)
                (with-selected-frame frame
                  (swint-cfs-set-font-with-saved-size))))
  (add-hook 'window-setup-hook
            'swint-cfs-set-font-with-saved-size))
;; ==================chinese-fonts-setup===================
(provide 'setup_chinese_fonts_setup)
