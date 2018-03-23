;;; octave
;; ======================octave====================
(def-package! octave
  :mode ("\\.m$" . octave-mode)
  :config
  ;; ac-octave/auto-complete-octave编译配置都有问题。
  (add-hook 'octave-mode-hook
            '(lambda ()
               (if (eq window-system 'x)
                   (font-lock-mode 1))
               (define-key octave-mode-map (kbd "C-c C-,") 'octave-find-definition)
               (define-key octave-mode-map (kbd "C-c C-.") 'pop-tag-mark)
               (define-key octave-mode-map (kbd "C-c C-/") 'octave-help)
               (define-key octave-mode-map (kbd "C-c C-c") '(lambda ()
                                                              (interactive)
                                                              (if mark-active
                                                                  (call-interactively 'octave-send-region)
                                                                (call-interactively 'octave-send-buffer))))
               (define-key octave-mode-map (kbd "M-.") nil)
               (define-key octave-mode-map (kbd "C-h") nil)))
  (add-hook 'inferior-octave-mode-hook
            (lambda ()
              (turn-on-font-lock)
              (define-key inferior-octave-mode-map (kbd "C-c C-,") 'octave-find-definition)
              (define-key inferior-octave-mode-map (kbd "C-c C-/") 'octave-help)
              (define-key inferior-octave-mode-map (kbd "C-q") 'comint-send-eof)
              (define-key inferior-octave-mode-map [up] 'comint-previous-input)
              (define-key inferior-octave-mode-map [down] 'comint-next-input)
              (define-key inferior-octave-mode-map (kbd "M-.") nil)
              (define-key inferior-octave-mode-map (kbd "C-h") nil)))
  ;; 使用%注释。
  (setq octave-comment-start "%")
  (setq octave-comment-char ?%)
  (setq octave-auto-indent t)
  ;; 退出octave时关闭buffer。
  (add-hook 'inferior-octave-mode-hook 'kill-shell-buffer-after-exit t))
;; ======================octave====================
(provide 'setup_octave)
