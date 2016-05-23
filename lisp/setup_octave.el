;;; octave
;; ======================octave====================
(use-package octave
  ;; Enabled in modes.
  :defer t
  :commands octave-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.m$" . octave-mode))
  :config
  ;; (setenv "PATH" (concat (getenv "PATH") "c:/Octave/Octave3.6.4_gcc4.6.2/bin/"))
  ;; (setq exec-path (append exec-path '("c:/Octave/Octave3.6.4_gcc4.6.2/bin/")))
  ;; 使用mingw的octave，下列路径中有octave.exe
  (when is-win
    (add-to-list 'exec-path "c:/Octave/Octave3.6.4_gcc4.6.2/bin/"))
  ;; 使用cygwin安装octave，速度很快，但无法在emacs中启动。
  ;; 由于inferior-octave-program为octave，而c:/cygwin64/bin中有octave-3.8.2.exe和octave，没有octave.exe
  ;; 显示searching for program: permission denied, octave
  ;; 修改inferior-octave-program的定义，找到该程序，但emacs冻结，没有效果，放弃使用。
  ;; (add-to-list 'exec-path "c:/cygwin64/bin/")
  ;; (setq inferior-octave-program "octave-3.8.2.exe")
  ;;使用matlab-mode编辑文件，使用octave运行程序
  ;; (setq auto-mode-alist (remq '("\\.m\\'" . objc-mode) auto-mode-alist))
  ;; (autoload 'matlab-mode "matlab" "Enter MATLAB mode." t)
  ;; (setq auto-mode-alist (cons '("\\.m\\'" . matlab-mode) auto-mode-alist))
  ;; (autoload 'matlab-shell "matlab" "Interactive MATLAB mode." t)
  ;; (setq matlab-shell-command "octave"
  ;;       shell-command-echoes nil)
  ;; (setq matlab-shell-command-switches '("--traditional"))
  ;; (setq matlab-shell-buffer-name "octave")
  ;; (defalias 'octave-shell 'matlab-shell)
  ;;使用octave-mode
  ;; (autoload 'octave-mode "octave-mod" nil t) ;显示无法找到octave-mod
  (add-hook 'octave-mode-hook
            '(lambda ()
               (abbrev-mode 1)
               (auto-fill-mode 1)
               (if (eq window-system 'x)
                   (font-lock-mode 1))
               (define-key octave-mode-map (kbd "C-c C-,") 'octave-find-definition)
               (define-key octave-mode-map (kbd "C-c C-/") 'octave-help)
               (define-key octave-mode-map (kbd "C-c q") '(lambda ()
                                                            (interactive)
                                                            (if mark-active
                                                                (call-interactively 'octave-send-region)
                                                              (call-interactively 'octave-send-line))))
               (define-key octave-mode-map (kbd "M-.") nil)
               (define-key octave-mode-map (kbd "C-h") nil)))
  (add-hook 'inferior-octave-mode-hook
            (lambda ()
              (turn-on-font-lock)
              (define-key inferior-octave-mode-map (kbd "C-c C-,") 'octave-find-definition)
              (define-key inferior-octave-mode-map (kbd "C-c C-/") 'octave-help)
              (define-key inferior-octave-mode-map [up] 'comint-previous-input)
              (define-key inferior-octave-mode-map [down] 'comint-next-input)
              (define-key inferior-octave-mode-map (kbd "M-.") nil)
              (define-key inferior-octave-mode-map (kbd "C-h") nil)))
  ;; 使用%注释。
  (setq octave-comment-start "%")
  (setq octave-comment-char ?%)
  ;; 退出octave时关闭buffer。
  (add-hook 'inferior-octave-mode-hook 'kill-shell-buffer-after-exit t))
;; ======================octave====================
(provide 'setup_octave)
