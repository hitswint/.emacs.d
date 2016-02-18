;; ===================octave==================
(use-package octave
  ;; Enabled in octave-mode.
  :defer t
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
            (lambda ()
              (abbrev-mode 1)
              (auto-fill-mode 1)
              (if (eq window-system 'x)
                  (font-lock-mode 1))))
  (add-hook 'inferior-octave-mode-hook ;up and down arrow in the shell
            (lambda ()
              (turn-on-font-lock)
              (define-key inferior-octave-mode-map (kbd "M-.") nil)
              (define-key inferior-octave-mode-map (kbd "C-c RET") nil)
              (define-key inferior-octave-mode-map [up] 'comint-previous-input)
              (define-key inferior-octave-mode-map [down] 'comint-next-input)))
  (setq octave-comment-start "%% ")       ;使用%注释
  (setq octave-comment-char 37)           ;使用%注释
  (add-hook 'octave-mode-hook
            '(lambda ()
               (define-key octave-mode-map (kbd "C-c i") 'octave-send-line)
               (define-key octave-mode-map (kbd "C-c o") 'octave-send-region)
               (define-key octave-mode-map (kbd "C-c C-i") nil)
               (define-key octave-mode-map (kbd "C-c C-f") nil)
               (define-key octave-mode-map [(control \h)] nil)
               (define-key octave-mode-map [(meta \q)] nil)))
  ;; 退出octave时关闭buffer
  (add-hook 'inferior-octave-mode-hook 'kill-shell-buffer-after-exit t))
;; ===================octave==================
(provide 'setup_octave)
