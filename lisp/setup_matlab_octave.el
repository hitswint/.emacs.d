;; =========================MATLAB===============================
;; 启用matlab-mode
;; (setenv "PATH" (concat (getenv "PATH") "/usr/local/MATLAB/R2011b/bin/"))
;; (setq exec-path (append exec-path '("/usr/local/MATLAB/R2011b/bin/")))
;; (server-start)
;; (add-to-list 'load-path
;;              "~/.emacs.d/matlab-emacs/matlab-emacs")
;; (require 'matlab-load)
;; (autoload 'matlab-mode "matlab" "Enter MATLAB mode." t)
;; (setq auto-mode-alist (cons '("\\.m\\'" . matlab-mode) auto-mode-alist))
;; (autoload 'matlab-shell "matlab" "Interactive MATLAB mode." t)
;; (setq matlab-indent-function-body t)    ; if you want function bodies indented
;; (setq matlab-verify-on-save-flag nil)   ; turn off auto-verify on save
;; (defun my-matlab-mode-hook ()
;;   (setq fill-column 76)
;;   (imenu-add-to-menubar "Find"))        ; where auto-fill should wrap
;; (add-hook 'matlab-mode-hook 'my-matlab-mode-hook)
;; (defun my-matlab-shell-mode-hook ()
;;   '())
;; (add-hook 'matlab-shell-mode-hook 'my-matlab-shell-mode-hook)
;; (setq matlab-shell-command-switches '("-nodesktop -nosplash"))
;; (add-hook 'matlab-mode-hook
;;           '(lambda ()
;;              (define-key matlab-mode-map [(control \h)] nil)
;;              (define-key matlab-mode-map [(meta \q)] nil)
;;              (define-key matlab-mode-map [(control meta \e)] nil)
;;              )) ;取消C-h的快捷键
;; =========================MATLAB===============================
;; ======================octave==============================
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
(setq auto-mode-alist
      (cons '("\\.m$" . octave-mode) auto-mode-alist))
(add-hook 'octave-mode-hook
          (lambda ()
            (abbrev-mode 1)
            (auto-fill-mode 1)
            (if (eq window-system 'x)
                (font-lock-mode 1))))
(add-hook 'inferior-octave-mode-hook ;up and down arrow in the shell
          (lambda ()
            (turn-on-font-lock)
            (define-key inferior-octave-mode-map (kbd "C-c RET") nil)
            (define-key inferior-octave-mode-map [up]
              'comint-previous-input)
            (define-key inferior-octave-mode-map [down]
              'comint-next-input)))
(setq octave-comment-start "%% ")        ;使用%注释
(setq octave-comment-char 37)           ;使用%注释
(add-hook 'octave-mode-hook
          '(lambda ()
             (define-key octave-mode-map (kbd "C-c i") 'octave-send-line)
             (define-key octave-mode-map (kbd "C-c o") 'octave-send-region)
             (define-key octave-mode-map (kbd "C-c C-i") nil)
             (define-key octave-mode-map (kbd "C-c C-f") nil)
             (define-key octave-mode-map [(control \h)] nil)
             (define-key octave-mode-map [(meta \q)] nil)
             )) ;取消C-h的快捷键
;; 退出octave时关闭buffer
(defun kill-octave-buffer(process event)
  "The one actually kill octave buffer when exit. "
  (kill-buffer (process-buffer process))
  )
(defun kill-octave-buffer-after-exit()
  "kill octave buffer when exit."
  (set-process-sentinel (get-buffer-process (current-buffer))
                        #'kill-octave-buffer)
  )
(add-hook 'inferior-octave-mode-hook 'kill-octave-buffer-after-exit t)
;; ======================octave==============================
(provide 'setup_matlab_octave)
