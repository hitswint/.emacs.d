;;; python-mode
;; =================python-mode================
(use-package python
  :mode ("\\.py\\'" . python-mode))
;; =================python-mode================
;;; pyvenv
;; ===================pyvenv===================
(use-package pyvenv
  :bind (("C-x C-M-3" . pyvenv-workon)
         ("C-x C-M-#" . pyvenv-deactivate)
         ("C-M-3" . elpy-shell-switch-to-shell))
  :config
  ;; 使用pyvenv-activate/deactivate启动/关闭虚拟环境，使用pyvenv-workon列出可用虚拟环境并切换。
  (defalias 'workon 'pyvenv-workon)
  ;; ipython默认设置有bug，需要加--simple-prompt选项。
  (setq python-shell-interpreter "ipython"
        python-shell-interpreter-args "-i --simple-prompt --pylab")
  (defun swint-elpy-shell-switch-to-shell (fn)
    (unless pyvenv-virtual-env
      (call-interactively 'pyvenv-workon))
    (funcall fn))
  (advice-add 'elpy-shell-switch-to-shell :around #'swint-elpy-shell-switch-to-shell))
;; ===================pyvenv===================
;;; elpy
;; ====================elpy====================
(use-package elpy
  :after pyvenv
  :init
  (setq elpy-remove-modeline-lighter nil)
  :config
  (elpy-enable)
  ;; 同时安装jedi/rope时默认采用rope，手动设置以便采用jedi后端。
  (setq elpy-rpc-backend "jedi")
  (setq elpy-rpc-timeout nil)
  (add-hook 'inferior-python-mode-hook 'kill-shell-buffer-after-exit t)
  (define-key elpy-mode-map (kbd "M-.") nil)
  (define-key elpy-mode-map (kbd "C-c C-c") '(lambda (arg) (interactive "P")
                                               (if arg (elpy-use-cpython)
                                                 (elpy-use-ipython)
                                                 (setq python-shell-interpreter-args "--simple-prompt --pylab"))
                                               (call-interactively 'elpy-shell-send-region-or-buffer)))
  (define-key elpy-mode-map (kbd "C-c C-,") 'elpy-goto-definition)
  (define-key elpy-mode-map (kbd "C-c C-.") 'pop-tag-mark)
  (define-key elpy-mode-map (kbd "C-c C-/") 'elpy-doc)
  (define-key elpy-mode-map (kbd "C-c c") 'ein:connect-to-notebook-command)
  (define-key inferior-python-mode-map (kbd "C-q") 'comint-send-eof)
  (define-key inferior-python-mode-map (kbd "C-c C-,") 'elpy-goto-definition)
  (define-key inferior-python-mode-map (kbd "C-c C-.") 'pop-tag-mark)
  (define-key inferior-python-mode-map (kbd "C-c C-/") 'elpy-doc)
  ;; 使用global-elpy-mode方式开启elpy-mode。
  ;; (define-global-minor-mode global-elpy-mode elpy-mode
  ;;   (lambda () (when (eq major-mode 'python-mode) (elpy-mode 1))))
  ;; (global-elpy-mode 1)
  ;; 在opened python buffer中开启elpy-mode。
  (dolist (buf (remove-if-not (lambda (x)
                                (equal (buffer-mode x) 'python-mode))
                              (helm-buffer-list)))
    (with-current-buffer buf
      (elpy-mode 1))))
;; ====================elpy====================
;;; emacs-ipython-notebook
;; ====================ein=====================
(use-package ein
  :bind ("C-M-#" . ein:jupyter-server-start)
  :config
  ;; ein:url-or-port可取8888或http://127.0.0.1(localhost):8888。
  (defun swint-ein:jupyter-server-start (fn &rest args)
    (interactive
     (lambda (spec)
       (unless (and (boundp 'pyvenv-virtual-env) pyvenv-virtual-env)
         (call-interactively 'pyvenv-workon))
       (unless (buffer-live-p (get-buffer ein:jupyter-server-buffer-name))
         (advice-eval-interactive-spec spec))))
    (if (buffer-live-p (get-buffer ein:jupyter-server-buffer-name))
        (ein:notebooklist-open)
      (let ((ein:jupyter-server-args '("--no-browser")))
        (apply fn args)
        (set-process-query-on-exit-flag (get-process "EIN: Jupyter notebook server") nil))))
  (advice-add 'ein:jupyter-server-start :around #'swint-ein:jupyter-server-start)
  ;; 默认补全后端为ac，可选company。
  ;; (setq ein:completion-backend 'ein:use-company-backend)
  ;; Enable "superpack" (a little bit hacky improvements).
  (setq ein:use-auto-complete-superpack t)
  (setq ein:use-smartrep t)
  (use-package ein-notebook
    :config
    ;; 在notebook中输入%pylab(%matplotlib) inline显示行内图片。
    ;; 在ein:notebook中关闭company的自动补全。
    (add-hook 'ein:notebook-mode-hook '(lambda ()
                                         (set (make-local-variable 'company-idle-delay) nil)))
    (setq ein:helm-kernel-history-search-key "\M-r")
    (define-key ein:notebook-mode-map (kbd "M-,") nil)
    (define-key ein:notebook-mode-map (kbd "M-.") nil)
    (define-key ein:notebook-mode-map (kbd "C-c C-,") 'ein:pytools-jump-to-source-command)
    (define-key ein:notebook-mode-map (kbd "C-c C-.") 'ein:pytools-jump-back-command))
  (use-package ein-connect
    :commands ein:connect-to-notebook-command
    :config
    ;; 在ein:connect中关闭company的自动补全。
    (add-hook 'ein:connect-mode-hook '(lambda ()
                                        (set (make-local-variable 'company-idle-delay) nil)))
    ;; 取消ein:connect-mode-map默认快捷键，以免与elpy冲突。
    (define-key ein:connect-mode-map "\C-c\C-c" nil)
    (define-key ein:connect-mode-map "\C-c\C-l" nil)
    (define-key ein:connect-mode-map "\C-c\C-r" nil)
    (define-key ein:connect-mode-map "\C-c\C-f" nil)
    (define-key ein:connect-mode-map "\C-c\C-z" nil)
    (define-key ein:connect-mode-map "\C-c\C-a" nil)
    (define-key ein:connect-mode-map "\C-c\C-o" nil)
    (define-key ein:connect-mode-map "\C-c\C-x" nil)
    (define-key ein:connect-mode-map (kbd "C-:") nil)
    (define-key ein:connect-mode-map "\M-," nil)
    (define-key ein:connect-mode-map "\M-." nil)
    (define-key ein:connect-mode-map (kbd "C-c C-,") nil)
    (define-key ein:connect-mode-map (kbd "C-c C-.") nil)
    (define-key ein:connect-mode-map (kbd "C-c C-/") nil)
    ;; Elpy快捷键为C-c C-x，ein快捷键为C-c x。
    (define-key ein:connect-mode-map "\C-cc" 'ein:connect-run-or-eval-buffer)
    (define-key ein:connect-mode-map "\C-cl" 'ein:connect-reload-buffer)
    (define-key ein:connect-mode-map "\C-cr" 'ein:connect-eval-region)
    (define-key ein:connect-mode-map "\C-ce" 'ein:shared-output-eval-string)
    (define-key ein:connect-mode-map "\C-co" 'ein:console-open)
    (define-key ein:connect-mode-map "\C-cs" 'ein:notebook-scratchsheet-open)
    (define-key ein:connect-mode-map "\C-ca" 'ein:connect-toggle-autoexec)
    (define-key ein:connect-mode-map "\C-cz" 'ein:connect-pop-to-notebook)
    (define-key ein:connect-mode-map "\C-cx" 'ein:tb-show)
    (define-key ein:connect-mode-map (kbd "C-c ,") 'ein:pytools-jump-to-source-command)
    (define-key ein:connect-mode-map (kbd "C-c .") 'ein:pytools-jump-back-command)
    (define-key ein:connect-mode-map (kbd "C-c /") 'ein:pytools-request-tooltip-or-help)))
;; ====================ein=====================
(provide 'setup_python)
