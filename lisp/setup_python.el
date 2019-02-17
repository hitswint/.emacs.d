;;; python-mode
;; =================python-mode================
(def-package! python
  :mode ("\\.py\\'" . python-mode))
;; =================python-mode================
;;; pyvenv
;; ===================pyvenv===================
(def-package! pyvenv
  :commands (pyvenv-workon-home pyvenv-activate)
  :bind (("C-x C-M-3" . pyvenv-workon)
         ("C-x C-M-#" . pyvenv-deactivate))
  :config
  (pyvenv-mode 1)
  ;; 使用pyvenv-activate/deactivate启动/关闭虚拟环境，使用pyvenv-workon列出可用虚拟环境并切换。
  (defalias 'workon 'pyvenv-workon)
  ;; ipython默认设置有bug，需要加--simple-prompt选项。
  (setq python-shell-interpreter "ipython"
        python-shell-interpreter-args "-i --simple-prompt --pylab"))
;; ===================pyvenv===================
;;; elpy
;; ====================elpy====================
(def-package! elpy
  :diminish elpy-mode
  :commands (elpy-shell-switch-to-shell toggle-elpy-mode-all-buffers)
  :init
  (bind-key "C-M-3" 'elpy-shell-switch-to-shell)
  (add-hook 'python-mode-hook (lambda ()
                                (bind-key "C-c t" 'toggle-elpy-mode-all-buffers python-mode-map)))
  (setq elpy-remove-modeline-lighter nil)
  :config
  (setq elpy-rpc-timeout nil)
  (setq elpy-shell-use-project-root nil)
  (add-hook 'inferior-python-mode-hook 'kill-shell-buffer-after-exit t)
  (define-key elpy-mode-map (kbd "M-.") nil)
  (define-key elpy-mode-map (kbd "C-c C-,") 'elpy-goto-definition)
  (define-key elpy-mode-map (kbd "C-c C-.") 'pop-tag-mark)
  (define-key elpy-mode-map (kbd "C-c C-/") 'elpy-doc)
  (define-key inferior-python-mode-map (kbd "C-q") 'comint-send-eof)
  (define-key inferior-python-mode-map (kbd "C-c C-,") 'elpy-goto-definition)
  (define-key inferior-python-mode-map (kbd "C-c C-.") 'pop-tag-mark)
  (define-key inferior-python-mode-map (kbd "C-c C-/") 'elpy-doc)
  (advice-add 'elpy-shell-switch-to-shell :before #'(lambda ()
                                                      (unless (equal (bound-and-true-p pyvenv-virtual-env-name) "py3")
                                                        (pyvenv-activate (format "%s/%s" (pyvenv-workon-home) "py3")))))
  ;; 使用global-elpy-mode方式开启elpy-mode。
  ;; (define-global-minor-mode global-elpy-mode elpy-mode
  ;;   (lambda () (when (eq major-mode 'python-mode) (elpy-mode 1))))
  ;; (global-elpy-mode 1)
  ;; 在opened python buffer中开关elpy-mode。
  (defun toggle-elpy-mode-all-buffers ()
    (interactive)
    (if elpy-modules-initialized-p
        (progn (elpy-disable)
               (pyvenv-mode 1))
      (unless (equal (bound-and-true-p pyvenv-virtual-env-name) "py3")
        (pyvenv-activate (format "%s/%s" (pyvenv-workon-home) "py3")))
      (elpy-enable))
    (dolist (buf (cl-remove-if-not (lambda (x)
                                     (equal (buffer-mode x) 'python-mode))
                                   (buffer-list)))
      (with-current-buffer buf
        (elpy-mode 'toggle)))))
;; ====================elpy====================
;;; emacs-ipython-notebook
;; ====================ein=====================
(def-package! ein
  :bind (("M-g j" . ein:jupyter-server-start)
         ("M-g J" . ein:jupyter-server-stop))
  :config
  ;; ein:url-or-port可取8888或http://127.0.0.1(localhost):8888。
  ;; ein:jupyter-server-conn-info搜索并确定url和token，但中文提示导致搜索失败。
  (defun ein:jupyter-server-conn-info/override (&optional buffer)
    "Return the url-or-port and password for BUFFER or the global session."
    (unless buffer
      (setq buffer (get-buffer ein:jupyter-server-buffer-name)))
    (let ((result '(nil nil)))
      (if buffer
          (with-current-buffer buffer
            (save-excursion
              (goto-char (point-max))
              (re-search-backward (format "Process %s" *ein:jupyter-server-process-name*)
                                  nil "") ;; important if we start-stop-start
              (when (or (re-search-forward "\\([[:alnum:]]+\\) is\\( now\\)? running" nil t)
                        (re-search-forward "\\(本程序运行在\\)" nil t))
                (let ((hub-p (search "jupyterhub" (downcase (match-string 1)))))
                  (when (re-search-forward "\\(https?://[^:]*:[0-9]+\\)\\(?:/\\?token=\\([[:alnum:]]+\\)\\)?" nil t)
                    (let ((raw-url (match-string 1))
                          (token (or (match-string 2) (and (not hub-p) ""))))
                      (setq result (list (ein:url raw-url) token)))))))))
      result))
  (advice-add 'ein:jupyter-server-conn-info :override #'ein:jupyter-server-conn-info/override)
  (defun ein:jupyter-server-start/around (fn &rest args)
    (interactive
     (lambda (spec)
       (unless (equal (bound-and-true-p pyvenv-virtual-env-name) "py3")
         (pyvenv-activate (format "%s/%s" (pyvenv-workon-home) "py3")))
       (unless (ein:jupyter-server-process)
         (advice-eval-interactive-spec spec))))
    (if (ein:jupyter-server-process)
        (call-interactively 'ein:notebooklist-login)
      (apply fn args)
      (set-process-query-on-exit-flag (get-process "EIN: Jupyter notebook server") nil)
      (setq *ein:last-jupyter-directory* nil)))
  (advice-add 'ein:jupyter-server-start :around #'ein:jupyter-server-start/around)
  ;; 默认补全后端为ac，可选company。
  ;; (setq ein:completion-backend 'ein:use-company-backend)
  (setq ein:use-auto-complete-superpack t)
  (setq ein:use-smartrep t)
  (def-package! ein-notebook
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
  (def-package! ein-connect
    :commands ein:connect-to-notebook-command
    :init
    (add-hook 'python-mode-hook (lambda ()
                                  (bind-key "C-c c" 'ein:connect-to-notebook-command python-mode-map)))
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
