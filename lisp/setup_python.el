;;; python-mode
;; =================python-mode================
(def-package! python
  :mode ("\\.py\\'" . python-mode)
  :config
  (add-hook 'inferior-python-mode-hook 'kill-shell-buffer-after-exit t))
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
  (defalias 'workon 'pyvenv-workon))
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
  (setq elpy-shell-starting-directory nil)
  ;; ipython默认设置有bug，需要加--simple-prompt选项。
  (setq python-shell-interpreter "ipython"
        python-shell-interpreter-args "-i --simple-prompt --pylab")
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
        (progn (dolist (buf (cl-remove-if-not (lambda (x)
                                                (equal (buffer-mode x) 'python-mode))
                                              (buffer-list)))
                 (with-current-buffer buf
                   (elpy-mode 'toggle)))
               (elpy-disable)
               (pyvenv-mode 1))
      (unless (equal (bound-and-true-p pyvenv-virtual-env-name) "py3")
        (pyvenv-activate (format "%s/%s" (pyvenv-workon-home) "py3")))
      (elpy-enable))))
;; ====================elpy====================
;;; emacs-ipython-notebook
;; ====================ein=====================
(def-package! ein
  :bind (("M-g j" . ein:jupyter-server-start)
         ("M-g J" . ein:jupyter-server-stop))
  :config
  ;; ein:url-or-port可取8888或http://127.0.0.1(localhost):8888。
  ;; ein:jupyter-server-conn-info搜索并确定url和token，但中文提示导致搜索失败。
  (defun ein:jupyter-server-conn-info/override (&optional buffer-name)
    "Return the url-or-port and password for BUFFER or the global session."
    (unless buffer-name
      (setq buffer-name ein:jupyter-server-buffer-name))
    (let ((buffer (get-buffer buffer-name))
          (result '(nil nil)))
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
  ;; 补全后端可选ac/company。
  (setq ein:completion-backend 'ein:use-ac-backend)
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
    :bind (:map python-mode-map
                ("C-c c" . ein:connect-to-notebook-command))
    :config
    ;; 在ein:connect中关闭company的自动补全。
    (add-hook 'ein:connect-mode-hook '(lambda ()
                                        (set (make-local-variable 'company-idle-delay) nil)))
    ;; 与elpy快捷键冲突。
    (define-key ein:connect-mode-map (kbd "C-:") nil)
    (define-key ein:connect-mode-map "\M-," nil)
    (define-key ein:connect-mode-map "\M-." nil)
    (define-key ein:connect-mode-map "\C-c\C-e" 'ein:shared-output-eval-string)
    (define-key ein:connect-mode-map (kbd "C-c C-,") 'ein:pytools-jump-to-source-command)
    (define-key ein:connect-mode-map (kbd "C-c C-.") 'ein:pytools-jump-back-command)))
;; ====================ein=====================
;;; jupyter
;; ==================jupyter===================
(def-package! jupyter
  :bind (("M-g C-j" . swint-jupyter-run-repl)
         ("M-g C-M-j" . jupyter-connect-repl))
  :config
  (bind-key "C-c c" 'jupyter-repl-associate-buffer python-mode-map)
  (defun swint-jupyter-run-repl ()
    "Synchronization of bypy-sync."
    (interactive)
    (unless (equal (bound-and-true-p pyvenv-virtual-env-name) "py3")
      (pyvenv-activate (format "%s/%s" (pyvenv-workon-home) "py3")))
    (call-interactively 'jupyter-run-repl)
    (unless (featurep 'ob-jupyter)
      (add-to-list 'org-babel-load-languages '(jupyter . t) t)
      (org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages)))
  (define-key jupyter-repl-interaction-mode-map (kbd "M-i") nil)
  (define-key jupyter-repl-interaction-mode-map (kbd "C-x C-e") nil)
  (define-key jupyter-repl-interaction-mode-map (kbd "C-c C-/") #'jupyter-inspect-at-point)
  (define-key jupyter-repl-interaction-mode-map (kbd "C-c C-e") #'jupyter-eval-string-command))
;; ==================jupyter===================
(provide 'setup_python)
