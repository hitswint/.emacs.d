;;; python-mode
;; =================python-mode================
(use-package python
  ;; Enabled in modes.
  :defer t
  :mode ("\\.py\\'" . python-mode))
;; =================python-mode================
;;; elpy
;; ====================elpy====================
(use-package elpy
  ;; Enabled after features.
  :defer t
  :after python
  :bind (("C-M-3" . swint-ipython)
         ("C-M-#" . swint-cpython)
         ("C-x C-M-3" . pyvenv-workon)
         ("C-x C-M-#" . pyvenv-deactivate))
  :config
  (elpy-enable)
  (add-hook 'python-mode-hook '(lambda ()
				 (add-hook 'before-save-hook 'elpy-format-code nil t)))
  (add-hook 'inferior-python-mode-hook 'kill-shell-buffer-after-exit t)
  (define-key elpy-mode-map (kbd "M-.") nil)
  (define-key elpy-mode-map (kbd "C-c C-c") '(lambda () (interactive) (elpy-use-ipython)
                                               (let ((python-shell-interpreter-args "--simple-prompt --pylab"))
                                                 (call-interactively 'elpy-shell-send-region-or-buffer))))
  (define-key elpy-mode-map (kbd "C-c C-,") 'elpy-goto-definition)
  (define-key elpy-mode-map (kbd "C-c C-.") 'pop-tag-mark)
  (define-key elpy-mode-map (kbd "C-c C-/") 'elpy-doc)
  (define-key inferior-python-mode-map (kbd "C-q") 'comint-send-eof)
  (define-key inferior-python-mode-map (kbd "C-c C-,") 'elpy-goto-definition)
  (define-key inferior-python-mode-map (kbd "C-c C-.") 'pop-tag-mark)
  (define-key inferior-python-mode-map (kbd "C-c C-/") 'elpy-doc)
  ;; 使用pyvenv-activate/deactivate启动/关闭虚拟环境，使用pyvenv-workon列出可用虚拟环境并切换。
  (defalias 'workon 'pyvenv-workon)
  ;; ipython默认设置有bug，需要加--simple-prompt选项。
  (defun swint-ipython ()
    (interactive)
    (unless pyvenv-virtual-env
      (call-interactively 'pyvenv-workon))
    (elpy-use-ipython)
    (setq python-shell-interpreter-args "--simple-prompt --pylab")
    (call-interactively 'elpy-shell-switch-to-shell))
  (defun swint-cpython ()
    (interactive)
    (unless pyvenv-virtual-env
      (call-interactively 'pyvenv-workon))
    (elpy-use-cpython)
    (setq python-shell-interpreter-args "-i")
    (call-interactively 'elpy-shell-switch-to-shell)))
;; ====================elpy====================
;;; emacs-ipython-notebook
;; ====================ein=====================
(use-package ein
  ;; Enabled at at commands.
  :defer t
  :bind ("M-s 3" . swint-ein:notebooklist-open)
  :config
  ;; (setq ein:use-auto-complete t)
  ;; Enable "superpack" (a little bit hacky improvements).
  (setq ein:use-auto-complete-superpack t)
  ;; (add-hook 'ein:connect-mode-hook 'ein:jedi-setup)
  (setq ein:use-smartrep t)
  (defun swint-ein:notebooklist-open ()
    (interactive)
    (unless (and (boundp 'pyvenv-virtual-env) pyvenv-virtual-env)
      (call-interactively 'pyvenv-workon))
    (unless (get-process "IPYNB")
      (start-process-shell-command
       "IPYNB" "*IPYNB*" "jupyter notebook --no-browser"))
    (ein:notebooklist-open))
  (use-package ein-notebook
    :config
    (define-key ein:notebook-mode-map (kbd "M-,") nil)
    (define-key ein:notebook-mode-map (kbd "M-.") nil)
    (define-key ein:notebook-mode-map (kbd "C-c C-,") 'ein:pytools-jump-to-source-command)
    (define-key ein:notebook-mode-map (kbd "C-c C-.") 'ein:pytools-jump-back-command)))
(use-package ein-connect
  ;; Enabled at at commands.
  :defer t
  :bind ("M-s #" . ein:connect-to-notebook-buffer)
  :config
  (define-key ein:connect-mode-map (kbd "M-,") nil)
  (define-key ein:connect-mode-map (kbd "M-.") nil)
  (define-key ein:connect-mode-map (kbd "C-c C-,") 'ein:pytools-jump-to-source-command)
  (define-key ein:connect-mode-map (kbd "C-c C-.") 'ein:pytools-jump-back-command))
;; ====================ein=====================
(provide 'setup_python)
