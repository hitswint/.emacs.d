;;; python-mode
;; =================python-mode================
(use-package python
  ;; Enabled in modes.
  :defer t
  :commands python-mode
  :mode ("\\.py\\'" . python-mode))
;; =================python-mode================
;;; elpy
;; ====================elpy====================
(use-package elpy
  ;; Enabled after features.
  ;; 安装依赖：sudo pip install jedi(or rope) flake8 importmagic autopep8 yapf virtualenv virtualenvwrapper。
  :defer t
  :after python
  :config
  (elpy-enable)
  (add-hook 'inferior-python-mode-hook 'kill-shell-buffer-after-exit t)
  (define-key elpy-mode-map (kbd "M-.") nil)
  (define-key elpy-mode-map (kbd "C-c C-,") 'elpy-goto-definition)
  (define-key elpy-mode-map (kbd "C-c C-.") 'pop-tag-mark)
  (define-key elpy-mode-map (kbd "C-c C-/") 'elpy-doc)
  (define-key inferior-python-mode-map (kbd "C-q") 'comint-send-eof)
  (define-key inferior-python-mode-map (kbd "C-c C-,") 'elpy-goto-definition)
  (define-key inferior-python-mode-map (kbd "C-c C-.") 'pop-tag-mark)
  (define-key inferior-python-mode-map (kbd "C-c C-/") 'elpy-doc)
  ;; 建立虚拟环境：在终端中使用virtualenv [虚拟环境名称]。
  ;; 默认不启用系统已安装packages，使用--system-site-packages启用系统packages。
  ;; 启动虚拟环境：cd ENV ; source ./bin/activate
  ;; 使用pyvenv-activate/pyvenv-deactivate启动/关闭虚拟环境，使用pyvenv-workon列出可用虚拟环境并切换。
  (defalias 'workon 'pyvenv-workon))
;; ====================elpy====================
(provide 'setup_python)
