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
  :defer t
  :after python
  :bind (("C-M-3" . swint-ipython)
         ("C-M-#" . swint-cpython)
         ("C-x C-M-3" . pyvenv-workon)
         ("C-x C-M-#" . pyvenv-deactivate))
  :config
  (elpy-enable)
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
  ;; 使用pyvenv-activate/pyvenv-deactivate启动/关闭虚拟环境，使用pyvenv-workon列出可用虚拟环境并切换。
  (defalias 'workon 'pyvenv-workon)
  ;; ipython默认设置有bug，需要加--simple-prompt选项。
  (defun swint-ipython ()
    (interactive)
    (unless pyvenv-virtual-env
      (call-interactively 'pyvenv-workon))
    (elpy-use-ipython)
    (let ((python-shell-interpreter-args "--simple-prompt --pylab"))
      (call-interactively 'elpy-shell-switch-to-shell)))
  (defun swint-cpython ()
    (interactive)
    (unless pyvenv-virtual-env
      (call-interactively 'pyvenv-workon))
    (elpy-use-cpython)
    (call-interactively 'elpy-shell-switch-to-shell)))
;; ====================elpy====================
(provide 'setup_python)
