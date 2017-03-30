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
    (let ((python-shell-interpreter-args "--simple-prompt --pylab"))
      (elpy-shell-switch-to-shell)))
  (defun swint-cpython ()
    (interactive)
    (unless pyvenv-virtual-env
      (call-interactively 'pyvenv-workon))
    (elpy-use-cpython)
    (elpy-shell-switch-to-shell)))
;; ====================elpy====================
;;; emacs-ipython-notebook
;; ====================ein=====================
(use-package ein
  ;; Enabled at at commands.
  :defer t
  :bind ("M-s 3" . swint-ein:notebooklist-open)
  :config
  ;; ein:url-or-port可取8888或http://127.0.0.1(localhost):8888。
  (defun swint-ein:notebooklist-open ()
    (interactive)
    (unless (and (boundp 'pyvenv-virtual-env) pyvenv-virtual-env)
      (call-interactively 'pyvenv-workon))
    (unless (get-process "IPYNB")
      (start-process-shell-command
       "IPYNB" "*IPYNB*" (concat "jupyter notebook --no-browser --notebook-dir="
                                 (expand-file-name "~/Documents/Python/Jupyter"))))
    (unless (if (boundp 'ein:notebooklist-first-open-hook)
                (ein:notebooklist-open)
              (ein:notebooklist-open nil nil t))
      (ein:force-ipython-version-check)))
  ;; (setq ein:use-auto-complete t)
  ;; Enable "superpack" (a little bit hacky improvements).
  (setq ein:use-auto-complete-superpack t)
  (setq ein:use-smartrep t)
  (use-package ein-notebook
    :config
    (add-hook 'ein:notebook-mode-hook '(lambda ()
                                         (set (make-local-variable 'company-idle-delay) nil)))
    (define-key ein:notebook-mode-map (kbd "M-,") nil)
    (define-key ein:notebook-mode-map (kbd "M-.") nil)
    (define-key ein:notebook-mode-map (kbd "C-c C-,") 'ein:pytools-jump-to-source-command)
    (define-key ein:notebook-mode-map (kbd "C-c C-.") 'ein:pytools-jump-back-command)))
(use-package ein-connect
  ;; Enabled at at commands.
  :defer t
  :bind ("M-s #" . swint-ein:connect-to-notebook)
  :config
  ;; ein:connect-to-notebook无法获取notebooks列表。
  (defun swint-ein:connect-to-notebook (&optional arg)
    (interactive "P")
    (let ((ein:connect-default-notebook "8888/Default.ipynb"))
      (if arg
          (ein:connect-to-notebook
           (completing-read "Notebook to connect [URL-OR-PORT/NAME]: "
                            (mapcar #'(lambda (x) (concat "8888/" x))
                                    (directory-files "~/Documents/Python/Jupyter" nil ".+\\.ipynb"))
                            nil t nil nil nil nil))
        (ein:connect-to-default-notebook))))
  ;; (add-hook 'ein:connect-mode-hook 'ein:jedi-setup)
  ;; 在ein:connect中关闭company的自动补全。
  (add-hook 'ein:connect-mode-hook '(lambda ()
                                      (set (make-local-variable 'company-idle-delay) nil)))
  ;; 取消ein:connect-mode-map默认快捷键，以免与elpy冲突。
  (define-key ein:connect-mode-map "\C-c\C-c" nil)
  (define-key ein:connect-mode-map "\C-c\C-l" nil)
  (define-key ein:connect-mode-map "\C-c\C-r" nil)
  (define-key ein:connect-mode-map "\C-c\C-f" nil)
  (define-key ein:connect-mode-map "\C-c\C-i" nil)
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
  (define-key ein:connect-mode-map "\C-cu" 'ein:completer-complete)
  (define-key ein:connect-mode-map "\C-cs" 'ein:notebook-scratchsheet-open)
  (define-key ein:connect-mode-map "\C-ca" 'ein:connect-toggle-autoexec)
  (define-key ein:connect-mode-map "\C-cz" 'ein:connect-pop-to-notebook)
  (define-key ein:connect-mode-map "\C-cx" 'ein:tb-show)
  (define-key ein:connect-mode-map (kbd "C-c ,") 'ein:pytools-jump-to-source-command)
  (define-key ein:connect-mode-map (kbd "C-c .") 'ein:pytools-jump-back-command)
  (define-key ein:connect-mode-map (kbd "C-c /") 'ein:pytools-request-tooltip-or-help))
;; ====================ein=====================
(provide 'setup_python)
