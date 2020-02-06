;;; python-mode
;; =================python-mode================
(def-package! python
  :mode ("\\.py\\'" . python-mode)
  :config
  (bind-key "C-M-#" 'run-python)
  (define-key python-mode-map (kbd "C-c C-c") 'python-shell-send-line-or-region)
  (define-key python-mode-map (kbd "C-c C-b") 'python-shell-send-buffer)
  (define-key python-mode-map (kbd "C-c C-e") 'python-shell-send-string)
  (add-hook 'inferior-python-mode-hook 'kill-shell-buffer-after-exit t)
  (defun python-shell-send-line-or-region ()
    (interactive)
    (if (region-active-p)
        (call-interactively 'python-shell-send-region)
      (python-shell-send-region (line-beginning-position) (line-end-position)))))
;; =================python-mode================
;;; pyvenv
;; ===================pyvenv===================
(def-package! pyvenv
  :commands (pyvenv-workon-home pyvenv-activate)
  :bind (("C-x C-M-3" . pyvenv-workon)
         ("C-x C-M-#" . pyvenv-deactivate)
         ("M-s M-p" . swint-python-plot-data))
  :config
  (pyvenv-mode 1)
  ;; 使用pyvenv-activate/deactivate启动/关闭虚拟环境，使用pyvenv-workon列出可用虚拟环境并切换。
  (defalias 'workon 'pyvenv-workon)
  (defun swint-python-plot-data (&optional arg)
    (interactive "P")
    (let (data-string files-list files-string file-exten header-line-string header-line-list columns-list columns-string rows-string column-x-string style-string labels-string fonts-string sizes-string colors-string lines-string markers-string save-string other-args send-other-args)
      (if (memq major-mode '(inferior-python-mode jupyter-repl-mode))
          (setq data-string (read-string "data_x, data_y, label: "))
        (setq files-list (if (eq major-mode 'dired-mode) (dired-get-marked-files) (list (buffer-file-name))))
        (setq files-string (mapconcat 'identity files-list ","))
        (setq file-exten (downcase (file-name-extension (car files-list))))
        (setq header-line-string (shell-command-to-string (format "awk 'NR==1{print}' %s" (car files-list))))
        (setq header-line-list (if (equal file-exten "csv")
                                   (split-string header-line-string "," t "[ \t\n]")
                                 (mapcar 'number-to-string (number-sequence 0 (1- (length (split-string header-line-string "[ \t]+" t "[ \t\n]")))))))
        (setq columns-list
              (helm-comp-read "Columns to plot: " header-line-list
                              :marked-candidates t
                              :buffer "*helm python plot data-swint*"))
        (setq columns-string (mapconcat 'identity columns-list ","))
        (setq rows-string (read-string "Rows: " nil nil ":::"))
        (setq column-x-string (helm-comp-read "Column_x: " (cons "None" header-line-list)
                                              :buffer "*helm python plot data-swint*")))
      (when arg
        (setq style-string (helm-comp-read "Style: " (list "plot" "scatter" "bar")
                                           :buffer "*helm python plot data-swint*"))
        (setq labels-string (read-string "xlabel_str, ylabel_str: "))
        (setq fonts-string (read-string "tick_font, label_font, legend_font (default T,T,T): "))
        (setq sizes-string (read-string "tick_size, label_size, legend_size (default 16,24,16): "))
        (setq colors-string (mapconcat 'identity (helm-comp-read "Colors: " (list "None" "r" "g" "b" "y" "c" "m" "k")
                                                                 :marked-candidates t
                                                                 :buffer "*helm python plot data-swint*") ","))
        (setq lines-string (mapconcat 'identity (helm-comp-read "Lines: " (list "None" "-" "--" "-." ":")
                                                                :marked-candidates t
                                                                :buffer "*helm python plot data-swint*") ","))
        (setq markers-string (mapconcat 'identity (helm-comp-read "Markers: " (list "None" "o" "v" "^" "<" ">" "1" "2" "3" "4" "8" "s" "p" "P" "*" "h" "H" "+" "x" "X" "D" "d" "|" "_")
                                                                  :marked-candidates t
                                                                  :buffer "*helm python plot data-swint*") ","))
        (setq save-string (yes-or-no-p "Save? "))
        (setq other-args (concat (concat " --style " "\"" style-string "\"")
                                 (unless (string= "" labels-string) (concat " --labels " "\"" labels-string "\""))
                                 (unless (string= "" fonts-string) (concat " --fonts " "\"" fonts-string "\""))
                                 (unless (string= "" sizes-string) (concat " --sizes " "\"" sizes-string "\""))
                                 (unless (string= "None" colors-string) (concat " --colors " "\"" colors-string "\""))
                                 (unless (string= "None" lines-string) (concat " --lines " "\"" lines-string "\""))
                                 (unless (string= "None" markers-string) (concat " --markers " "\"" markers-string "\""))
                                 (if save-string (concat " --save "))))
        (setq send-other-args (concat (format ",style='%s'" style-string)
                                      (unless (string= "" labels-string) (format ",labels='%s'" labels-string))
                                      (unless (string= "" fonts-string) (format ",fonts='%s'" fonts-string))
                                      (unless (string= "" sizes-string) (format ",sizes='%s'" sizes-string))
                                      (unless (string= "None" colors-string) (format ",colors='%s'" colors-string))
                                      (unless (string= "None" lines-string) (format ",lines='%s'" lines-string))
                                      (unless (string= "None" markers-string) (format ",markers='%s'" markers-string))
                                      (if save-string ",save=True"))))
      (unless (equal (bound-and-true-p pyvenv-virtual-env-name) "py3")
        (pyvenv-activate (format "%s/%s" (pyvenv-workon-home) "py3")))
      (let ((python-command-string (format "if 'plot_data' not in dir():from sys import path;path.append('%s');import plot_data
plot_data.cli_plot([%s]%s)" (expand-file-name "~/Documents/Python") data-string (or send-other-args ""))))
        (cond ((equal major-mode 'inferior-python-mode)
               (python-shell-send-string python-command-string))
              ((equal major-mode 'jupyter-repl-mode)
               (jupyter-eval-string-command python-command-string))
              (t (if (python-shell-get-process)
                     (python-shell-send-string (format "if 'plot_data' not in dir():from sys import path;path.append('%s');import plot_data
plot_data.file_plot('%s','%s','%s','%s'%s)" (expand-file-name "~/Documents/Python") files-string columns-string rows-string column-x-string (or send-other-args "")))
                   (let* ((plot-data-command (concat "python " (expand-file-name "~/Documents/Python/plot_data.py")
                                                     " -i " "\"" files-string "\""
                                                     " -c " "\"" columns-string "\""
                                                     " -r " "\"" rows-string "\""
                                                     " --column_x " "\"" column-x-string "\""
                                                     other-args))
                          (process (start-process-shell-command "plot-data" "*plot-data*" plot-data-command)))
                     (message "%s" plot-data-command)
                     (message "Data plotting.")
                     (set-process-sentinel
                      process
                      (lambda (process signal)
                        (when (memq (process-status process) '(exit signal))
                          (message "Data plot done."))))))))))))
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
  (setq elpy-shell-starting-directory 'current-directory)
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
  :bind (("M-g 3" . ein:jupyter-server-start)
         ("M-g #" . ein:jupyter-server-stop))
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
  :bind (("M-s 3" . swint-jupyter-run-repl)
         ("M-s #" . jupyter-connect-repl))
  :config
  (bind-key "C-c c" 'jupyter-repl-associate-buffer python-mode-map)
  (defun swint-jupyter-run-repl ()
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
