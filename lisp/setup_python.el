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
  (defvar swint-python-plot-hash (make-hash-table :test 'equal))
  (defun swint-python-plot-data (&optional arg)
    (interactive "P")
    (let (data-string files-list files-string file-exten header-line-string header-line-list columns-list columns-string rows-string column-x-string style-string labels-string fonts-string sizes-string colors-string lines-string markers-string save-string other-args send-other-args)
      (if (memq major-mode '(inferior-python-mode jupyter-repl-mode))
          (setq data-string (puthash "data" (read-string "data_x, data_y, label: " (gethash "data" swint-python-plot-hash)) swint-python-plot-hash))
        (setq files-list (if (eq major-mode 'dired-mode) (dired-get-marked-files) (list (buffer-file-name))))
        (setq files-string (mapconcat 'identity files-list ","))
        (setq file-exten (downcase (file-name-extension (car files-list))))
        (setq header-line-string (shell-command-to-string (format "awk 'NR==1{print}' %s" (car files-list))))
        (setq header-line-list (if (equal file-exten "csv")
                                   (split-string header-line-string "," t "[ \t\n]+")
                                 (mapcar 'number-to-string (number-sequence 0 (1- (length (split-string header-line-string "[ \t]+" t "[ \t\n]+")))))))
        (setq columns-list
              (helm-comp-read "Columns to plot: " header-line-list
                              :marked-candidates t
                              :buffer "*helm python plot data-swint*"))
        (setq columns-string (mapconcat 'identity columns-list ","))
        (setq rows-string (puthash "rows" (read-string "Rows: " (gethash "rows" swint-python-plot-hash) nil "::") swint-python-plot-hash))
        (setq column-x-string (helm-comp-read "Column_x: " (cons "None" header-line-list)
                                              :buffer "*helm python plot data-swint*")))
      (when arg
        (setq style-string (helm-comp-read "Style: " (list "plot" "scatter" "bar")
                                           :buffer "*helm python plot data-swint*"))
        (setq labels-string (puthash "labels" (read-string "xlabel_str, ylabel_str: " (gethash "labels" swint-python-plot-hash)) swint-python-plot-hash))
        (setq fonts-string (puthash "fonts" (read-string "tick_font, label_font, legend_font (default t,t,t): " (gethash "fonts" swint-python-plot-hash)) swint-python-plot-hash))
        (setq sizes-string (puthash "sizes" (read-string "tick_size, label_size, legend_size (default 16,24,16): " (gethash "sizes" swint-python-plot-hash)) swint-python-plot-hash))
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
              (t (if (and (fboundp 'python-shell-get-process) (python-shell-get-process))
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
  (setq elpy-get-info-from-shell t)
  (setq elpy-shell-starting-directory 'current-directory)
  ;; 使用ipython作为交互环境。
  (setq python-shell-interpreter "ipython"
        python-shell-interpreter-args "-i --simple-prompt --pylab")
  ;; 使用jupyter作为交互环境。
  ;; (setq python-shell-interpreter "jupyter"
  ;;       python-shell-interpreter-args "console --simple-prompt"
  ;;       python-shell-prompt-detect-failure-warning nil)
  ;; (add-to-list 'python-shell-completion-native-disabled-interpreters "jupyter")
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
;;; jedi
;; ===================jedi=====================
(def-package! jedi
  ;; 使用jedi:install-server安装服务端。
  :commands jedi:setup
  :init
  (add-hook 'python-mode-hook '(lambda ()
                                 (unless (equal (bound-and-true-p pyvenv-virtual-env-name) "py3")
                                   (pyvenv-activate (format "%s/%s" (pyvenv-workon-home) "py3")))
                                 (jedi:setup)))
  :config
  (setq jedi:complete-on-dot t)
  (setq jedi:install-imenu t)
  (setq jedi:tooltip-method nil)
  (add-hook 'jedi-mode-hook '(lambda ()
                               (set (make-local-variable 'company-idle-delay) nil)))
  (define-key jedi-mode-map (kbd "C-c C-o") 'jedi:get-in-function-call)
  (define-key jedi-mode-map (kbd "M-u") 'jedi:complete)
  (define-key jedi-mode-map (kbd "C-c C-/") 'jedi:show-doc)
  (define-key jedi-mode-map (kbd "C-c C-,") 'jedi:goto-definition)
  (define-key jedi-mode-map (kbd "C-c C-.") 'jedi:goto-definition-pop-marker)
  (define-key jedi-mode-map (kbd "<C-tab>") nil)
  (define-key jedi-mode-map (kbd "C-c ?") nil)
  (define-key jedi-mode-map (kbd "C-c .") nil)
  (define-key jedi-mode-map (kbd "C-c ,") nil))
;; ===================jedi=====================
(provide 'setup_python)
