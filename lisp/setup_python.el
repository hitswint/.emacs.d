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
         ("M-o p" . swint-python-plot-data)
         ("M-o P" . swint-python-fig-config)
         ("M-o M-p" . swint-python-load-file))
  :config
  (define-key minibuffer-local-map (kbd "C-c m") 'swint-python-insert-data)
  (define-key minibuffer-local-map (kbd "C-c M") 'swint-python-insert-variables)
  (pyvenv-mode 1)
  ;; 使用pyvenv-activate/deactivate启动/关闭虚拟环境，使用pyvenv-workon列出可用虚拟环境并切换。
  (defalias 'workon 'pyvenv-workon)
  (require 'ht)
  (defvar swint-python-plot-hash (ht ("data" "")
                                     ("rows" "::")
                                     ("row_x" "::")
                                     ("labels" "x,y")
                                     ("fonts" "t,t,t")
                                     ("sizes" "48,56,48")
                                     ;; 可直接输入用逗号分割的colors/lines/markers。
                                     ("colors" (nconc (list "r" "g" "b" "y" "c" "m" "k" "C0" "C1" "C2" "C3" "C4" "C5" "C6" "C7" "C8" "C9")
                                                      (cl-remove-if (lambda (x)
                                                                      (string-match "[ \t\n\r]+" x))
                                                                    (defined-colors))))
                                     ("lines" (list "-" "--" "-." ":"))
                                     ("markers" (list "o" "v" "^" "<" ">" "1" "2" "3" "4" "8" "s" "p" "P" "*" "h" "H" "+" "x" "X" "D" "d" "|" "_"))))
  (defun swint-python-load-file ()
    (interactive)
    (let ((file-name (if (eq major-mode 'dired-mode) (dired-get-filename) (buffer-file-name))))
      (if (and (fboundp 'python-shell-get-process) (python-shell-get-process))
          (let ((file-base-name (file-name-base file-name)))
            (if (member (ignore-errors (downcase (file-name-extension file-name))) (list "xls" "xlsx"))
                (python-shell-send-string
                 (format "
if 'pd' not in dir():
    import pandas as pd
if 'builtins' not in dir():
    import builtins
def is_number(s):
    try:
        float(s)
    except:
        return False
    return True
names=locals()
dict_%s=pd.read_excel('%s', sheet_name=None, header=0, comment='#')
for k,v in dict_%s.items():
    if not v.empty:
        # 当第1行都是数字时，将原来读入的header变为第1行数据，使用0.1.2作为columns
        names['df_%s_'+k]=v.T.reset_index().T if builtins.all(is_number(ele) for ele in v.columns) else v
        # 或使用：
        # df = df.columns.to_frame().T.append(df, ignore_index=True)
        # df.columns = range(len(df.columns))
" file-base-name (expand-file-name file-name) file-base-name file-base-name))
              (let ((header-line-string (shell-command-to-string (format "awk '!/^($|#)/' %s | awk 'NR==1{printf $0}'" file-name)))) ;先排除#注释行再返回无回车的第1行
                (python-shell-send-string
                 (format "if not set(['pd', 'builtins']) < set(dir()):import pandas as pd;import builtins;
exec(\"def is_number(s):\\n try:  float(s)\\n except:  return False\\n return True\")
df_%s=pd.read_csv('%s', header=None if builtins.all(is_number(ele) for ele in '%s'.split()) else 'infer', sep='%s', skipinitialspace=True, comment='#')"
                         file-base-name
                         (expand-file-name file-name) header-line-string
                         (if (string= (ignore-errors (downcase (file-name-extension file-name))) "csv") "," "\\\\s+"))))))
        (message "No python process found!" ))))
  (defun swint-python-insert-variables ()
    (interactive)
    (cl-assert (minibuffer-window-active-p (selected-window)) nil
               "Error: Attempt to use minibuffer history outside a minibuffer")
    (let ((enable-recursive-minibuffers t)
          (elm (mapconcat 'identity (helm-comp-read "Variables: " (split-string (python-shell-send-string-no-output "%who")
                                                                                "[ \t]+" t "[ \t\n]+")
                                                    :marked-candidates t
                                                    :buffer "*helm python plot data-swint*")
                          ",")))
      (insert elm)))
  (defun swint-python-insert-data ()
    (interactive)
    (cl-assert (minibuffer-window-active-p (selected-window)) nil
               "Error: Attempt to use minibuffer history outside a minibuffer")
    (let ((df-list (split-string (python-shell-send-string-no-output "%who") "[ \t]+" t "[ \t\n]+")))
      (cl-flet ((df-read (&optional is-x-p)
                         (let ((df-list (helm-comp-read (if is-x-p "x: " "y: ") (if is-x-p (cons "None" df-list) df-list)
                                                        :marked-candidates t
                                                        :buffer "*helm python plot data-swint*")))
                           (if is-x-p
                               (car df-list)
                             df-list)))
                (columns-read (df &optional is-x-p)
                              (let ((column-list (helm-comp-read (if is-x-p "Column as x: " "Columns to plot: ")
                                                                 (cons (if is-x-p "None" "")
                                                                       (split-string (python-shell-send-string-no-output (format "\",\".join(%s.columns)" df))
                                                                                     "," t "[ \t\n']+"))
                                                                 :marked-candidates t
                                                                 :buffer "*helm python plot data-swint*")))
                                (if is-x-p
                                    (car column-list)
                                  column-list))))
        (let* ((enable-recursive-minibuffers t)
               (df-y (df-read))
               (columns (columns-read (car df-y)))
               (df-x-pre (df-read t))
               (df-x (unless (equal df-x-pre "None") df-x-pre))
               (column-x (columns-read (or df-x (car df-y)) t)))
          (cl-loop for df in df-y do
                   (cl-loop for column in columns do
                            (insert (if (equal column-x "None") ""
                                      (format "%s.loc[:, '%s'], " (or df-x df) column-x))
                                    (format "%s.loc[:, '%s'], " df column)
                                    (format "'%s', " column))))))))
  (defun swint-python-fig-config ()
    (interactive)
    (if (and (fboundp 'python-shell-get-process) (python-shell-get-process))
        (let* ((config-list (helm-comp-read "Configs: " (list "tick_font" "tick_size" "label_font" "label_size" "legend_font" "legend_size" "legend_ncol")
                                            :marked-candidates t
                                            :buffer "*helm python fig config-swint*"))
               (args-list (cl-loop for x in config-list
                                   collect (format "%s='%s'" x (read-string (concat x ": ")))))
               (args-string (mapconcat 'identity args-list ",")))
          (when args-string
            (python-shell-send-string (format "if 'plot_data' not in dir():from sys import path;path.append('%s');import plot_data
plot_data.fig_config(%s)" (expand-file-name "~/Documents/Python") args-string))))
      (message "No python process found!")))
  (defun swint-python-plot-data (&optional arg)
    (interactive "P")
    (let (data-string files-list files-string columns-string file-x-string column-x-string style-string save-string shell-command-args send-string-args)
      (cl-flet ((plot-file-setup (data-file &optional is-x-p)
                                 (let* ((header-line-string (shell-command-to-string (format "awk '!/^($|#)/' %s | awk 'NR==1{print}'" data-file)))
                                        (header-line-list (if (equal (ignore-errors (downcase (file-name-extension data-file))) "csv")
                                                              (split-string header-line-string "," t "[ \t\n]+")
                                                            (if (string-match "\"" header-line-string) ;非csv文件的首行用("x" "y" "z")
                                                                (split-string header-line-string "\"" t "[ \t\n]+")
                                                              (mapcar 'number-to-string (number-sequence 0 (1- (length (split-string header-line-string "[ \t]+" t "[ \t\n]+"))))))))
                                        (columns-list (helm-comp-read (if is-x-p "Column as x: " "Columns to plot: ") (cons (if is-x-p "None" "") header-line-list)
                                                                      :marked-candidates t
                                                                      :buffer "*helm python plot data-swint*")))
                                   (mapconcat 'identity columns-list ","))))
        (setq style-string (helm-comp-read "Style: " (list "plot" "stackplot" "step" "scatter" "bar" "barh" "stackbar" "stackbarh" "pie" "boxplot" "boxploth" "contourf")
                                           :buffer "*helm python plot data-swint*"))
        (if (memq major-mode '(inferior-python-mode jupyter-repl-mode))
            (setq data-string (puthash "data" (read-string "data_x, data_y, label: " (gethash "data" swint-python-plot-hash)) swint-python-plot-hash))
          (setq files-list (if (eq major-mode 'dired-mode) (dired-get-marked-files) (list (buffer-file-name))))
          (setq files-string (mapconcat 'identity files-list ","))
          (setq columns-string (plot-file-setup (car files-list)))
          (setq file-x-string (if arg
                                  (helm-comp-read "File as x: " (cons "None" (directory-files default-directory nil directory-files-no-dot-files-regexp))
                                                  :buffer "*helm python plot data-swint*")
                                "None"))
          (setq column-x-string (plot-file-setup (if (string= file-x-string "None") (car files-list) file-x-string) t)))
        (when arg
          (let* ((config-list (helm-comp-read "Configs: " (list "rows" "row_x" "labels" "fonts" "sizes" "colors" "lines" "markers")
                                              :marked-candidates t
                                              :buffer "*helm python fig config-swint*"))
                 (args-alist (cl-loop for x in config-list
                                      collect (cons x (if (listp (gethash x swint-python-plot-hash))
                                                          (mapconcat 'identity (helm-comp-read (concat x ": ") (gethash x swint-python-plot-hash)
                                                                                               :marked-candidates t
                                                                                               :buffer (concat "*helm python plot data-swint*")) ",")
                                                        (puthash x (read-string (concat x ": ") (gethash x swint-python-plot-hash)) swint-python-plot-hash)))))
                 (save-string (yes-or-no-p "Save? ")))
            (setq shell-command-args (concat (mapconcat #'(lambda (x)
                                                            (format "--%s \"%s\"" (car x) (cdr x))) args-alist " ")
                                             (if save-string " --save ")))
            (setq send-string-args (concat (mapconcat #'(lambda (x)
                                                          (format "%s='%s'" (car x) (cdr x))) args-alist ",")
                                           (if save-string ",save=True")))))
        (unless (equal (bound-and-true-p pyvenv-virtual-env-name) "py3")
          (pyvenv-activate (format "%s/%s" (pyvenv-workon-home) "py3")))
        (let ((python-command-string (format "if 'plot_data' not in dir():from sys import path;path.append('%s');import plot_data
plot_data.cli_plot([%s],'%s', %s)" (expand-file-name "~/Documents/Python") data-string style-string (or send-string-args ""))))
          (cond ((equal major-mode 'inferior-python-mode)
                 (python-shell-send-string python-command-string))
                ((equal major-mode 'jupyter-repl-mode)
                 (jupyter-eval-string-command python-command-string))
                (t (if (and (fboundp 'python-shell-get-process) (python-shell-get-process))
                       (python-shell-send-string (format "if 'plot_data' not in dir():from sys import path;path.append('%s');import plot_data
plot_data.file_plot('%s','%s','%s','%s','%s', %s)" (expand-file-name "~/Documents/Python") files-string columns-string file-x-string column-x-string style-string (or send-string-args "")))
                     (let* ((plot-data-command (concat "python " (expand-file-name "~/Documents/Python/plot_data.py")
                                                       " -i " "\"" files-string "\""
                                                       " -y " "\"" columns-string "\""
                                                       " -I " "\"" file-x-string "\""
                                                       " -x " "\"" column-x-string "\""
                                                       " -s " "\"" style-string "\" "
                                                       shell-command-args))
                            (process (start-process-shell-command "plot-data" "*plot-data*" plot-data-command)))
                       (message "Data plotting.")
                       (set-process-sentinel
                        process
                        (lambda (process signal)
                          (when (memq (process-status process) '(exit signal))
                            (message "Data plot done.")))))))))))))
;; ===================pyvenv===================
;;; elpy
;; ====================elpy====================
(def-package! elpy
  :diminish elpy-mode
  :commands (elpy-shell-switch-to-shell toggle-elpy-mode-all-buffers)
  :init
  (bind-key "C-M-3" 'elpy-shell-switch-to-shell)
  (add-hook 'python-mode-hook (lambda ()
                                (bind-key "C-c e" 'toggle-elpy-mode-all-buffers python-mode-map)))
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
  :commands jedi:get-in-function-call
  :init
  (add-hook 'python-mode-hook (lambda ()
                                (bind-key "C-c j" 'jedi:get-in-function-call python-mode-map)))
  :config
  (setq jedi:complete-on-dot t
        jedi:install-imenu t
        jedi:tooltip-method nil
        jedi:get-in-function-call-delay 2000
        jedi:get-in-function-call-timeout 5000)
  (add-hook 'jedi-mode-hook '(lambda ()
                               (set (make-local-variable 'company-idle-delay) nil)))
  (define-key jedi-mode-map (kbd "C-c j") 'jedi:get-in-function-call)
  (define-key jedi-mode-map (kbd "M-u") 'jedi:complete)
  (define-key jedi-mode-map (kbd "C-c C-/") 'jedi:show-doc)
  (define-key jedi-mode-map (kbd "C-c C-,") 'jedi:goto-definition)
  (define-key jedi-mode-map (kbd "C-c C-.") 'jedi:goto-definition-pop-marker)
  (define-key jedi-mode-map (kbd "<C-tab>") nil)
  (define-key jedi-mode-map (kbd "C-c ?") nil)
  (define-key jedi-mode-map (kbd "C-c .") nil)
  (define-key jedi-mode-map (kbd "C-c ,") nil)
  (advice-add 'jedi:get-in-function-call :before #'(lambda ()
                                                     (unless (equal (bound-and-true-p pyvenv-virtual-env-name) "py3")
                                                       (pyvenv-activate (format "%s/%s" (pyvenv-workon-home) "py3")))
                                                     (unless jedi-mode
                                                       (dolist (buf (cl-remove-if-not (lambda (x)
                                                                                        (equal (buffer-mode x) 'python-mode))
                                                                                      (buffer-list)))
                                                         (with-current-buffer buf
                                                           (call-interactively 'jedi:setup)))
                                                       (add-hook 'python-mode-hook 'jedi:setup)))))
;; ===================jedi=====================
(provide 'setup_python)
