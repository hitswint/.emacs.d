;;; python-mode
;; =================python-mode================
(use-package python
  :delight "Py"
  :mode ("\\.py\\'" . python-mode)
  :config
  (bind-key "M-o M-P" 'run-python)
  (define-key python-mode-map (kbd "C-c C-c") 'python-shell-send-line-or-region)
  (define-key python-mode-map (kbd "C-c C-b") 'python-shell-send-buffer)
  (define-key python-mode-map (kbd "C-c C-e") 'python-shell-send-string)
  (add-hook 'python-mode-hook #'(lambda ()  ;参考elpy-module-sane-defaults
                                  (setq forward-sexp-function nil)  ;默认python-nav-forward-sexp
                                  (setq comment-inline-offset 2)))
  (add-hook 'inferior-python-mode-hook 'kill-shell-buffer-after-exit t)
  (defun python-shell-send-line-or-region ()
    (interactive)
    (if (region-active-p)
        (call-interactively 'python-shell-send-region)
      (python-shell-send-region (line-beginning-position) (line-end-position)))))
;; =================python-mode================
;;; pyvenv
;; ===================pyvenv===================
(use-package pyvenv
  :commands (pyvenv-workon-home pyvenv-activate-py3)
  :bind (("M-o C-M-p" . pyvenv-workon)
         ("M-o C-M-S-p" . pyvenv-deactivate)
         ("M-o p" . swint-python-plot-data)
         ("M-o P" . swint-python-fig-config)
         ("M-o C-p" . swint-python-load-file)
         ("M-o C-S-p" . swint-python-load-mysql))
  :config
  (define-key minibuffer-local-map (kbd "C-c i") 'swint-python-insert-data)
  (define-key minibuffer-local-map (kbd "C-c I") 'swint-python-insert-variables)
  (pyvenv-mode 1)
  ;; 使用pyvenv-activate/deactivate启动/关闭虚拟环境，使用pyvenv-workon列出可用虚拟环境并切换
  (defalias 'workon 'pyvenv-workon)
  (require 'ht)
  (defun pyvenv-activate-py3 ()
    (unless (equal (bound-and-true-p pyvenv-virtual-env-name) "py3")
      (pyvenv-activate (format "%s/%s" (pyvenv-workon-home) "py3"))))
  (defvar swint-python-plot-hash (ht ("data" "")
                                     ("rows" "::")
                                     ("labels" "x,y")
                                     ("fonts" "st,st,st")
                                     ("sizes" "48,56,48")
                                     ;; 可直接输入用逗号分割的colors/lines/markers
                                     ("colors" (nconc (list "r" "g" "b" "y" "c" "m" "k" "C0" "C1" "C2" "C3" "C4" "C5" "C6" "C7" "C8" "C9")
                                                      (cl-remove-if (lambda (x)
                                                                      (string-match "[ \t\n\r]+" x))
                                                                    (defined-colors))))
                                     ("lines" (list "-" "--" "-." ":"))
                                     ("markers" (list "o" "v" "^" "<" ">" "1" "2" "3" "4" "8" "s" "p" "P" "*" "h" "H" "+" "x" "X" "D" "d" "|" "_"))
                                     ("hatchs" (list "/" "\\" "|" "-" "+" "x" "o" "O" "." "*" "//" "\\\\\\\\" "||" "--" "++" "xx" "oo" "OO" ".." "**"))
                                     ("polyfit" (list "1" "2" "3" "4" "5"))))
  (defvar swint-python-plot-exec-path (expand-file-name "~/Documents/Python/plot"))
  (defun string-numberp (s)
    (condition-case invalid-read-syntax
        (numberp (read s))
      (error nil)))
  (defun swint-python-load-file ()
    (interactive)
    (let ((file-name (if (eq major-mode 'dired-mode) (dired-get-filename) (buffer-file-name))))
      (if (and (fboundp 'python-shell-get-process) (python-shell-get-process))
          (let ((file-base-name (file-name-base file-name)))
            (cond
             ;; 导入Excel文件
             ((member (ignore-errors (downcase (file-name-extension file-name))) (list "xls" "xlsx"))
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
names['dict_'+re.sub('\\W','_','%s')]=pd.read_excel('%s', sheet_name=None, header=0, comment='#')
for k,v in names['dict_'+re.sub('\\W','_','%s')].items():
    if not v.empty:
        # 当第1行都是数字时，将原来读入的header变为第1行数据，使用0.1.2作为columns
        # 使用re.sub('\W|^(?=\d)', '_', filename)将文件名转为合法的变量名，缺点是首字母是数字时加下划线
        names['df_'+re.sub('\\W','_','%s_')+re.sub('\\W','_',k)]=v.T.reset_index().T if builtins.any(is_number(ele) for ele in v.columns) else v
        # 或使用：
        # df = df.columns.to_frame().T.append(df, ignore_index=True)
        # df.columns = range(len(df.columns))
" file-base-name (expand-file-name file-name) file-base-name file-base-name)))
             ;; 导入sqlite文件
             ;; sql-sqlite(进入命令行) / sqlite-mode-open-file(展示表格)
             ((member (ignore-errors (downcase (file-name-extension file-name))) '("db" "sdb" "sqlite" "db3" "s3db" "sqlite3" "sl3" "db2" "s2db" "sqlite2" "sl2"))
              (let ((table (helm-comp-read "Table to select: "
                                           (split-string (shell-command-to-string (format "sqlite3 \"%s\" \".table\"" (expand-file-name file-name)))
                                                         "[ \t\n]" t "[ \t\n]+")
                                           :buffer "*helm python plot data-swint*")))
                (python-shell-send-string
                 (format "
if 'pd' not in dir():
    import pandas as pd
if 'sql' not in dir():
    import sqlalchemy as sql
conn = sql.create_engine('sqlite:///%s')
names=locals()
names['df_'+re.sub('\\W','_','%s_')+re.sub('\\W','_','%s')] = pd.read_sql('%s', conn)
" (expand-file-name file-name) file-base-name table table))))
             ;; 需修改esoreader.py源文件，data = {v[1:]: self.data[self.dd.index[v]] for v in variables} -> data = {':'.join(filter(None, v[1:])): self.data[self.dd.index[v]] for v in variables}
             ((member (ignore-errors (downcase (file-name-extension file-name))) '("eso" "mtr"))
              (python-shell-send-string
               (format "
if 'pd' not in dir():
    import pandas as pd
if 'esoreader' not in dir():
    import esoreader
esofile=esoreader.read_from_path('%s')
names=locals()
for freq in ['TimeStep', 'Hourly', 'Daily', 'Monthly', 'Annual', 'RunPeriod']:
    if esofile.find_variable('', frequency=freq):
        names['df_'+re.sub('\\W','_','%s_')+freq] = esofile.to_frame('', frequency=freq)
" (expand-file-name file-name) file-base-name)))
             ;; 导入epw文件
             ((member (ignore-errors (downcase (file-name-extension file-name))) '("epw"))
              (python-shell-send-string
               (format "
if 'pd' not in dir():
    import pandas as pd
if 'epw' not in dir():
    from epw import epw
names=locals()
names['epw_'+re.sub('\\W','_','%1$s')] = epw()
names['epw_'+re.sub('\\W','_','%1$s')].read(r'%2$s')
names['df_'+re.sub('\\W','_','%1$s')] = names['epw_'+re.sub('\\W','_','%1$s')].dataframe
names['df_'+re.sub('\\W','_','%1$s')]['Time'] = names['df_'+re.sub('\\W','_','%1$s')]['Year'].map(str)
for x in ['Month', 'Day']:
    # 增加Time列，将月/日补零
    names['df_'+re.sub('\\W','_','%1$s')]['Time'] += '-' + names['df_'+re.sub('\\W','_','%1$s')][x].map(lambda x: str(x).zfill(2))
# 处理24:00:00时间转换错误：ParserError: hour must be in 0..23
names['df_'+re.sub('\\W','_','%1$s')]['Time'] = pd.to_datetime(names['df_'+re.sub('\\W','_','%1$s')]['Time']) + pd.to_timedelta(names['df_'+re.sub('\\W','_','%1$s')]['Hour'].map(str) + ':' + names['df_'+re.sub('\\W','_','%1$s')]['Minute'].map(str) + ':00')
" file-base-name (expand-file-name file-name))))
             ;; 导入py文件
             ((member (ignore-errors (downcase (file-name-extension file-name))) '("py" "pyc"))
              (python-shell-send-file file-name))
             ;; 导入csv文件
             ((member (ignore-errors (downcase (file-name-extension file-name))) '("csv"))
              (python-shell-send-string
               (format "
if 'pd' not in dir():
    import pandas as pd
names=locals()
names['df_'+re.sub('\\W','_','%s')]=pd.read_csv('%s', skipinitialspace=True, comment='#')
names['df_'+re.sub('\\W','_','%s')].columns = names['df_'+re.sub('\\W','_','%s')].columns.str.strip()
" file-base-name (expand-file-name file-name) file-base-name file-base-name)))
             ;; 导入其他文件
             (t (let ((header-line-string (shell-command-to-string (format "awk '!/^($|#)/' '%s' | awk 'NR==1{printf $0}'" file-name)))) ;先排除#注释行再返回无回车的第1行
                  (python-shell-send-string
                   (format "
if not set(['pd', 'builtins']) < set(dir()):import pandas as pd;import builtins;
exec(\"def is_number(s):\\n try:  float(s)\\n except:  return False\\n return True\")
names=locals()
names['df_'+re.sub('\\W','_','%s')]=pd.read_csv('%s', header=None if builtins.any(is_number(ele) for ele in '%s'.split()) else 'infer', sep='%s', skipinitialspace=True, comment='#')
" file-base-name (expand-file-name file-name) header-line-string "\\\\s+"))))))
        (message "No python process found!" ))))
  (defun swint-python-load-mysql ()
    (interactive)
    (if (and (fboundp 'python-shell-get-process) (python-shell-get-process))
        (let* ((user (get-auth-user "mysql"))
               (pass (get-auth-pass "mysql"))
               (database (helm-comp-read "Database to select: "
                                         (cdr (split-string (shell-command-to-string (format "mysql --user=%s --password=%s -e \"show databases;\"" user pass))
                                                            "\n" t "[ \t\n]+"))
                                         :buffer "*helm python plot data-swint*"))
               (table (helm-comp-read "Table to select: "
                                      (cdr (split-string (shell-command-to-string (format "mysql --user=%s --password=%s -e \"show tables;\" %s" user pass database))
                                                         "\n" t "[ \t\n]+"))
                                      :buffer "*helm python plot data-swint*")))
          (python-shell-send-string
           (format "
if 'pd' not in dir():
    import pandas as pd
if 'sql' not in dir():
    import sqlalchemy as sql
conn = sql.create_engine('mysql+pymysql://%s:%s@localhost:3306/%s?charset=utf8')
names=locals()
names['df_'+re.sub('\\W','_','%s_')+re.sub('\\W','_','%s')] = pd.read_sql('%s', conn)
" user pass database database table table)))
      (message "No python process found!")))
  (defun swint-python-insert-variables ()
    (interactive)
    (cl-assert (minibuffer-window-active-p (selected-window)) nil
               "Error: Attempt to use minibuffer history outside a minibuffer")
    (let ((enable-recursive-minibuffers t)
          (elm (mapconcat 'identity (helm-comp-read "Variables: " (split-string (python-shell-send-string-no-output
                                                                                 "get_ipython().run_line_magic(\"who\", \"\")")
                                                                                "[ \t]+" t "[ \t\n]+")
                                                    :marked-candidates t
                                                    :buffer "*helm python plot data-swint*")
                          ",")))
      (insert elm)))
  (defun swint-python-insert-data ()
    (interactive)
    (cl-assert (minibuffer-window-active-p (selected-window)) nil
               "Error: Attempt to use minibuffer history outside a minibuffer")
    (let ((df-list (split-string (python-shell-send-string-no-output
                                  "get_ipython().run_line_magic(\"who\", \"\")")
                                 "[ \t]+" t "[ \t\n]+")))
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
        (let* ((config-list (helm-comp-read "Configs: " (list "xlabel" "ylabel" "fonts" "sizes" "time_formatter" "legend_ncol" "text" "annotate" "axline" "patch" "remove")
                                            :marked-candidates t
                                            :buffer "*helm python fig config-swint*"))
               (args-list (cl-loop for x in config-list
                                   collect (cond
                                            ((equal x "text")
                                             (concat (format "text='%s'" (read-string "text($\\\\alpha$): "))
                                                     (format ",text_config='%s'" (read-string "text_config(x,y): "))
                                                     (format ",text_config_optional='%s'" (read-string "text_config_optional: " "56,s,k"))))
                                            ((equal x "annotate")
                                             (concat (format "annotate='%s'" (read-string "annotate($\\\\alpha$): "))
                                                     (format ",annotate_config='%s'" (read-string "annotate_config(x,y,x[text],y[text]): "))
                                                     (format ",annotate_config_optional='%s'" (read-string "annotate_config_optional: "
                                                                                                           "56,s,k,arrowstyle=\"simple\",connectionstyle=\"arc3\",edgecolor=\"k\",facecolor=\"w\",fill=False,linestyle=\"-\",linewidth=2"))))
                                            ((equal x "axline")
                                             (concat (format "axline='%s'" (read-string "axline(h/v): "))
                                                     (format ",axline_config='%s'" (read-string "axline_config(coordinate): "))
                                                     (format ",axline_config_optional='%s'" (read-string "axline_config_optional: " "-,k"))))
                                            ((equal x "patch")
                                             (let ((patch (helm-comp-read "Patch: " (list "Circle" "Rectangle" "Ellipse" "RegularPolygon")
                                                                          :buffer "*helm python plot data-swint*")))
                                               (concat (format "patch='%s'" patch)
                                                       (format ",patch_config='%s'"
                                                               (cond ((equal patch "Circle")
                                                                      (read-string "Circle(x,y,R): "))
                                                                     ((equal patch "Rectangle")
                                                                      (read-string "Rectangle(x,y,w,h,angle=0): "))
                                                                     ((equal patch "Ellipse")
                                                                      (read-string "Ellipse(x,y,w,h,angle=0): "))
                                                                     ((equal patch "RegularPolygon")
                                                                      (read-string "RegularPolygon(x,y,n,R,orientation=0): "))))
                                                       (format ",patch_config_optional='%s'" (read-string "patch_config_optional: " "edgecolor=\"k\",facecolor=\"w\",fill=False,linestyle=\"-\",linewidth=2")))))
                                            ((equal x "remove")
                                             (format "remove='%s'" (helm-comp-read "Remove: " (list "text" "annotate" "axline" "patch")
                                                                                   :buffer "*helm python plot data-swint*")))
                                            ((equal x "time_formatter")
                                             (concat (format "time_locator='%s'" (read-string "time_locator(Second/Minute/Hour/Day/Weekday/Month/Year/Auto,rotation): " "Auto,0"))
                                                     (format ",time_formatter='%s'" (read-string "time_formatter: " "%Y/%m/%d %H:%M:%S"))))
                                            (t
                                             (format "%s='%s'" x (read-string (concat x ": ")))))))
               (args-string (mapconcat 'identity args-list ",")))
          (when args-string
            (python-shell-send-string (format "if 'plot_data' not in dir():from sys import path;path.append('%s');import plot_data
plot_data.fig_config(%s)" swint-python-plot-exec-path args-string))))
      (message "No python process found!")))
  (defun swint-python-plot-data ()
    (interactive)
    (let (data-string files-list files-string columns-string file-x-string column-x-string style-string shell-command-args send-string-args)
      (cl-flet ((plot-file-setup (data-file &optional is-x-p)
                  (let* ((header-line-string (shell-command-to-string (format "awk '!/^($|#)/' '%s' | awk 'NR==1{print}'" data-file)))
                         (header-line-list (if (equal (ignore-errors (downcase (file-name-extension data-file))) "csv")
                                               (split-string header-line-string "," t "[ \t\n]+")
                                             (if (string-match "\"" header-line-string) ;非csv文件的首行用("x" "y" "z")
                                                 (split-string header-line-string "\"" t "[ \t\n]+")
                                               (let ((header-list (split-string header-line-string "[ \t]+" t "[ \t\n]+")))
                                                 (if (cl-loop for ele in header-list
                                                              thereis (string-numberp ele))
                                                     (mapcar 'number-to-string (number-sequence 0 (1- (length header-list))))
                                                   header-list)))))
                         (columns-list (helm-comp-read (if is-x-p "Column as x: " "Columns to plot: ") (cons (if is-x-p "None" "")
                                                                                                             (let* ((current-line (when (buffer-file-name)
                                                                                                                                    (buffer-substring-no-properties
                                                                                                                                     (line-beginning-position) (line-end-position))))
                                                                                                                    (current-list (when (string-prefix-p "#" current-line)
                                                                                                                                    (split-string (string-remove-prefix "#" current-line) "[ \t]+" t "[ \t\n]+"))))
                                                                                                               (if (= (length current-list) (length header-line-list))
                                                                                                                   (-zip-pair current-list header-line-list)
                                                                                                                 header-line-list)))
                                                       :marked-candidates t
                                                       :buffer "*helm python plot data-swint*")))
                    (mapconcat 'identity columns-list ","))))
        (setq style-string (helm-comp-read "Style: " (list "plot" "stackplot" "step" "scatter" "bar" "barh" "stackbar" "stackbarh" "pie" "polar" "boxplot" "boxploth" "contourf")
                                           :buffer "*helm python plot data-swint*"))
        (if (memq major-mode '(inferior-python-mode jupyter-repl-mode))
            (setq data-string (puthash "data" (read-string "data_x, data_y, label: " (gethash "data" swint-python-plot-hash)) swint-python-plot-hash))
          (setq files-list (if (eq major-mode 'dired-mode) (dired-get-marked-files) (list (buffer-file-name))))
          (setq files-string (mapconcat 'identity files-list ","))
          (setq columns-string (plot-file-setup (car files-list)))
          (setq file-x-string (helm-comp-read "File as x: " (cons "None" (directory-files (helm-current-directory) nil directory-files-no-dot-files-regexp))
                                              :buffer "*helm python plot data-swint*"))
          (setq column-x-string (plot-file-setup (if (string= file-x-string "None") (car files-list) file-x-string) t)))
        (let* ((config-list (helm-comp-read "Configs: " (list "None" "rows" "labels" "fonts" "sizes" "colors" "lines" "markers" "hatchs" "polyfit" "twinx" "save" "animate")
                                            :marked-candidates t
                                            :buffer "*helm python fig config-swint*"))
               ;; 从列表中除去多个元素，也可以用：(cl-set-difference config-list '("twinx" "save" "animate" "None") :test 'equal)
               (args-alist (cl-loop for x in (seq-difference config-list '("twinx" "save" "animate" "None"))
                                    collect (cons x (if (listp (gethash x swint-python-plot-hash))
                                                        (mapconcat 'identity (helm-comp-read (concat x ": ") (gethash x swint-python-plot-hash)
                                                                                             :marked-candidates t
                                                                                             :buffer "*helm python plot data-swint*")
                                                                   ",")
                                                      (puthash x (read-string (concat x ": ") (gethash x swint-python-plot-hash)) swint-python-plot-hash))))))
          (setq shell-command-args (concat (mapconcat #'(lambda (x)
                                                          (format "--%s \"%s\"" (car x) (cdr x)))
                                                      args-alist " ")
                                           (if (member "twinx" config-list) " --twinx ")
                                           (if (member "save" config-list) " --save ")
                                           (if (member "animate" config-list) " --animate ")))
          (setq send-string-args (concat (if args-alist
                                             (concat "," (mapconcat #'(lambda (x)
                                                                        (format "%s='%s'" (car x) (cdr x)))
                                                                    args-alist ",")))
                                         (if (member "twinx" config-list) ",twinx=True")
                                         (if (member "save" config-list) ",save=True")
                                         (if (member "animate" config-list) ",animate=True"))))
        (pyvenv-activate-py3)
        (let ((python-command-string (format "if 'plot_data' not in dir():from sys import path;path.append('%s');import plot_data
plot_data.cli_plot([%s],'%s' %s)" swint-python-plot-exec-path data-string style-string (or send-string-args ""))))
          (cond ((equal major-mode 'inferior-python-mode)
                 (python-shell-send-string python-command-string))
                ((equal major-mode 'jupyter-repl-mode)
                 (jupyter-eval-string-command python-command-string))
                (t (if (and (fboundp 'python-shell-get-process) (python-shell-get-process))
                       (python-shell-send-string (format "if 'plot_data' not in dir():from sys import path;path.append('%s');import plot_data
plot_data.file_plot('%s','%s','%s','%s','%s' %s)" swint-python-plot-exec-path files-string columns-string file-x-string column-x-string style-string (or send-string-args "")))
                     (let* ((plot-data-command (concat "python " (expand-file-name "plot_data.py" swint-python-plot-exec-path)
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
(use-package elpy
  :delight '(:eval (propertize " E" 'face 'font-lock-function-name-face))
  :commands (elpy-shell-switch-to-shell toggle-elpy-mode-all-buffers)
  :init
  (bind-key "M-o M-p" #'(lambda (&optional arg) (interactive "P") (let* ((dir (helm-current-directory))
                                                                         (default-directory dir))
                                                                    (elpy-shell-switch-to-shell)
                                                                    (when arg
                                                                      (python-shell-send-string (format "import os; os.chdir('%s')" dir))))))
  (add-hook 'python-mode-hook (lambda ()
                                (bind-key "C-c e" 'toggle-elpy-mode-all-buffers python-mode-map)))
  (setq elpy-remove-modeline-lighter t)
  :config
  ;; elpy-config：查看运行环境，elpy-rpc--get-package-list：查看需要的包
  (setq elpy-rpc-timeout nil
        elpy-rpc-virtualenv-path 'current ;采用py3虚拟环境，而不是默认~/.emacs.d/elpy/rpc-venv
        elpy-get-info-from-shell t
        elpy-shell-starting-directory 'current-directory)
  ;; 使用ipython作为交互环境
  (setq python-shell-interpreter "ipython"
        python-shell-interpreter-args "-i --simple-prompt --pylab")
  ;; 使用jupyter作为交互环境
  ;; (setq python-shell-interpreter "jupyter"
  ;;       python-shell-interpreter-args "console --simple-prompt"
  ;;       python-shell-prompt-detect-failure-warning nil)
  ;; (add-to-list 'python-shell-completion-native-disabled-interpreters "jupyter")
  (define-key elpy-mode-map (kbd "M-.") nil)
  (define-key elpy-mode-map (kbd "C-c C-,") 'elpy-goto-definition)
  (define-key elpy-mode-map (kbd "C-c C-.") 'pop-tag-mark)
  (define-key elpy-mode-map (kbd "C-c C-/") 'elpy-doc)
  (define-key elpy-mode-map (kbd "M-TAB") 'elpy-company-backend)
  (define-key elpy-mode-map (kbd "C-c C-f") 'elpy-find-file)
  (define-key elpy-mode-map (kbd "C-c C-o") 'elpy-occur-definitions)
  (smartrep-define-key elpy-mode-map "C-c" '(("C-p" . elpy-flymake-previous-error)
                                             ("C-n" . elpy-flymake-next-error)))
  (define-key elpy-mode-map (kbd "C-c M-f") 'elpy-format-code)
  (define-key elpy-mode-map (kbd "C-c C-s") 'elpy-rgrep-symbol)
  (define-key elpy-mode-map (kbd "C-c C-r") elpy-refactor-map)
  (define-key elpy-mode-map (kbd "C-c C-x") elpy-django-mode-map)
  (define-key elpy-mode-map (kbd "C-c C-c") nil)
  (define-key elpy-mode-map (kbd "C-c C-b") nil)
  (define-key elpy-mode-map (kbd "C-c C-e") nil)
  (define-key inferior-python-mode-map (kbd "C-q") 'comint-send-eof)
  (define-key inferior-python-mode-map (kbd "C-c C-,") 'elpy-goto-definition)
  (define-key inferior-python-mode-map (kbd "C-c C-.") 'pop-tag-mark)
  (define-key inferior-python-mode-map (kbd "C-c C-/") 'elpy-doc)
  (advice-add 'elpy-shell-switch-to-shell :before #'pyvenv-activate-py3)
  ;; 使用global-elpy-mode方式开启elpy-mode
  ;; (define-global-minor-mode global-elpy-mode elpy-mode
  ;;   (lambda () (when (eq major-mode 'python-mode) (elpy-mode 1))))
  ;; (global-elpy-mode 1)
  ;; 在opened python buffer中开关elpy-mode
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
      (pyvenv-activate-py3)
      (elpy-enable)
      (elpy-modules-remove-modeline-lighter 'flymake-mode))))
;; ====================elpy====================
;;; jupyter
;; ==================jupyter===================
(use-package jupyter
  ;; 依赖emacs-zmq，自动安装失败时，可手动下载emacs-zmq.so并置于zmq.el同文件夹下
  ;; 开启jupyter后，需重新打开org文件，才能让ob-jupyter生效
  :bind (("M-o j" . swint-jupyter-run-repl)
         ("M-o J" . jupyter-connect-repl))
  :init
  (add-hook 'python-mode-hook (lambda ()
                                (bind-key "C-c c" 'jupyter-repl-associate-buffer python-mode-map)))
  :config
  (defun swint-jupyter-run-repl ()
    (interactive)
    (unless (featurep 'ob-jupyter)
      (add-to-list 'org-babel-load-languages '(jupyter . t) t)
      (org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages))
    (pyvenv-activate-py3)
    (call-interactively 'jupyter-run-repl))
  (define-key jupyter-repl-interaction-mode-map (kbd "M-i") nil)
  (define-key jupyter-repl-interaction-mode-map (kbd "C-x C-e") nil)
  (define-key jupyter-repl-interaction-mode-map (kbd "C-c C-/") #'jupyter-inspect-at-point)
  (define-key jupyter-repl-interaction-mode-map (kbd "C-c C-e") #'jupyter-eval-string-command))
;; ==================jupyter===================
;;; jedi
;; ===================jedi=====================
(use-package jedi
  ;; 使用jedi:install-server安装服务端
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
  (add-hook 'jedi-mode-hook #'(lambda ()
                                (set (make-local-variable 'company-idle-delay) nil)))
  (define-key jedi-mode-map (kbd "C-c j") 'jedi:get-in-function-call)
  (define-key jedi-mode-map (kbd "M-u") 'jedi:complete)
  (define-key jedi-mode-map (kbd "C-c C-,") 'jedi:goto-definition)
  (define-key jedi-mode-map (kbd "C-c C-.") 'jedi:goto-definition-pop-marker)
  (define-key jedi-mode-map (kbd "C-c C-/") 'jedi:show-doc)
  (define-key jedi-mode-map (kbd "C-c /") 'helm-jedi-related-names)
  (define-key jedi-mode-map (kbd "<C-tab>") nil)
  (define-key jedi-mode-map (kbd "C-c ?") nil)
  (define-key jedi-mode-map (kbd "C-c .") nil)
  (define-key jedi-mode-map (kbd "C-c ,") nil)
  (advice-add 'jedi:get-in-function-call :before #'(lambda ()
                                                     (pyvenv-activate-py3)
                                                     (unless jedi-mode
                                                       (dolist (buf (cl-remove-if-not (lambda (x)
                                                                                        (equal (buffer-mode x) 'python-mode))
                                                                                      (buffer-list)))
                                                         (with-current-buffer buf
                                                           (call-interactively 'jedi:setup)))
                                                       (add-hook 'python-mode-hook 'jedi:setup)))))
;; ===================jedi=====================
(provide 'setup_python)
