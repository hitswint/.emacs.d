;;; ivy
;; =====================ivy========================
(use-package ivy
  :commands (ivy-read ivy-completing-read)
  :config
  (bind-key "M-s y" 'ivy-resume)
  (bind-key "C-h" 'ivy-avy ivy-minibuffer-map)
  (bind-key "C-;" 'ivy-mark ivy-minibuffer-map)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-height 10)
  (setq ivy-count-format "%d/%d ")
  (use-package ivy-hydra
    :after ivy)
  (use-package hydra
    :after ivy-hydra)
  ;; https://github.com/pengpengxp/swiper/wiki/ivy-support-chinese-pinyin
  ;; (add-to-list 'ivy-re-builders-alist '(swiper . re-builder-pinyin)) ;单独对某命令设置
  (setq ivy-re-builders-alist '((counsel-M-x . ivy--regex-plus)
                                (counsel-ag . ivy--regex-plus)
                                (t . re-builder-pinyin)))
  (defun swint-pinyinlib-build-regexp-string (str)
    (cond ((equal str ".*") ".*")
          (t (pinyinlib-build-regexp-string str t))))
  (defun swint-pinyin-regexp-helper (str)
    (cond ((equal str " ") ".*")
          ((equal str "") nil)
          (t str)))
  (defun pinyin-to-utf8 (str)
    (cond ((equal 0 (length str)) nil)
          (t (mapconcat 'swint-pinyinlib-build-regexp-string
                        (remove nil (mapcar 'swint-pinyin-regexp-helper
                                            (split-string str "")))
                        ""))))
  (defun re-builder-pinyin (str)
    (or (pinyin-to-utf8 str)
        (ivy--regex-plus str)
        (ivy--regex-ignore-order str))))
;; =====================ivy========================
;;; swiper
;; ===================swiper=======================
(use-package swiper
  :commands (swiper swiper-all swiper-from-isearch)
  :init
  (bind-key "M-s s" #'(lambda () (interactive) (swiper (swint-get-current-thing))))
  (bind-key "M-s S" #'(lambda () (interactive) (swiper-all (swint-get-current-thing))))
  (add-hook 'isearch-mode-hook (lambda ()
                                 (bind-key "M-s s" 'swiper-from-isearch isearch-mode-map))))
;; ===================swiper=======================
;;; counsel
;; ===================counsel======================
(use-package counsel
  ;; 按键逻辑：helm(C-x c)/counsel(M-s c)
  :commands counsel-read-file-for-rsync
  :bind (("M-X" . counsel-M-x)
         ("C-x C-r" . swint-counsel-history)
         ("M-s c b" . counsel-switch-buffer)
         ("M-s c u" . counsel-unicode-char)
         ("M-s c l" . counsel-locate)
         ("M-s c i" . counsel-imenu)
         ("M-s c C-x C-f" . counsel-find-file)
         ("M-s c o" . counsel-outline)
         ("M-s c d" . counsel-dpkg)
         ("M-s c g" . counsel-ag)
         ("M-s c G" . counsel-rg)
         ("M-s c p" . counsel-list-processes)
         ("M-s c f" . counsel-fonts)
         ("M-s c M-y" . counsel-yank-pop))
  :config
  (defun swint-counsel-history ()
    "List command history based on major-mode."
    (interactive)
    (cond
     ((memq major-mode '(shell-mode inferior-octave-mode))
      (call-interactively 'counsel-shell-history))
     ((eq major-mode 'eshell-mode)
      (call-interactively 'counsel-esh-history))
     ((memq major-mode '(inferior-python-mode jupyter-repl-mode))
      (let ((selected-hist (helm-comp-read "History: " (nreverse (cl-delete-if (lambda (x) (or (null x) (or (string-match-p "import codecs, os;" x)
                                                                                                            (string-match-p "__PYTHON_EL_eval" x))))
                                                                               ;; 直接使用python-shell-send-string-no-output返回hist结果速度过慢，加-f以通过~/.ipython_history中转
                                                                               (split-string (let ((ipython-history-file (expand-file-name "~/.ipython_history")))
                                                                                               (delete-file ipython-history-file) ; 先删除历史文件，否则弹出overwrite确认提示
                                                                                               (python-shell-send-string-no-output
                                                                                                (format "get_ipython().run_line_magic(\"hist\", \"-u -g -l 1000 -f %s\")" ipython-history-file))
                                                                                               (shell-command-to-string (format "cat %s" ipython-history-file)))
                                                                                             "\n" t "[ \t\n]+")))
                                           :buffer "*helm python history-swint*")))
        (insert (replace-regexp-in-string "^[0-9]*/*[0-9]+: " "" selected-hist))))
     (t (call-interactively 'swint-counsel-sh-history))))
  (defun swint-counsel-sh-history ()
    "Insert the bash history."
    (interactive)
    (let* ((collection (nreverse (split-string (if (file-exists-p "~/.zsh_history")
                                                   ;; 直接读取.zsh_history时中文显示乱码，且每行为": 1647668977:0;df -h"，需进行替换
                                                   ;; (f-read-text  (file-truename "~/.zsh_history")
                                                   ;; (while (re-search-forward "^: [0-9]+:[0-9];\\(.+\\)\n" nil t)
                                                   ;;   (replace-match "\\1\n"))
                                                   (shell-command-to-string "zsh -i -c 'HISTSIZE=1000; fc -R ~/.zsh_history; fc -l -n 1'")
                                                 (f-read-text (file-truename "~/.bash_history")))
                                               "\n" t)))
           (val (if (= 1 (length collection))
                    (car collection)
                  (ivy-read (format "Bash/Zsh history:") collection))))
      (when val
        (insert val))))
  (defun counsel-read-file-for-rsync (files &optional initial-input)
    "Forward to `find-file'.
When INITIAL-INPUT is non-nil, use it in the minibuffer during completion."
    (interactive)
    (ivy-read "Find file: " 'read-file-name-internal
              :matcher #'counsel--find-file-matcher
              :initial-input initial-input
              :action #'(lambda (x) (with-ivy-window
                                      (if (and counsel-find-file-speedup-remote
                                               (file-remote-p ivy--directory))
                                          (let ((find-file-hook nil))
                                            (setf (symbol-value files)
                                                  (append (symbol-value files)
                                                          (last (split-string (expand-file-name x ivy--directory) ":")))))
                                        (setf (symbol-value files)
                                              (append (symbol-value files)
                                                      (last (split-string (expand-file-name x ivy--directory) ":")))))))
              :preselect (counsel--preselect-file)
              :require-match 'confirm-after-completion
              :history 'file-name-history
              :keymap counsel-find-file-map
              :caller 'counsel-find-file)))
;; ===================counsel======================
(provide 'setup_ivy)
