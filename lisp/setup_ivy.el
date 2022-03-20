;;; ivy
;; =====================ivy========================
(def-package! ivy
  :commands (ivy-read ivy-completing-read)
  :config
  (bind-key "M-s y" 'ivy-resume)
  (bind-key "C-h" 'ivy-avy ivy-minibuffer-map)
  (bind-key "C-;" 'ivy-mark ivy-minibuffer-map)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-height 10)
  (setq ivy-count-format "%d/%d ")
  (def-package! ivy-hydra
    :after ivy)
  (def-package! hydra
    :after ivy-hydra)
  ;; https://github.com/pengpengxp/swiper/wiki/ivy-support-chinese-pinyin
  ;; (add-to-list 'ivy-re-builders-alist '(swiper . re-builder-pinyin)) ;单独对某命令设置
  (setq ivy-re-builders-alist '((t . re-builder-pinyin)))
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
                                            (split-string str ""))) ""))))
  (defun re-builder-pinyin (str)
    (or (pinyin-to-utf8 str)
        (ivy--regex-plus str)
        (ivy--regex-ignore-order str))))
;; =====================ivy========================
;;; swiper
;; ===================swiper=======================
(def-package! swiper
  :commands swiper-from-isearch
  :bind (("M-s s" . swint-swiper)
         ("M-s S" . swiper-all))
  :init
  (add-hook 'isearch-mode-hook (lambda ()
                                 (bind-key "M-s s" 'swiper-from-isearch isearch-mode-map)))
  :config
  (defun swint-swiper ()
    (interactive)
    (let ((swint-swiper-current-thing
           (if (region-active-p)
               (buffer-substring (region-beginning) (region-end))
             (symbol-name-at-point))))
      (deactivate-mark)
      (swiper swint-swiper-current-thing))))
;; ===================swiper=======================
;;; counsel
;; ===================counsel======================
(def-package! counsel
  ;; 按键逻辑：helm(C-x c x)/counsel(M-s c x)。
  :commands counsel-read-file-for-rsync
  :bind (("M-X" . counsel-M-x)
         ("C-x C-r" . swint-counsel-history)
         ("M-s c u" . counsel-unicode-char)
         ("M-s c l" . counsel-locate)
         ("M-s c i" . counsel-imenu)
         ("M-s c C-x C-f" . counsel-find-file)
         ("M-s c o" . counsel-outline)
         ("M-s c d" . counsel-dpkg)
         ("M-s c g" . counsel-ag)
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
      (let ((selected-hist (helm-comp-read "History: " (nreverse (cl-delete-if (lambda (x) (or (null x) (string-match-p "import codecs, os;" x)))
                                                                               (split-string (python-shell-send-string-no-output "%hist -u -g -l 1000") "\n" t "[ \t\n]+")))
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
