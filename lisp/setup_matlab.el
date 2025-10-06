;;; octave
;; ======================octave====================
(use-package octave
  :delight "Oct"
  :mode ("\\.m$" . octave-mode)
  :init
  (bind-key "M-o M-o" 'run-octave)
  :config
  (add-hook 'inferior-octave-mode-hook 'kill-shell-buffer-after-exit t)
  (define-key octave-mode-map (kbd "C-c C-,") 'octave-find-definition)
  (define-key octave-mode-map (kbd "C-c C-.") 'pop-tag-mark)
  (define-key octave-mode-map (kbd "C-c C-/") 'octave-help)
  (define-key octave-mode-map (kbd "C-c C-c") #'(lambda () (interactive)
                                                  (if mark-active
                                                      (call-interactively 'octave-send-region)
                                                    (call-interactively 'octave-send-line))))
  (define-key octave-mode-map (kbd "C-c C-b") 'octave-send-buffer)
  (define-key octave-mode-map (kbd "M-.") nil)
  (define-key octave-mode-map (kbd "C-h") nil)
  (define-key inferior-octave-mode-map (kbd "C-c C-,") 'octave-find-definition)
  (define-key inferior-octave-mode-map (kbd "C-c C-/") 'octave-help)
  (define-key inferior-octave-mode-map (kbd "C-q") 'comint-send-eof)
  (define-key inferior-octave-mode-map [up] 'comint-previous-input)
  (define-key inferior-octave-mode-map [down] 'comint-next-input)
  (define-key inferior-octave-mode-map (kbd "M-.") nil)
  (define-key inferior-octave-mode-map (kbd "C-h") nil)
  (setq octave-comment-start "%")
  (setq octave-comment-char ?%)
  (setq octave-auto-indent t))
;; ======================octave====================
;;; matlab
;; ======================matlab====================
(use-package matlab
  :delight "Mat"
  :commands matlab-shell
  :init
  (bind-key "M-o M-O" #'(lambda (&optional arg) (interactive "P") (let* ((dir (helm-current-directory))
                                                                         (default-directory dir))
                                                                    (matlab-shell)
                                                                    (when arg
                                                                      (matlab-shell-send-string (format "cd '%s'\n" dir))))))
  ;; 默认修改magic-mode-alist，使得打开.m文件始终加载matlab-mode，无法加载octave-mode
  (setq magic-mode-alist (remove '(matlab-is-matlab-file . matlab-mode) magic-mode-alist))
  :config
  (advice-add 'matlab-is-matlab-file :around #'(lambda (fn) (and (funcall fn) (ignore-errors (matlab-shell-active-p)))))
  ;; matlab-shell无法进入中文文件夹，设置LANG=zh_CN.UTF-8切换至中文，仍存在问题
  ;; 画图时弹出界面不能用C-q关闭，否则将退出matlab-shell
  (setq matlab-indent-function-body t)
  (setq matlab-shell-command-switches '("-nodesktop -nosplash"))
  (defun matlab-shell-locate-fcn (fcn)
    "Run \"which FCN\" in the `matlab-shell', then open the file."
    (interactive
     (list
      (let ((default (matlab-read-word-at-point)))
        (if (and default (not (equal default "")))
            (let ((s (read-string (concat "MATLAB locate fcn (default " default "): "))))
              (if (string= s "") default s))
          (read-string "MATLAB locate fcn: ")))))
    (let ((file (matlab-shell-which-fcn fcn)))
      (if (or (not file)
              (string-empty-p (car file)))
          (error "Command which('%s') returned empty" fcn)
        (xref-push-marker-stack)
        (find-file (car file)))))
  (define-key matlab-mode-map (kbd "C-h") nil)
  (define-key matlab-mode-map (kbd "M-s") nil)
  (define-key matlab-mode-map (kbd "C-c C-x") matlab--shell-help-map)
  (define-key matlab-mode-map (kbd "C-c i") 'matlab-insert-map-fcn)
  (define-key matlab-mode-map (kbd "C-c C-,") 'matlab-shell-locate-fcn)
  (define-key matlab-mode-map (kbd "C-c C-.") 'xref-pop-marker-stack)
  (define-key matlab-mode-map (kbd "C-c C-/") 'matlab-shell-describe-command)
  (define-key matlab-mode-map (kbd "C-c C-c") 'matlab-shell-run-region-or-line)
  (define-key matlab-mode-map (kbd "C-c C-b") #'(lambda () (interactive)
                                                  (matlab-shell-run-region (point-min) (point-max))))
  (add-hook 'matlab-shell-mode-hook
            #'(lambda ()
                (kill-shell-buffer-after-exit)
                (define-key matlab-shell-mode-map (kbd "C-h") nil)
                (define-key matlab-shell-mode-map (kbd "C-c C-x") matlab--shell-help-map)
                (define-key matlab-shell-mode-map (kbd "<C-tab>") nil)
                (define-key matlab-shell-mode-map (kbd "C-c <C-tab>") 'matlab-shell-c-tab)
                (define-key matlab-shell-mode-map (kbd "C-c C-,") 'matlab-shell-locate-fcn)
                (define-key matlab-shell-mode-map (kbd "C-c C-/") 'matlab-shell-describe-command))))
;; ======================matlab====================
(provide 'setup_matlab)
