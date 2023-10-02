;;; tangotango-theme
;; ==============tangotango-theme=============
(use-package tangotango-theme
  :config
  (load-theme 'tangotango t)
  ;; 设置mode-line中buffer名字显示
  ;; (set-face-attribute 'mode-line-buffer-id nil :foreground "deep sky blue")
  (set-face-attribute 'highlight nil :background "black"))
;; ==============tangotango-theme=============
;;; cycle-mini
;; ================cycle-mini=================
(use-package cycle-mini
  :load-path "site-lisp/cycle-mini/"
  :bind (:map minibuffer-local-completion-map
              ("C-p" . cycle-mini-previous-completion)
              ("C-n" . cycle-mini-next-completion)))
;; ================cycle-mini=================
;;; all-the-icons
;; ==============all-the-icons================
(use-package all-the-icons
  ;; :if (display-graphic-p)
  :after (all-the-icons-dired neotree))
;; ==============all-the-icons================
;; ==============all-the-icons================
;;; Mode Line
;; ================Mode Line==================
(line-number-mode t)
(column-number-mode t)
(defcustom ml/name-width 64
  "Maximum size of the file name in the mode-line."
  :type 'integer)
(defcustom ml/directory-truncation-string (if (char-displayable-p ?…) "…/" ".../")
  "String used when truncating part of the file path."
  :type 'string)
(defun ml/do-shorten-directory (dir total-max-length)
  "Show up to TOTAL-MAX-LENGTH characters of a directory name DIR."
  (let ((buffer-name-length (string-width (buffer-name))))
    (if (<= (string-width dir) (- total-max-length buffer-name-length))
        dir
      (let ((path (reverse (split-string dir "/")))
            (output ""))
        (when (and path (equal "" (car path)))
          (setq path (cdr path)))
        (let ((max (- total-max-length
                      buffer-name-length
                      (string-width ml/directory-truncation-string))))
          (while (and path (<= (string-width (concat (car path) "/" output))
                               max))
            (setq output (concat (car path) "/" output))
            (setq path (cdr path))))
        (when path
          (setq output (concat ml/directory-truncation-string output)))
        output))))
(setq-default mode-line-format (delete '(vc-mode vc-mode) mode-line-format) ;去除vc-mode显示
              mode-line-percent-position '(-3 "%p")
              mode-line-modes (seq-remove (lambda (x) (member x '("(" ")"))) mode-line-modes)
              ;; 升级29之后buffer名字前有额外空格
              ;; 设置mode-line-buffer-identification为默认值"%b"时无空格，而其他值都有空格
              ;; 采用compact模式，使用单个空格代替连续空格
              mode-line-compact 'long
              mode-line-buffer-identification (let ((orig (car mode-line-buffer-identification)))
                                                `(:eval (cons (concat
                                                               (when-let ((file (buffer-file-name)))
                                                                 (concat (when (display-graphic-p)
                                                                           (all-the-icons-dired--icon file))
                                                                         " "
                                                                         (ml/do-shorten-directory (abbreviate-file-name default-directory)
                                                                                                  ;; (min (- (window-width) 8) ml/name-width)
                                                                                                  (- (window-width)
                                                                                                     24
                                                                                                     (length mode-line-modes)
                                                                                                     (length (and projectile-mode projectile--mode-line))
                                                                                                     (length (and (frame-parameter nil 'swint-persp-loadp) (persp-name (persp-curr))))))))
                                                               ,orig)
                                                              (cdr mode-line-buffer-identification)))))
(add-hook 'dired-mode-hook #'(lambda () (setq-local mode-line-buffer-identification
                                                    (let ((orig (car mode-line-buffer-identification)))
                                                      `(:eval (cons (concat (when (display-graphic-p)
                                                                              (all-the-icons-dired--icon default-directory))
                                                                            " "
                                                                            (ml/do-shorten-directory (abbreviate-file-name
                                                                                                      (file-name-parent-directory default-directory))
                                                                                                     ;; (min (- (window-width) 8) ml/name-width)
                                                                                                     (- (window-width)
                                                                                                        24
                                                                                                        (length mode-line-modes)
                                                                                                        (length (and projectile-mode projectile--mode-line))
                                                                                                        (length (and (frame-parameter nil 'swint-persp-loadp) (persp-name (persp-curr))))))
                                                                            (propertize ,orig 'face 'dired-directory))
                                                                    (cdr mode-line-buffer-identification)))))))
;; ================Mode Line==================
(provide 'setup_appearance)
