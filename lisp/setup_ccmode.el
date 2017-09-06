;;; ccmode
;; ===================ccmode====================
(use-package cc-mode
  ;; Enabled in modes.
  :defer t
  :mode (("\\.\\(cc\\|hh\\)\\'" . c++-mode)
         ("\\.[ch]\\(pp\\|xx\\|\\+\\+\\)\\'" . c++-mode)
         ("\\.\\(CC?\\|HH?\\)\\'" . c++-mode)
         ("\\.[ch]\\'" . c-mode)
         ("\\.y\\(acc\\)?\\'" . c-mode)
         ("\\.lex\\'" . c-mode)
         ("\\.i\\'" . c-mode)
         ("\\.ii\\'" . c++-mode))
  :config
  (defun c-compile-current-file ()
    (interactive)
    (unless (file-exists-p "Makefile")
      (let ((file (file-name-nondirectory buffer-file-name)))
        (compile (format "%s -o %s.out %s %s %s"
                         (or (getenv "CC") "gcc")
                         (file-name-sans-extension file)
                         (or (getenv "CPPFLAGS") "")
                         (or (getenv "CFLAGS") "-Wall -g")
                         file)))))
  (add-hook 'c-mode-hook
            (lambda ()
              (define-key c-mode-base-map (kbd "C-c C-c") 'c-compile-current-file)
              (define-key c-mode-base-map (kbd "C-c C-S-c") (lambda ()
                                                              (interactive)
                                                              (setq-local compilation-read-command nil)
                                                              (call-interactively 'compile)))
              (define-key c-mode-base-map (kbd "C-c d") 'disaster)
              (define-key c-mode-base-map (kbd "C-M-q") nil)
              (define-key c-mode-base-map (kbd "C-M-h") nil)
              (define-key c-mode-base-map (kbd "(") nil)
              (define-key c-mode-base-map (kbd "{") nil)))
  ;; Available C style:
  ;; “gnu”: The default style for GNU projects
  ;; “k&r”: What Kernighan and Ritchie, the authors of C used in their book
  ;; “bsd”: What BSD developers use, aka “Allman style” after Eric Allman.
  ;; “whitesmith”: Popularized by the examples that came with Whitesmiths C, an early commercial C compiler.
  ;; “stroustrup”: What Stroustrup, the author of C++ used in his book
  ;; “ellemtel”: Popular C++ coding standards as defined by “Programming in C++, Rules and Recommendations,” Erik Nyquist and Mats Henricson, Ellemtel
  ;; “linux”: What the Linux developers use for kernel development
  ;; “python”: What Python developers use for extension modules
  ;; “java”: The default style for java-mode (see below)
  ;; “user”: When you want to define your own style
  (setq c-default-style "linux"))
;; ===================ccmode====================
;;; gdb
;; =====================gdb=====================
(use-package gdb-mi
  ;; Enabled at commands.
  :defer t
  :commands gdb-or-gud-go
  :init
  (add-hook 'c-mode-hook '(lambda ()
                            (define-key c-mode-map (kbd "C-c g") 'gdb-or-gud-go)))
  :config
  (define-key gud-mode-map (kbd "C-c G") 'gud-quit)
  ;; 直接使用gdb-or-gud-go弹出gud-comint-buffer未定义，先gdb，然后gdb-or-gud-go。
  (defun gdb-or-gud-go (&optional arg)
    "If gdb isn't running; run gdb, else call gud-go."
    (interactive "P")
    (if arg
        (gdb-many-windows 1)
      (gdb-many-windows 0))
    (if (and gud-comint-buffer
             (buffer-name gud-comint-buffer)
             (get-buffer-process gud-comint-buffer)
             (with-current-buffer gud-comint-buffer (eq gud-minor-mode 'gdb)))
        (gud-call (if gdb-active-process "continue" "run") "")
      (gdb (gud-query-cmdline 'gdb))))
  (defun gud-quit ()
    "Kill gdb process."
    (interactive)
    (when gdb-many-windows
      (kill-buffer (gdb-locals-buffer-name))
      (kill-buffer (gdb-stack-buffer-name))
      (kill-buffer (gdb-breakpoints-buffer-name))
      (kill-buffer (gdb-inferior-io-name))
      (kill-buffer "*Buffer List*"))
    ;; 关闭gdb buffer。
    (with-current-buffer gud-comint-buffer
      (comint-skip-input)
      (kill-process (get-buffer-process gud-comint-buffer))
      (delete-other-windows)))
  ;; 关闭gdb的process时，关闭buffer。
  (add-hook 'gdb-mode-hook 'kill-shell-buffer-after-exit t))
;; =====================gdb=====================
;;; function-args
;; ==================function-args==============
(use-package function-args
  ;; Enabled in modes.
  :defer t
  :commands fa-config-default
  :init
  (add-hook 'c-mode-hook 'fa-config-default)
  :config
  (define-key function-args-mode-map (kbd "C-c u") 'moo-complete)
  (define-key function-args-mode-map (kbd "C-c j") 'moo-jump-directory)
  (define-key function-args-mode-map (kbd "C-c i") 'moo-jump-local)
  (define-key function-args-mode-map (kbd "C-c o") '(lambda ()
                                                      (interactive)
                                                      (if (overlayp fa-overlay)
                                                          (fa-abort)
                                                        (fa-show))))
  (define-key function-args-mode-map (kbd "C-j") 'fa-jump-maybe)
  (define-key function-args-mode-map (kbd "M-u") nil)
  (define-key function-args-mode-map (kbd "M-i") nil)
  (define-key function-args-mode-map (kbd "M-o") nil)
  (define-key function-args-mode-map (kbd "M-h") nil)
  (define-key function-args-mode-map (kbd "M-n") nil)
  (define-key function-args-mode-map (kbd "M-j") nil)
  (define-key function-args-mode-map (kbd "C-M-j") nil)
  (define-key function-args-mode-map (kbd "C-M-k") nil))
;; ==================function-args==============
;;; hs-minor-mode
;; ==================hs-minor-mode==============
(use-package hideshow
  ;; Enabled in modes.
  :defer t
  :commands hs-minor-mode
  :init
  (add-hook 'c-mode-common-hook 'hs-minor-mode)
  :config
  (add-hook 'c-mode-hook
            (lambda ()
              (define-key hs-minor-mode-map (kbd "C-c C-`") 'hs-toggle-hiding))))
;; ==================hs-minor-mode==============
;;; semantic
;; ===================semantic==================
(use-package semantic
  ;; Enabled in modes.
  :defer t
  :init
  (dolist (hook '(c-mode-hook
                  c++-mode-hook))
    (add-hook hook 'semantic-mode))
  :config
  (add-to-list 'semantic-default-submodes 'global-semanticdb-minor-mode)
  ;; 在空闲时分析buffer。
  (add-to-list 'semantic-default-submodes 'global-semantic-idle-scheduler-mode)
  ;; 在minibuffer显示函数。
  (add-to-list 'semantic-default-submodes 'global-semantic-idle-summary-mode)
  ;; 在header-line显示函数。
  (add-to-list 'semantic-default-submodes 'global-semantic-stickyfunc-mode)
  ;; 默认semantic快捷键以C-c ,为前缀。
  (define-key semantic-mode-map (kbd "C-c ,") nil))
;; ===================semantic==================
;;; helm-gtags
;; ==================helm-gtags=================
(use-package helm-gtags
  ;; Enabled in modes.
  :defer t
  :bind (("M-s ," . swint-helm-gtags-dwim)
         ("M-s ." . swint-helm-gtags-pop-stack)
         ("M-s /" . swint-helm-gtags-select))
  :config
  (defun swint-helm-gtags-dwim ()
    (interactive)
    (unless helm-gtags-mode
      (helm-gtags-mode t))
    (add-hook (derived-mode-hook-name major-mode) 'helm-gtags-mode)
    (call-interactively 'helm-gtags-dwim))
  (defun swint-helm-gtags-pop-stack ()
    (interactive)
    (unless helm-gtags-mode
      (helm-gtags-mode t))
    (add-hook (derived-mode-hook-name major-mode) 'helm-gtags-mode)
    (call-interactively 'helm-gtags-pop-stack))
  (defun swint-helm-gtags-select ()
    (interactive)
    (unless helm-gtags-mode
      (helm-gtags-mode t))
    (add-hook (derived-mode-hook-name major-mode) 'helm-gtags-mode)
    (call-interactively 'helm-gtags-select))
  (setq helm-gtags-ignore-case t
        helm-gtags-auto-update t
        helm-gtags-use-input-at-cursor t
        helm-gtags-pulse-at-cursor t
        helm-gtags-prefix-key "\M-s")
  (smartrep-define-key helm-gtags-mode-map "M-s"
    '(("," . helm-gtags-dwim)
      ("." . helm-gtags-pop-stack)
      ("/" . helm-gtags-select)
      ("C-," . helm-gtags-previous-history)
      ("C-." . helm-gtags-next-history)
      ("C-/" . helm-gtags-show-stack))))
;; ==================helm-gtags=================
;;; arduino
;; ===================arduino===================
(use-package arduino-mode
  ;; Enabled in modes.
  :defer t
  :mode (("\\.pde\\'" . arduino-mode)
         ("\\.ino\\'" . arduino-mode)))
;; ===================arduino===================
;; ==================disaster===================
(use-package disaster
  ;; Enabled at commands.
  :defer t
  :commands disaster)
;; ==================disaster===================
(provide 'setup_ccmode)
