;;; ccmode
;; ===================ccmode====================
(use-package cc-mode
  ;; Enabled in modes.
  :defer t
  :commands (c-mode c++-mode)
  :init
  (add-to-list 'auto-mode-alist '("\\.\\(cc\\|hh\\)\\'" . c++-mode))
  (add-to-list 'auto-mode-alist '("\\.[ch]\\(pp\\|xx\\|\\+\\+\\)\\'" . c++-mode))
  (add-to-list 'auto-mode-alist '("\\.\\(CC?\\|HH?\\)\\'" . c++-mode))
  (add-to-list 'auto-mode-alist '("\\.[ch]\\'" . c-mode))
  (add-to-list 'auto-mode-alist '("\\.y\\(acc\\)?\\'" . c-mode))
  (add-to-list 'auto-mode-alist '("\\.lex\\'" . c-mode))
  (add-to-list 'auto-mode-alist '("\\.i\\'" . c-mode))
  (add-to-list 'auto-mode-alist '("\\.ii\\'" . c++-mode))
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
  :bind (("M-g g" . gdb-or-gud-go)
         ("M-g G" . gud-kill))
  :config
  ;; 默认打开多窗口会有问题。
  (setq gdb-many-windows t)
  ;; 出现问题，在于 gdb-ui过时了，似乎改成gdb-mi。
  (defun gdb-or-gud-go ()
    "If gdb isn't running; run gdb, else call gud-go."
    (interactive)
    (if (and gud-comint-buffer
             (buffer-name gud-comint-buffer)
             (get-buffer-process gud-comint-buffer)
             ;; (with-current-buffer gud-comint-buffer (eq gud-minor-mode 'gdb))
             )
        (gud-call (if gdb-active-process "continue" "run") "")
      (gdb (gud-query-cmdline 'gdb))))
  (defun gud-kill ()
    "Kill gdb process."
    (interactive)
    ;; 关闭其他四个buffer，其中io buffer会询问。
    (kill-buffer (gdb-locals-buffer-name))
    (kill-buffer (gdb-stack-buffer-name))
    (kill-buffer (gdb-breakpoints-buffer-name))
    (kill-buffer (gdb-inferior-io-name))
    ;; 关闭gdb buffer。
    (with-current-buffer gud-comint-buffer (comint-skip-input))
    (kill-process (get-buffer-process gud-comint-buffer))
    (delete-window))
  ;; 关闭gdb的process时，关闭buffer。取自setup_misc中'退出shell时关闭buffer'。
  (add-hook 'gdb-mode-hook 'kill-shell-buffer-after-exit t)
  ;; 直接使用gdb-or-gud-go会显示gud-comint-buffer变量未定义，需要先使用gdb一次，然后才能使用gdb-or-gud-go。
  )
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
  (define-key function-args-mode-map (kbd "C-M-j") nil))
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
;; helm-man-woman: C-x c m
;; helm-semantic-or-imenu: C-x c i
(use-package helm-gtags
  ;; Enabled in modes.
  :defer t
  :commands helm-gtags-mode
  :init
  ;; Enable helm-gtags-mode
  (dolist (hook '(;; dired-mode-hook
                  ;; eshell-mode-hook
                  c-mode-hook
                  c++-mode-hook
                  asm-mode-hook))
    (add-hook hook 'helm-gtags-mode))
  :config
  (setq helm-gtags-ignore-case t
        helm-gtags-auto-update t
        helm-gtags-use-input-at-cursor t
        helm-gtags-pulse-at-cursor t
        helm-gtags-prefix-key "\C-c"
        helm-gtags-suggested-key-mapping t)
  (define-key helm-gtags-mode-map (kbd "C-c C-,") 'helm-gtags-dwim)
  (define-key helm-gtags-mode-map (kbd "C-c C-.") 'helm-gtags-pop-stack)
  (define-key helm-gtags-mode-map (kbd "C-c C-/") 'helm-gtags-select)
  (define-key helm-gtags-mode-map (kbd "C-c C-M-/") 'helm-gtags-tags-in-this-function)
  (smartrep-define-key helm-gtags-mode-map "C-c"
    '(("," . helm-gtags-previous-history)
      ("." . helm-gtags-next-history)
      ("/" . helm-gtags-show-stack)))
  (define-key helm-gtags-mode-map (kbd "M-.") nil))
;; ==================helm-gtags=================
;;; arduino
;; ===================arduino===================
(use-package arduino-mode
  ;; Enabled in modes.
  :defer t
  :commands arduino-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.pde\\'" . arduino-mode))
  (add-to-list 'auto-mode-alist '("\\.ino\\'" . arduino-mode)))
;; ===================arduino===================
(provide 'setup_ccmode)
