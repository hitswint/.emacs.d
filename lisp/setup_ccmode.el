;;; ccmode
;; ===================ccmode====================
(use-package cc-mode
  :delight
  (c++-mode "CC")
  (c-mode "C")
  :mode (("\\.\\(cc\\|hh\\)\\'" . c++-mode)
         ("\\.[ch]\\(pp\\|xx\\|\\+\\+\\)\\'" . c++-mode)
         ("\\.\\(CC?\\|HH?\\)\\'" . c++-mode)
         ("\\.[ch]\\'" . c-mode)
         ("\\.y\\(acc\\)?\\'" . c-mode)
         ("\\.lex\\'" . c-mode)
         ("\\.i\\'" . c-mode)
         ("\\.ii\\'" . c++-mode))
  :config
  (setq c-default-style "linux")
  (defconst OpenFOAM-style '((c-basic-offset . 4)
                             (c-tab-always-indent . t)
                             (c-comment-only-line-offset . (0 . 0))
                             (c-indent-comments-syntactically-p . t)
                             (c-block-comments-indent-p . nil)
                             ;;(comment-style . 'multi-line)
                             (comment-start . "// ")
                             (comment-end . "")
                             (c-cleanup-list . '((defun-close-semi) (list-close-comma) (scope-operator)))
                             (c-backslash-column . 79)
                             (c-backslash-max-column . 79)
                             (c-auto-align-backslashes . t)
                             (c-toggle-auto-state . 1)
                             (c-toggle-auto-hungry-state . 1)
                             (c-offsets-alist
                              ;;(c . +)                     ;; inside a multi-line C style block comment
                              (defun-open . 0)            ;; brace that opens a function definition
                              (defun-close . 0)           ;; brace that closes a function definition
                              (defun-block-intro . +)     ;; the first line in a top-level defun
                              (class-open . 0)            ;; brace that opens a class definition
                              (class-close . 0)           ;; brace that closes a class definition
                              (inline-open . 0)           ;; brace that opens an in-class inline method
                              (inline-close . 0)          ;; brace that closes an in-class inline method
                              (topmost-intro . 0)         ;; the first line in a topmost construct
                              ;; definition
                              (topmost-intro-cont . 0)    ;; topmost definition continuation lines
                              (member-init-intro . +)     ;; first line in a member initialization list
                              (member-init-cont . 0)      ;; subsequent member initialization list lines
                              (inher-intro . 0)           ;; first line of a multiple inheritance list
                              (inher-cont . +)            ;; subsequent multiple inheritance lines
                              (block-open . 0)            ;; statement block open brace
                              (block-close . 0)           ;; statement block close brace
                              (brace-list-open . 0)       ;; open brace of an enum or static array list
                              (brace-list-close . 0)      ;; open brace of an enum or static array list
                              (brace-list-intro . +)      ;; first line in an enum or static array list
                              (brace-list-entry . 0)      ;; subsequent lines in an enum or static array
                              ;; list
                              (statement . 0)             ;; a C/C++/ObjC statement
                              (statement-cont . 0)        ;; a continuation of a C/C++/ObjC statement
                              (statement-block-intro . +) ;; the first line in a new statement block
                              (statement-case-intro . +)  ;; the first line in a case `block'
                              (statement-case-open . +)   ;; the first line in a case `block'
                              ;; starting with brace
                              (substatement . +)          ;; the first line after an if/while/for/do/else
                              (substatement-open . 0)     ;; the brace that opens a substatement block
                              (case-label . +)            ;; a case or default label
                              (access-label . -)          ;; C++ private/protected/public access label
                              (label . -)                 ;; any non-special C/C++/ObjC label
                              (do-while-closure . 0)      ;; the `while' that ends a do/while construct
                              (else-clause . 0)           ;; the `else' of an if/else construct
                              (comment-intro . 0)         ;; line containing only a comment introduction
                              (arglist-intro . +)         ;; the first line in an argument list
                              (arglist-cont . 0)          ;; subsequent argument list lines when no
                              ;; subsequent argument list lines when no the
                              ;; arglist opening paren
                              (arglist-cont-nonempty . 0) ;; subsequent argument list lines when at
                              ;; subsequent argument list lines when at line
                              ;; as the arglist opening paren
                              (arglist-close . 0)         ;; line as the arglist opening paren
                              (stream-op . +)             ;; lines continuing a stream operator construct
                              (inclass . +)               ;; the construct is nested inside a class
                              ;; definition
                              (cpp-macro . 0)             ;; the construct is nested inside a class
                              ;; definition
                              (friend . 0)                ;; a C++ friend declaration
                              ))
    "OpenFOAM C++ Programming Style"
    )
  (c-add-style "OpenFOAM" OpenFOAM-style)
  (advice-add 'c-update-modeline :override #'ignore)
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
  (dolist (hook '(c-mode-hook c++-mode-hook asm-mode-hook))
    (add-hook hook (lambda ()
                     (define-key c-mode-base-map (kbd "C-c C-c") 'c-compile-current-file)
                     (define-key c-mode-base-map (kbd "C-c C-M-c") (lambda () (interactive)
                                                                     (setq-local compilation-read-command nil)
                                                                     (call-interactively 'compile)))
                     (define-key c-mode-base-map (kbd "C-M-q") nil)
                     (define-key c-mode-base-map (kbd "(") nil)
                     (define-key c-mode-base-map (kbd "{") nil)))))
;; ===================ccmode====================
;;; gdb
;; =====================gdb=====================
(use-package gdb-mi
  :bind (("M-o M-g" . gdb-or-gud-go)
         ("M-o g" . gdb-restore-windows)
         ("M-o G" . gdb-many-windows))
  :init
  (setq gud-key-prefix "\C-x\C-a")
  :config
  (setq gdb-show-main t)
  (setq gdb-many-windows t)
  (defun gdb-or-gud-go ()
    "If gdb isn't running; run gdb, else call gud-go."
    (interactive)
    (if (and gud-comint-buffer
             (buffer-name gud-comint-buffer)
             (get-buffer-process gud-comint-buffer)
             (with-current-buffer gud-comint-buffer (memq gud-minor-mode '(gdbmi gdb))))
        (gud-go nil)
      (gdb (gud-query-cmdline 'gdb))))
  (add-hook 'gdb-mode-hook 'kill-shell-buffer-after-exit t))
(use-package gud
  :after gdb-mi
  :config
  (defun gud-quit ()
    "Kill gdb buffers."
    (if-let ((io (get-process "gdb-inferior")))
        (delete-process io)) ;gud-kill-buffer-hook后仍残留gdb-inferior
    (when gdb-many-windows
      (cl-loop for bn in (list (gdb-locals-buffer-name)
                               (gdb-stack-buffer-name)
                               (gdb-breakpoints-buffer-name)
                               (gdb-inferior-io-name))
               do (if-let ((b (get-buffer bn)))
                      (when (buffer-live-p b)
                        (kill-buffer b))))))
  (advice-add 'gud-kill-buffer-hook :after #'gud-quit))
;; =====================gdb=====================
;;; function-args
;; ==================function-args==============
(use-package function-args
  :delight '(:eval (propertize " F" 'face 'font-lock-function-name-face))
  :commands (moo-complete moo-jump-local moo-jump-directory fa-jump-maybe swint-fa-show)
  :init
  (dolist (hook '(c-mode-hook c++-mode-hook asm-mode-hook))
    (add-hook hook (lambda ()
                     (local-set-key (kbd "C-c u") 'moo-complete)
                     (local-set-key (kbd "C-c i") 'moo-jump-local)
                     (local-set-key (kbd "C-c I") 'moo-jump-directory)
                     (local-set-key (kbd "C-c j") 'fa-jump-maybe)
                     (local-set-key (kbd "C-c o") 'swint-fa-show))))
  :config
  (function-args-mode 1) ;function-args-mode only trigger semantic-mode
  (assq-delete-all 'function-args-mode minor-mode-map-alist) ;解除默认按键绑定
  (defun swint-fa-show ()
    (interactive)
    (if (overlayp fa-overlay)
        (fa-abort)
      (fa-show))))
;; ==================function-args==============
;;; semantic
;; ===================semantic==================
(use-package semantic
  :after function-args
  :config
  (semantic-mode 1)
  (setq semantic-idle-scheduler-idle-time 5)
  ;; 弹出警告：Selecting deleted buffer
  ;; (semanticdb-enable-gnu-global-databases 'c-mode)
  ;; (semanticdb-enable-gnu-global-databases 'c++-mode)
  (add-to-list 'semantic-default-submodes 'global-semantic-stickyfunc-mode)
  ;; (add-to-list 'semantic-default-submodes 'global-semantic-idle-summary-mode)
  ;; 默认semantic快捷键以C-c ,为前缀
  (define-key semantic-mode-map (kbd "C-c ,") nil))
;; ===================semantic==================
;;; hs-minor-mode
;; ==================hs-minor-mode==============
(use-package hideshow
  :diminish hs-minor-mode
  :commands hs-toggle-hiding
  :init
  (dolist (hook '(c-mode-common-hook asm-mode-hook))
    (add-hook hook (lambda ()
                     (local-set-key (kbd "C-c C-`") 'hs-toggle-hiding))))
  :config
  (define-key hs-minor-mode-map (kbd "C-c C-`") 'hs-toggle-hiding))
;; ==================hs-minor-mode==============
;;; helm-gtags
;; ==================helm-gtags=================
(use-package helm-gtags
  :diminish helm-gtags-mode
  :commands (helm-gtags-mode
             helm-gtags-dwim
             helm-gtags-pop-stack
             helm-gtags-select)
  :init
  (dolist (hook '(c-mode-hook c++-mode-hook asm-mode-hook))
    (add-hook hook (lambda ()
                     (local-set-key (kbd "C-c C-,") 'helm-gtags-dwim)
                     (local-set-key (kbd "C-c C-.") 'helm-gtags-pop-stack)
                     (local-set-key (kbd "C-c C-/") 'helm-gtags-select))))
  :config
  (dolist (hook '(c-mode-hook c++-mode-hook asm-mode-hook))
    (add-hook hook 'helm-gtags-mode))
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (derived-mode-p 'c-mode 'c++-mode 'asm-mode)
        (helm-gtags-mode))))
  (defun helm-gtags--find-tag-simple/around (fn &rest args)
    (let ((helm-execute-action-at-once-if-one nil))
      (apply fn args)))
  (advice-add 'helm-gtags--find-tag-simple :around #'helm-gtags--find-tag-simple/around)
  (setq helm-gtags-ignore-case t
        helm-gtags-auto-update t
        helm-gtags-use-input-at-cursor t
        helm-gtags-pulse-at-cursor t
        helm-gtags-path-style 'root
        helm-gtags-suggested-key-mapping nil)
  (smartrep-define-key helm-gtags-mode-map "C-c"
    '(("C-," . helm-gtags-dwim)
      ("C-." . helm-gtags-pop-stack)
      ("C-/" . helm-gtags-select)
      ("," . helm-gtags-previous-history)
      ("." . helm-gtags-next-history)
      ("/" . helm-gtags-show-stack))))
;; ==================helm-gtags=================
;;; arduino
;; ===================arduino===================
(use-package arduino-mode
  :mode (("\\.pde\\'" . arduino-mode)
         ("\\.ino\\'" . arduino-mode)))
;; ===================arduino===================
;;; disaster
;; ==================disaster===================
(use-package disaster
  :commands disaster)
;; ==================disaster===================
;;; meghanada
;; =================meghanada===================
(use-package meghanada
  :delight '(:eval (propertize " M" 'face 'font-lock-function-name-face))
  :commands (meghanada-jump-declaration
             meghanada-back-jump
             meghanada-reference)
  :init
  (add-hook 'java-mode-hook (lambda ()
                              (bind-key "C-c C-," 'meghanada-jump-declaration java-mode-map)
                              (bind-key "C-c C-." 'meghanada-back-jump java-mode-map)
                              (bind-key "C-c C-/" 'meghanada-reference java-mode-map)))
  :config
  (add-hook 'java-mode-hook 'meghanada-mode)
  (dolist (buf (cl-remove-if-not (lambda (x)
                                   (equal (buffer-mode x) 'java-mode))
                                 (buffer-list)))
    (with-current-buffer buf
      (meghanada-mode)))
  ;; 默认快捷键以C-c C-r和C-c C-c开头
  (define-key meghanada-mode-map (kbd "C-c C-,") 'meghanada-jump-declaration)
  (define-key meghanada-mode-map (kbd "C-c C-.") 'meghanada-back-jump)
  (define-key meghanada-mode-map (kbd "C-c C-/") 'meghanada-reference)
  (define-key meghanada-mode-map (kbd "C-c C-f") 'meghanada-code-beautify)
  (define-key meghanada-mode-map (kbd "C-c c") 'meghanada-exec-main)
  (define-key meghanada-mode-map (kbd "C-M-,") nil)
  (define-key meghanada-mode-map (kbd "C-c C-<tab>") 'meghanada-switch-testcase))
;; =================meghanada===================
(provide 'setup_ccmode)
