;;; ccmode
;; ===================ccmode====================
(def-package! cc-mode
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
  (add-hook 'c-mode-common-hook
            (lambda ()
              (define-key c-mode-base-map (kbd "C-c C-c") 'c-compile-current-file)
              (define-key c-mode-base-map (kbd "C-c C-S-c") (lambda () (interactive)
                                                              (setq-local compilation-read-command nil)
                                                              (call-interactively 'compile)))
              (define-key c-mode-base-map (kbd "C-c d") 'disaster)
              (define-key c-mode-base-map (kbd "C-M-q") nil)
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
(def-package! gdb-mi
  :commands gdb-or-gud-go
  :init
  (add-hook 'c-mode-common-hook (lambda ()
                                  (local-set-key (kbd "C-c g") 'gdb-or-gud-go)))
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
(def-package! function-args
  :diminish function-args-mode
  :commands (moo-complete moo-jump-directory moo-jump-local fa-jump-maybe swint-fa-show)
  :init
  (dolist (hook '(c-mode-common-hook asm-mode-hook))
    (add-hook hook (lambda ()
                     (local-set-key (kbd "C-c u") 'moo-complete)
                     (local-set-key (kbd "C-c j") 'moo-jump-directory)
                     (local-set-key (kbd "C-c i") 'moo-jump-local)
                     (local-set-key (kbd "C-c o") 'swint-fa-show)
                     (local-set-key (kbd "C-j") 'fa-jump-maybe))))
  :config
  (function-args-mode 1) ;function-args-mode only trigger semantic-mode
  (defun swint-fa-show ()
    (interactive)
    (if (overlayp fa-overlay)
        (fa-abort)
      (fa-show))))
;; ==================function-args==============
;;; semantic
;; ===================semantic==================
(def-package! semantic
  :after function-args
  :config
  (semantic-mode 1)
  (setq semantic-idle-scheduler-idle-time 5)
  (semanticdb-enable-gnu-global-databases 'c-mode)
  (semanticdb-enable-gnu-global-databases 'c++-mode)
  ;; (add-to-list 'semantic-default-submodes 'global-semantic-idle-summary-mode)
  (add-to-list 'semantic-default-submodes 'global-semantic-stickyfunc-mode)
  ;; 默认semantic快捷键以C-c ,为前缀。
  (define-key semantic-mode-map (kbd "C-c ,") nil))
;; ===================semantic==================
;;; hs-minor-mode
;; ==================hs-minor-mode==============
(def-package! hideshow
  :diminish hs-minor-mode
  :commands hs-toggle-hiding
  :init
  (dolist (hook '(c-mode-common-hook asm-mode-hook))
    (add-hook hook (lambda ()
                     (local-set-key (kbd "C-c C-`") 'hs-toggle-hiding))))
  :config
  (hs-minor-mode 1)
  (define-key hs-minor-mode-map (kbd "C-c C-`") 'hs-toggle-hiding))
;; ==================hs-minor-mode==============
;;; helm-gtags
;; ==================helm-gtags=================
(def-package! helm-gtags
  :diminish helm-gtags-mode
  :commands (helm-gtags-mode
             helm-gtags-dwim
             helm-gtags-pop-stack
             helm-gtags-select)
  :init
  (dolist (hook '(c-mode-common-hook asm-mode-hook))
    (add-hook hook (lambda ()
                     (local-set-key (kbd "C-c C-,") 'helm-gtags-dwim)
                     (local-set-key (kbd "C-c C-.") 'helm-gtags-pop-stack)
                     (local-set-key (kbd "C-c C-/") 'helm-gtags-select))))
  :config
  (add-hook 'c-mode-common-hook 'helm-gtags-mode)
  (add-hook 'asm-mode-hook 'helm-gtags-mode)
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (derived-mode-p 'c-mode 'C++-mode 'asm-mode)
        (helm-gtags-mode))))
  (defun helm-gtags--find-tag-simple/around (fn &rest args)
    (let ((helm-execute-action-at-once-if-one nil))
      (apply fn args)))
  (advice-add 'helm-gtags--find-tag-simple :around #'helm-gtags--find-tag-simple/around)
  (setq helm-gtags-ignore-case t
        helm-gtags-auto-update t
        helm-gtags-use-input-at-cursor t
        helm-gtags-pulse-at-cursor t
        helm-gtags-prefix-key "\M-s"
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
(def-package! arduino-mode
  :mode (("\\.pde\\'" . arduino-mode)
         ("\\.ino\\'" . arduino-mode)))
;; ===================arduino===================
;;; disaster
;; ==================disaster===================
(def-package! disaster
  :commands disaster)
;; ==================disaster===================
(provide 'setup_ccmode)
