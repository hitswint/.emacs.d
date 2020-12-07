;;; auto-complete
;; ================auto-complete===============
(def-package! auto-complete
  :diminish auto-complete-mode
  :bind ("M-u" . swint-auto-complete)
  :config
  (defun swint-auto-complete ()
    (interactive)
    (ignore-errors (company-abort))
    (unless auto-complete-mode
      (auto-complete-mode t))
    (auto-complete))
  :config
  (setq ac-auto-start nil)
  (setq ac-use-menu-map t)
  (setq ac-fuzzy-enable t)
  ;; (ac-set-trigger-key "TAB")
  (define-key ac-completing-map "\C-p" 'ac-previous)
  (define-key ac-completing-map "\C-n" 'ac-next)
;;;; eshell
  ;; ==================eshell==================
  ;; pcomplete会自动启动，造成打开eshell时读取ac-sources错误。
  ;; 放在auto-complete中，使其只有当ac开启时才能够执行。
  (def-package! pcomplete
    :after auto-complete
    :config
    (add-hook 'shell-mode-hook 'pcomplete-shell-setup)
    (add-hook 'auto-complete-mode-hook '(lambda ()
                                          (if (eq major-mode 'shell-mode)
                                              (pcomplete-shell-setup))))
    (add-hook 'eshell-mode-hook 'ac-eshell-mode-setup)
    (add-hook 'auto-complete-mode-hook '(lambda ()
                                          (if (eq major-mode 'eshell-mode)
                                              (ac-eshell-mode-setup))))
    (defun ac-eshell-mode-setup ()
      (add-to-list 'ac-sources 'ac-source-eshell-pcomplete))
    (defvar ac-source-eshell-pcomplete
      '((candidates . (pcomplete-completions)))))
  ;; ==================eshell==================
  )
;; ================auto-complete===============
;;; auto-complete-config
;; ===========auto-complete-config=============
(def-package! auto-complete-config
  :after auto-complete
  :config
  ;; ============ac-modes============
  (add-to-list 'ac-modes 'latex-mode)
  (add-to-list 'ac-modes 'org-mode)
  (add-to-list 'ac-modes 'octave-mode)
  (add-to-list 'ac-modes 'shell-mode)
  (add-to-list 'ac-modes 'eshell-mode)
  (add-to-list 'ac-modes 'gnuplot-mode)
  (add-to-list 'ac-modes 'graphviz-dot-mode)
  (add-to-list 'ac-modes 'arduino-mode)
  (ac-config-default)
  (setq-default ac-sources '(ac-source-abbrev
                             ac-source-dictionary
                             ac-source-words-in-same-mode-buffers
                             ac-source-files-in-current-dir)))
;; ===========auto-complete-config=============
;;; ac-ispell
;; ==================ac-ispell=================
(def-package! ac-ispell
  :bind ("M-U" . swint-auto-complete-ispell)
  :config
  (bind-key "M-U" 'hippie-expand ac-completing-map)
  ;; 使用ac-source-dictionary补全单词。
  ;; (add-to-list 'ac-dictionary-files "~/.english-words")
  (ac-ispell-setup)
  (defun swint-auto-complete-ispell (&optional arg)
    (interactive)
    (ignore-errors (company-abort))
    (require 'auto-complete)
    (unless auto-complete-mode
      (auto-complete-mode t))
    (cond
     ((equal last-command 'hippie-expand)
      (he-reset-string)
      (hippie-expand arg))
     ((equal last-command 'swint-auto-complete-ispell)
      (hippie-expand arg))
     (t (auto-complete '(ac-source-ispell-fuzzy
                         ac-source-ispell))))))
;; ==================ac-ispell=================
;;; auto-complete-c-headers
;; =========auto-complete-c-headers============
(def-package! auto-complete-c-headers
  :after auto-complete
  :config
  (add-hook 'c++-mode-hook 'ac-c-header-init)
  (add-hook 'c-mode-hook 'ac-c-header-init)
  (add-hook 'auto-complete-mode-hook '(lambda ()
                                        (if (memq major-mode '(c++-mode
                                                               c-mode))
                                            (ac-c-header-init))))
  (defun ac-c-header-init ()
    (add-to-list 'ac-sources 'ac-source-c-headers)
    (add-to-list 'achead:include-directories '"/usr/include/c++/4.9")
    (add-to-list 'achead:include-directories '"/usr/include/x86_64-linux-gnu/c++/4.9")
    (add-to-list 'achead:include-directories '"/usr/include/c++/4.9/backward")
    (add-to-list 'achead:include-directories '"/usr/lib/gcc/x86_64-linux-gnu/4.9/include")
    (add-to-list 'achead:include-directories '"/usr/local/include")
    (add-to-list 'achead:include-directories '"/usr/lib/gcc/x86_64-linux-gnu/4.9/include-fixed")
    (add-to-list 'achead:include-directories '"/usr/include/x86_64-linux-gnu")
    (add-to-list 'achead:include-directories '"/usr/include")))
;; =========auto-complete-c-headers============
;;; auto-complete-clang
;; ===========auto-complete-clang==============
(def-package! auto-complete-clang
  :after auto-complete
  :config
  (dolist (hook '(c-mode-hook c++-mode-hook))
    (add-hook hook 'ac-cc-mode-setup))
  (add-hook 'auto-complete-mode-hook '(lambda ()
                                        (if (memq major-mode '(c++-mode
                                                               c-mode))
                                            (ac-cc-mode-setup))))
  (defun ac-cc-mode-setup ()
    (setq ac-sources
          (append '(ac-source-clang
                    ac-source-semantic)
                  ac-sources)))
  (setq ac-clang-flags
        (mapcar (lambda (item)
                  (concat "-I" item))
                (split-string
                 "
/usr/include/c++/4.9
/usr/include/x86_64-linux-gnu/c++/4.9
/usr/include/c++/4.9/backward
/usr/lib/gcc/x86_64-linux-gnu/4.9/include
/usr/local/include
/usr/lib/gcc/x86_64-linux-gnu/4.9/include-fixed
/usr/include/x86_64-linux-gnu
/usr/include
"
                 ))))
;; ===========auto-complete-clang==============
;;; ac-auctex
;; ================ac-auctex===================
(def-package! auto-complete-auctex
  :after auto-complete
  :config
  (add-hook 'LaTeX-mode-hook 'ac-auctex-setup)
  (add-hook 'auto-complete-mode-hook '(lambda ()
                                        (if (eq major-mode 'latex-mode)
                                            (ac-auctex-setup)))))
;; ================ac-auctex===================
;;; ac-math
;; =================ac-math====================
(def-package! ac-math
  :after auto-complete
  :config
  (add-hook 'org-mode-hook 'ac-org-mode-setup)
  (add-hook 'auto-complete-mode-hook '(lambda ()
                                        (if (eq major-mode 'org-mode)
                                            (ac-org-mode-setup))))
  (defun ac-org-mode-setup ()
    (add-to-list 'ac-sources 'ac-source-math-unicode)))
;; =================ac-math====================
;;; shell
;; ===================shell====================
(def-package! readline-complete
  :after auto-complete
  :config
  (setq comint-process-echoes nil)
  (add-hook 'shell-mode-hook 'ac-rlc-setup-sources)
  (add-hook 'auto-complete-mode-hook '(lambda ()
                                        (if (eq major-mode 'shell-mode)
                                            (ac-rlc-setup-sources)))))
;; ===================shell====================
;;; company
;; ================company=====================
(def-package! company
  :diminish company-mode
  :after (:any company-try-hard yasnippet company-english-helper)
  :config
  (global-company-mode 1)
  (setq company-show-numbers t)
  (setq company-async-timeout 10)
  (define-key company-active-map (kbd "<tab>") nil)
  (define-key company-active-map (kbd "TAB") nil)
  (define-key company-active-map (kbd "C-p") 'company-select-previous)
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-search-map (kbd "C-p") 'company-select-previous)
  (define-key company-search-map (kbd "C-n") 'company-select-next)
  (dotimes (i 10)
    (define-key company-active-map (read-kbd-macro (format "C-%d" i)) 'company-complete-number)
    (define-key company-search-map (read-kbd-macro (format "C-%d" i)) 'company-complete-number))
  (setq company-backends (delete 'company-semantic company-backends))
  ;; To complete for projects, you need to tell Clang your include paths.
  ;; Create a file named .dir-locals.el at your project root:
  ;; ((nil . ((company-clang-arguments . ("-I/home/<user>/project_root/include1/"
  ;;                                      "-I/home/<user>/project_root/include2/")))))
  ;; If you use Helm, you can easily insert absolute path by C-c i at the current path in helm-find-files.
  )
;; ================company=====================
;;; company-try-hard
;; ==============company-try-hard==============
(def-package! company-try-hard
  :bind ("M-i" . company-try-hard)
  :config
  (define-key company-active-map (kbd "M-i") 'company-try-hard))
;; ==============company-try-hard==============
;;; company-quickhelp-mode
;; ===========company-quickhelp-mode===========
(def-package! company-quickhelp
  :after company
  :config
  (bind-key "C-o" 'company-quickhelp-manual-begin company-active-map)
  (setq company-quickhelp-delay nil)
  (company-quickhelp-mode 1))
;; ===========company-quickhelp-mode===========
;;; company-c-headers
;; =============company-c-headers==============
(def-package! company-c-headers
  :after company
  :config
  (add-to-list 'company-backends 'company-c-headers))
;; =============company-c-headers==============
;;; company-web
;; ===============company-web==================
(def-package! company-web
  :after company
  :config
  (add-to-list 'company-backends 'company-web-html)
  (add-to-list 'company-backends 'company-web-jade)
  (add-to-list 'company-backends 'company-web-slim))
;; ===============company-web==================
;;; ac-html-bootstrap
;; ============ac-html-bootstrap===============
(def-package! ac-html-bootstrap
  :commands company-web-bootstrap+
  :init
  (add-hook 'web-mode-hook (lambda ()
                             (bind-key "C-c i" 'company-web-bootstrap+ web-mode-map))))
;; ============ac-html-bootstrap===============
;;; company-english-helper
;; ==========company-english-helper============
(def-package! company-english-helper
  :load-path "site-lisp/company-english-helper/"
  :bind ("M-s M-u" . swint-company-english-helper-search)
  :config
  (defun swint-company-english-helper-search ()
    (interactive)
    (ignore-errors (company-abort))
    (call-interactively 'company-english-helper-search)))
;; ==========company-english-helper============
;;; hippie-expand
;; ==============hippie-expand=================
(def-package! hippie-exp
  :commands hippie-expand
  :config
  (setq hippie-expand-try-functions-list
        '(try-expand-by-dict
          ;; try-expand-dabbrev-all-buffers
          ;; try-complete-file-name-partially
          ;; try-complete-file-name
          ;; try-expand-all-abbrevs
          ;; try-expand-list
          ;; try-expand-line
          ;; try-expand-dabbrev
          ;; try-expand-dabbrev-from-kill
          ;; try-complete-lisp-symbol-partially
          ;; try-complete-lisp-symbol
          ))
  ;; Bin Chen写的补全单词函数，但无法使用ido界面，显示undo次数过多。
  (defun try-expand-by-dict (old)
    ;; Old is true if we have already attempted an expansion.
    (unless (bound-and-true-p ispell-minor-mode)
      (ispell-minor-mode 1))
    ;; English-words.txt is the fallback dicitonary.
    (if (not ispell-alternate-dictionary)
        (setq ispell-alternate-dictionary (file-truename "~/.english-words")))
    (let ((lookup-func (if (fboundp 'ispell-lookup-words)
                           'ispell-lookup-words
                         'lookup-words)))
      (unless old
        (he-init-string (he-lisp-symbol-beg) (point))
        (if (not (he-string-member he-search-string he-tried-table))
            (setq he-tried-table (cons he-search-string he-tried-table)))
        (setq he-expand-list
              (and (not (equal he-search-string ""))
                   (funcall lookup-func (concat (buffer-substring-no-properties (he-lisp-symbol-beg) (point)) "*")))))
      (if (null he-expand-list)
          (if old (he-reset-string))
        (he-substitute-string (car he-expand-list))
        (setq he-expand-list (cdr he-expand-list))
        t))))
;; ==============hippie-expand=================
;;; ycmd
;; ===================ycmd=====================
(def-package! ycmd
  :bind ("M-g y" . swint-toggle-ycmd)
  :init
  (setq ycmd-keymap-prefix (kbd "M-g M-y"))
  :config
  (set-variable 'ycmd-server-command `("python2" ,(file-truename "~/git-repo/Emacs/ycmd/ycmd")))
  (set-variable 'ycmd-global-config "~/git-repo/Emacs/ycmd/examples/.ycm_extra_conf.py")
  (defun swint-toggle-ycmd ()
    (interactive)
    (ycmd-mode 'toggle)
    (set (make-local-variable 'company-backends)
         (if (member 'company-ycmd company-backends)
             (delq 'company-ycmd (cl-remove-duplicates (mapcar #'identity company-backends)))
           (cons 'company-ycmd (cl-remove-duplicates (mapcar #'identity company-backends))))))
  (def-package! company-ycmd
    :commands company-ycmd
    :init
    (setq ycmd-min-num-chars-for-completion 1))
  (def-package! ycmd-eldoc
    :config
    (add-hook 'ycmd-mode-hook 'ycmd-eldoc-mode))
  (def-package! flycheck-ycmd
    :config
    (flycheck-ycmd-setup)))
;; ===================ycmd=====================
;;; yasnippet
;; =================yasnippet==================
(def-package! yasnippet
  :diminish yas-minor-mode
  :bind ("M-I" . swint-complete-yasnippet)
  :config
  (yas-global-mode 1)
  (defun swint-complete-yasnippet ()
    (interactive)
    (ignore-errors (company-abort))
    (require 'auto-complete)
    (unless auto-complete-mode
      (auto-complete-mode t))
    (unless (auto-complete '(ac-source-yasnippet))
      (call-interactively 'company-yasnippet))))
;; =================yasnippet==================
;;; auto-yasnippet
;; ===============auto-yasnippet===============
(def-package! auto-yasnippet
  :bind (("M-g i" . aya-create)
         ("M-g M-i" . aya-expand)))
;; ===============auto-yasnippet===============
(provide 'setup_completion)
