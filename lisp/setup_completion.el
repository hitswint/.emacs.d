;;; auto-complete
;; ================auto-complete===============
(use-package auto-complete
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
  ;; pcomplete会自动启动，造成打开eshell时读取ac-sources错误
  ;; 放在auto-complete中，使其只有当ac开启时才能够执行
  (use-package pcomplete
    :after auto-complete
    :config
    (add-hook 'shell-mode-hook 'pcomplete-shell-setup)
    (add-hook 'auto-complete-mode-hook #'(lambda ()
                                           (if (eq major-mode 'shell-mode)
                                               (pcomplete-shell-setup))))
    (add-hook 'eshell-mode-hook 'ac-eshell-mode-setup)
    (add-hook 'auto-complete-mode-hook #'(lambda ()
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
(use-package auto-complete-config
  :after auto-complete
  :config
  ;; ============ac-modes============
  (add-to-list 'ac-modes 'LaTeX-mode)
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
(use-package ac-ispell
  :commands swint-auto-complete-ispell
  :config
  (bind-key "M-U" 'hippie-expand ac-completing-map)
  ;; 使用ac-source-dictionary补全单词
  ;; (add-to-list 'ac-dictionary-files "~/.english-words")
  (ac-ispell-setup)
  (defun swint-auto-complete-ispell ()
    (interactive)
    (require 'auto-complete)
    (unless auto-complete-mode
      (auto-complete-mode t))
    (if (equal company-backend 'company-english-helper-search)
        (progn (ignore-errors (company-abort))
               (auto-complete '(ac-source-ispell-fuzzy
                                ac-source-ispell)))
      (ignore-errors (company-abort))
      (call-interactively 'company-english-helper-search))))
;; ==================ac-ispell=================
;;; auto-complete-c-headers
;; =========auto-complete-c-headers============
(use-package auto-complete-c-headers
  :after auto-complete
  :config
  (add-hook 'c++-mode-hook 'ac-c-header-init)
  (add-hook 'c-mode-hook 'ac-c-header-init)
  (add-hook 'auto-complete-mode-hook #'(lambda ()
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
(use-package auto-complete-clang
  :after auto-complete
  :config
  (dolist (hook '(c-mode-hook c++-mode-hook))
    (add-hook hook 'ac-cc-mode-setup))
  (add-hook 'auto-complete-mode-hook #'(lambda ()
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
(use-package auto-complete-auctex
  :after auto-complete
  :config
  (add-hook 'LaTeX-mode-hook 'ac-auctex-setup)
  (add-hook 'auto-complete-mode-hook #'(lambda ()
                                         (if (eq major-mode 'LaTeX-mode)
                                             (ac-auctex-setup)))))
;; ================ac-auctex===================
;;; ac-math
;; =================ac-math====================
(use-package ac-math
  :after auto-complete
  :config
  (add-hook 'org-mode-hook 'ac-org-mode-setup)
  (add-hook 'auto-complete-mode-hook #'(lambda ()
                                         (if (eq major-mode 'org-mode)
                                             (ac-org-mode-setup))))
  (defun ac-org-mode-setup ()
    (add-to-list 'ac-sources 'ac-source-math-unicode)))
;; =================ac-math====================
;;; shell
;; ===================shell====================
(use-package readline-complete
  :after auto-complete
  :config
  (setq comint-process-echoes nil)
  (add-hook 'shell-mode-hook 'ac-rlc-setup-sources)
  (add-hook 'auto-complete-mode-hook #'(lambda ()
                                         (if (eq major-mode 'shell-mode)
                                             (ac-rlc-setup-sources)))))
;; ===================shell====================
;;; company
;; ================company=====================
(use-package company
  :diminish company-mode
  :after (:any company-try-hard yasnippet company-english-helper)
  :config
  (global-company-mode 1)
  (setq company-show-quick-access t)
  (setq company-async-timeout 10)
  (define-key company-active-map (kbd "<tab>") nil)
  (define-key company-active-map (kbd "TAB") nil)
  (define-key company-active-map (kbd "C-p") 'company-select-previous-or-abort)
  (define-key company-active-map (kbd "C-n") 'company-select-next-or-abort)
  (define-key company-search-map (kbd "C-p") 'company-select-previous-or-abort)
  (define-key company-search-map (kbd "C-n") 'company-select-next-or-abort)
  (dotimes (i 10)
    (define-key company-active-map (read-kbd-macro (format "C-%d" i)) 'company-complete-tooltip-row)
    (define-key company-search-map (read-kbd-macro (format "C-%d" i)) 'company-complete-tooltip-row))
  (setq company-backends (delete 'company-semantic company-backends))
  (bind-key "M-U" 'swint-auto-complete-ispell company-active-map)
  ;; To complete for projects, you need to tell Clang your include paths.
  ;; Create a file named .dir-locals.el at your project root:
  ;; ((nil . ((company-clang-arguments . ("-I/home/<user>/project_root/include1/"
  ;;                                      "-I/home/<user>/project_root/include2/")))))
  ;; If you use Helm, you can easily insert absolute path by C-c i at the current path in helm-find-files.
  )
;; ================company=====================
;;; company-try-hard
;; ==============company-try-hard==============
(use-package company-try-hard
  :bind ("M-i" . company-try-hard)
  :config
  (define-key company-active-map (kbd "M-i") 'company-try-hard))
;; ==============company-try-hard==============
;;; company-quickhelp-mode
;; ===========company-quickhelp-mode===========
(use-package company-quickhelp
  :after company
  :config
  (bind-key "C-o" 'company-quickhelp-manual-begin company-active-map)
  (setq company-quickhelp-delay nil)
  (company-quickhelp-mode 1))
;; ===========company-quickhelp-mode===========
;;; company-c-headers
;; =============company-c-headers==============
(use-package company-c-headers
  :after company
  :config
  (add-to-list 'company-backends 'company-c-headers))
;; =============company-c-headers==============
;;; company-web
;; ===============company-web==================
(use-package company-web
  :after company
  :config
  (add-to-list 'company-backends 'company-web-html)
  (add-to-list 'company-backends 'company-web-jade)
  (add-to-list 'company-backends 'company-web-slim))
;; ===============company-web==================
;;; ac-html-bootstrap
;; ============ac-html-bootstrap===============
(use-package ac-html-bootstrap
  :commands company-web-bootstrap+
  :init
  (add-hook 'web-mode-hook (lambda ()
                             (bind-key "C-c i" 'company-web-bootstrap+ web-mode-map))))
;; ============ac-html-bootstrap===============
;;; company-english-helper
;; ==========company-english-helper============
(use-package company-english-helper
  :load-path "repos/company-english-helper/"
  :bind ("M-U" . swint-company-english-helper-search)
  :config
  (setq company-english-helper-fuzz-search-p nil)
  (defun swint-company-english-helper-search (&optional arg)
    (interactive)
    (cond
     ((equal last-command 'hippie-expand)
      (he-reset-string)
      (hippie-expand arg))
     ((equal last-command 'swint-company-english-helper-search)
      (hippie-expand arg))
     (t (call-interactively 'company-english-helper-search)))))
;; ==========company-english-helper============
;;; hippie-expand
;; ==============hippie-expand=================
(use-package hippie-exp
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
;;; yasnippet
;; =================yasnippet==================
(use-package yasnippet
  :diminish yas-minor-mode
  :bind ("M-I" . swint-complete-yasnippet)
  :config
  (yas-global-mode 1)
  (defun yas-ivy-prompt (prompt choices &optional display-fn)
    (yas-completing-prompt prompt choices display-fn #'(lambda (prompt collection
                                                                       &optional predicate require-match initial-input
                                                                       history def inherit-input-method)
                                                         (let ((no-match-required nil))
                                                           (ivy-completing-read prompt collection
                                                                                predicate no-match-required initial-input
                                                                                history def inherit-input-method)))))
  (setq yas-prompt-functions (cons #'yas-ivy-prompt yas-prompt-functions))
  (defun swint-complete-yasnippet ()
    (interactive)
    (ignore-errors (company-abort))
    (require 'auto-complete)
    (unless auto-complete-mode
      (auto-complete-mode t))
    (unless (auto-complete '(ac-source-yasnippet))
      (call-interactively 'company-yasnippet)))
  (add-hook 'cdlatex-tab-hook 'yas-expand)
  (add-hook 'cdlatex-tab-hook 'cdlatex-in-yas-field)
  (defun cdlatex-in-yas-field ()
    ;; Check if we're at the end of the Yas field
    (when-let* ((_ (overlayp yas--active-field-overlay))
                (end (overlay-end yas--active-field-overlay)))
      (if (>= (point) end)
          ;; Call yas-next-field if cdlatex can't expand here
          (let ((s (thing-at-point 'sexp)))
            (unless (and s (assoc (substring-no-properties s)
                                  cdlatex-command-alist-comb))
              (yas-next-field-or-maybe-expand)
              t))
        ;; otherwise expand and jump to the correct location
        (let (cdlatex-tab-hook minp)
          (setq minp
                (min (save-excursion (cdlatex-tab)
                                     (point))
                     (overlay-end yas--active-field-overlay)))
          (goto-char minp) t))))
  (bind-key "<tab>" 'yas-next-field-or-cdlatex yas-keymap)
  (defun yas-next-field-or-cdlatex ()
    (interactive)
    (if (or (bound-and-true-p cdlatex-mode)
            (bound-and-true-p org-cdlatex-mode))
        (cdlatex-tab)
      (yas-next-field-or-maybe-expand))))
(use-package yasnippet-snippets
  ;; 错误：byte-code: Recursive load
  ;; 删除snippets/bibtex-mode/.yas-setup.el中(require 'yasnippet-snippets)
  :after yasnippet)
;; =================yasnippet==================
;;; auto-yasnippet
;; ===============auto-yasnippet===============
(use-package auto-yasnippet
  :bind (("M-g M-i" . aya-expand)
         ("M-g M-I" . aya-create)))
;; ===============auto-yasnippet===============
(provide 'setup_completion)
