;; ===========================auto-complete============================
(use-package auto-complete
  ;; Enabled at commands.
  :defer t
  :init
  (bind-key "M-U" '(lambda ()
                     (interactive)
                     (unless (if (boundp 'auto-complete-mode)
                                 auto-complete-mode)
                       (auto-complete-mode 1))
                     (if (boundp 'company-mode)
                         (company-abort))
                     (auto-complete)))
  ;; =================ac-ispell=================
  ;; Completion words longer than 4 characters
  (use-package ac-ispell
    ;; Enabled at commands.
    :defer t
    :init
    (bind-key "M-u" 'swint-auto-complete-ispell)
    (defun swint-auto-complete-ispell ()
      (interactive)
      (unless (if (boundp 'auto-complete-mode)
                  auto-complete-mode)
        (auto-complete-mode 1))
      (unless (or (boundp 'ac-source-ispell)
                  (boundp 'ac-source-ispell-fuzzy))
        (ac-ispell-setup))
      (if (boundp 'company-mode)
          (company-abort))
      (auto-complete '(ac-source-ispell-fuzzy
                       ac-source-ispell))))
  ;; 也可以加下句，使用ac-source-dictionary补全单词。
  ;; (add-to-list 'ac-dictionary-files "~/.english-words")
  ;; =================ac-ispell=================
  :config
  (use-package auto-complete-config)
  (setq ac-auto-start nil)
  (setq ac-use-menu-map t)
  (setq ac-fuzzy-enable t)
  (define-key ac-completing-map "\C-p" 'ac-previous)
  (define-key ac-completing-map "\C-n" 'ac-next)
  ;; (ac-set-trigger-key "TAB")              ;导致无法indent
  ;; ;; 取消TAB绑定以适应yasnippet。TAB的默认作用有两个：
  ;; ;; 1. 延伸menu出现之前的默认选项。取消延伸默认选项，使用RET替代。
  ;; (define-key ac-completing-map (kbd "TAB") nil)
  ;; ;; 2. 切换menu的选项。取消TAB切换menu选项。
  ;; (define-key ac-menu-map (kbd "TAB") nil)
  ;; ============ac-modes============
  ;; hook的函数只在文件打开时运行，如果文件已经打开，那么hook的函数无效。
  ;; 在auto-complete激活名单中加入mode。
  ;; 与hook机制不同，即使该buffer已经打开，仍然起作用。
  (add-to-list 'ac-modes 'latex-mode)
  (add-to-list 'ac-modes 'org-mode)
  (add-to-list 'ac-modes 'octave-mode)
  (add-to-list 'ac-modes 'shell-mode)
  (add-to-list 'ac-modes 'eshell-mode)
  (add-to-list 'ac-modes 'graphviz-dot-mode)
  ;; =======================auto-complete-c-headers===========================
  (use-package auto-complete-c-headers
    ;; Enabled automatically.
    :config
    (add-hook 'c++-mode-hook 'ac-c-header-init)
    (add-hook 'c-mode-hook 'ac-c-header-init)
    (add-hook 'auto-complete-mode-hook '(lambda ()
                                          (if (memq (buffer-mode) '(c++-mode
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
  ;; =======================auto-complete-c-headers===========================
  ;; =======================auto-complete-clang===========================
  (use-package auto-complete-clang
    ;; Enabled automatically.
    :config
    (add-hook 'c-mode-common-hook 'ac-cc-mode-setup)
    (add-hook 'auto-complete-mode-hook '(lambda ()
                                          (if (memq (buffer-mode) '(c++-mode
                                                                    c-mode))
                                              (ac-cc-mode-setup))))
    (defun ac-cc-mode-setup ()
      (setq ac-sources
            (append '(ac-source-clang
                      ac-source-semantic) ac-sources)))
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
  ;; =======================auto-complete-clang===========================
  ;; ============================ac-auctex=========================
  (use-package auto-complete-auctex
    ;; Enabled automatically.
    :config
    (add-hook 'auto-complete-mode-hook '(lambda ()
                                          (if (memq (buffer-mode) '(latex-mode))
                                              (ac-auctex-setup)))))
  ;; lin上的ac-auctex会自动启闭latex-math-mode，造成`输入公式的方法失效，两种解决方法：
  ;; 1. 去掉(init . LaTeX-math-mode)项，这样会导致有时ac失效。
  ;; 2. 定义如下函数代替原函数中的初始化，由(init . LaTeX-math-mode)变成(init . swint-latex-math-mode)。
  ;; 结果，frac输入有问题，放弃使用。
  ;; win中不存在这个问题，没有修改，在tex中使用ac-auctex。
  ;; lin上emacs升级新版本之后解决上述问题，而ac-math在tex中自动补全选项有长有短，最后在tex中使用ac-auctex。
  ;; ============================ac-auctex=========================
  ;; =====================ac-math=========================
  ;; ac-math中输入一个字母ac就开启的问题是因为所有\之后的字母都被识别做ac-source中的math、unicode和commands，忽略了中间的空格。
  ;; 即$/alpha$中/alpla识别为公式，$\alpha beta$中的beta也被识别为公式，而且因为前面的字符数足够，只要输入b就启动ac。
  ;; 在于ac-source-math-unicode/ac-source-math-latex/ac-source-latex-commands中的prefix正则表达式匹配问题。
  ;; 修改ac-math.el原文件，使用[a-z0-9A-Z]代替原来的.任意字符，去掉空格的影响。
  ;; 这样公式环境中，不以\开头的字符，识别为普通字符。
  ;; 注释掉下列，在tex中不使用ac-math。
  ;; (add-to-list 'ac-modes 'latex-mode)   ; make auto-complete aware of `latex-mode`
  ;; (defun ac-latex-mode-setup ()         ; add ac-sources to default ac-sources
  ;;   (setq ac-sources
  ;;         (append '(
  ;;                   ;; ac-source-math-unicode ;在latex中禁用unicode输入，因为array被当作了非math环境，会默认输入unicode。
  ;;                   ;; 似乎是因为如果array写到下一行的话，无法识别公式环境。
  ;;                   ac-source-math-latex
  ;;                   ac-source-latex-commands
  ;;                   )
  ;;                 ac-sources)))
  ;; (add-hook 'LaTeX-mode-hook 'ac-latex-mode-setup)
  ;; (setq ac-math-unicode-in-math-p t)      ;在latex的math环境中激活unicode输入
  ;; 在org-mode中使用ac-math激活unicode输入
  (use-package ac-math
    ;; Enabled automatically.
    :config
    (add-hook 'org-mode-hook 'ac-org-mode-setup)
    (add-hook 'auto-complete-mode-hook '(lambda ()
                                          (if (memq (buffer-mode) '(org-mode))
                                              (ac-org-mode-setup))))
    (defun ac-org-mode-setup ()
      (add-to-list 'ac-sources 'ac-source-math-unicode)))
  ;; =====================ac-math=========================
  ;; ============================ac-octave=========================
  ;; ac-octave.el里面并没有定义关键词，似乎是通过和octave的沟通来补全，需要打开octave。
  ;; (require 'ac-octave)
  ;; ;; octave-mode中使用
  ;; (add-to-list 'ac-modes 'octave-mode)
  ;; (defun ac-octave-mode-setup ()
  ;;   (setq ac-sources '(ac-source-octave)))
  ;; (add-hook 'octave-mode-hook
  ;;           '(lambda () (ac-octave-mode-setup)))
  ;; ;; inferior-octave中使用
  ;; (add-to-list 'ac-modes 'inferior-octave-mode)
  ;; (add-hook 'inferior-octave-mode
  ;;           '(lambda () (ac-octave-mode-setup)))
  ;; matlab-mode中使用
  ;; (defun ac-matlab-mode-setup ()
  ;;   (add-to-list 'ac-sources 'ac-source-octave))
  ;; (add-hook 'matlab-mode-hook 'ac-matlab-mode-setup)
  ;; ============================ac-octave=========================
  ;; =========================auto-complete-octave=========================
  ;; ac-octave有问题，使用auto-complete-octave
  (use-package auto-complete-octave
    ;; Enabled automatically.
    :load-path "site-lisp/auto-complete-octave/"
    :config
    (add-hook 'auto-complete-mode-hook '(lambda ()
                                          (if (memq (buffer-mode) '(octave-mode))
                                              (add-to-list 'ac-sources 'ac-source-octave)))))
  ;; =========================auto-complete-octave=========================
  ;; =================shell==================
  ;; 下面这句会导致octave运行时emacs hang
  ;; (setq comint-process-echoes t)
  ;; prevent echoed commands from being printed (t)
  (use-package readline-complete
    ;; Enabled automatically.
    :config
    (setq comint-process-echoes nil)
    (add-hook 'shell-mode-hook 'ac-rlc-setup-sources)
    (add-hook 'auto-complete-mode-hook '(lambda ()
                                          (if (memq (buffer-mode) '(shell-mode))
                                              (ac-rlc-setup-sources)))))
  ;; =================shell==================
  ;; ===================eshell===================
  (use-package pcomplete
    ;; Enabled automatically.
    :config
    (add-hook 'shell-mode-hook 'pcomplete-shell-setup)
    (add-hook 'eshell-mode-hook 'ac-eshell-mode-setup)
    (add-hook 'auto-complete-mode-hook '(lambda ()
                                          (if (memq (buffer-mode) '(eshell-mode))
                                              (ac-eshell-mode-setup))))
    (defun ac-eshell-mode-setup ()
      (add-to-list 'ac-sources 'ac-source-eshell-pcomplete))
    (defvar ac-source-eshell-pcomplete
      '((candidates . (pcomplete-completions)))))
  ;; ===================eshell===================
  ;; 上述auto-complete-mode-hook函数在ac打开时运行。
  ;; 将初始化语句放在最后，使global-auto-complete-mode打开时加载上述设定，即使某buffer已经打开。
  (ac-config-default)
  (setq-default ac-sources '(ac-source-abbrev
                             ac-source-dictionary
                             ac-source-words-in-same-mode-buffers
                             ac-source-files-in-current-dir)))
;; ===========================auto-complete============================
;; ==============hippie-expand===================
(use-package hippie-exp
  ;; Enabled at commands.
  :defer t
  :bind ("C-x M-u" . hippie-expand)
  :config
  ;; 打开.english-words方式进行补全
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
    ;; old is true if we have already attempted an expansion
    (unless (bound-and-true-p ispell-minor-mode)
      (ispell-minor-mode 1))
    ;; english-words.txt is the fallback dicitonary
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
;; ==============hippie-expand===================
(provide 'setup_completion)
