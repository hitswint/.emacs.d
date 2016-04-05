;; ====================括号========================
;; (setq show-paren-style 'parenthesis)    ;highlight just brackets
;; (setq show-paren-style 'expression)     ;highlight entire bracket expression
;; (setq skeleton-pair t)                  ;自动插入匹配的括号
;; (global-set-key (kbd "(") 'skeleton-pair-insert-maybe)
;; (global-set-key (kbd "[") 'skeleton-pair-insert-maybe)
;; (global-set-key (kbd "{") 'skeleton-pair-insert-maybe)
;; (global-set-key (kbd "<") 'skeleton-pair-insert-maybe)
(defun insert-bracket-pair (leftBracket rightBracket)
  "Insert bracket pair automatically."
  (if (region-active-p)
      (let ((p1 (region-beginning))
            (p2 (region-end)))
        (goto-char p2)
        (insert rightBracket)
        (goto-char p1)
        (insert leftBracket)
        (goto-char (+ p2 2)))
    (progn
      (insert leftBracket rightBracket)
      (backward-char 1))))
(defun insert-bracket-pair-with-space (leftBracket rightBracket)
  "Insert bracket pair with space around automatically."
  (interactive)
  (if (or (char-equal (char-before) 32)
          (char-equal (char-before) 10))
      (insert leftBracket)
    (insert (concat " " leftBracket)))
  (if (or (char-equal (char-after) 32)
          (char-equal (char-after) 10))
      (progn (insert rightBracket)
             (backward-char 1))
    (progn (insert (concat rightBracket " "))
           (backward-char 2))))
(defun insert-pair-paren () (interactive) (insert-bracket-pair "(" ")"))
(defun insert-pair-bracket () (interactive) (insert-bracket-pair "[" "]"))
(defun insert-pair-brace () (interactive) (insert-bracket-pair "{" "}"))
(defun insert-pair-angle-bracket () (interactive) (insert-bracket-pair "<" ">"))
(defun insert-pair-double-angle-bracket () (interactive) (insert-bracket-pair "《" "》"))
(defun insert-pair-double-straight-quote () (interactive) (insert-bracket-pair "\"" "\""))
(defun insert-pair-single-straight-quote () (interactive) (insert-bracket-pair "'" "'"))
(defun insert-pair-double-curly-quote () (interactive) (insert-bracket-pair "“" "”"))
(defun insert-pair-single-curly-quote () (interactive) (insert-bracket-pair "‘" "’"))
(defun insert-pair-single-angle-quote‹› () (interactive) (insert-bracket-pair "‹" "›"))
(defun insert-pair-double-angle-quote«» () (interactive) (insert-bracket-pair "«" "»"))
(defun insert-pair-corner-bracket「」 () (interactive) (insert-bracket-pair "「" "」"))
(defun insert-pair-white-corner-bracket『』 () (interactive) (insert-bracket-pair "『" "』"))
(defun insert-pair-white-lenticular-bracket〖〗 () (interactive) (insert-bracket-pair "〖" "〗"))
(defun insert-pair-black-lenticular-bracket【】 () (interactive) (insert-bracket-pair "【" "】"))
(defun insert-pair-tortoise-shell-bracket〔〕 () (interactive) (insert-bracket-pair "〔" "〕"))
(defun insert-pair-math-bracket () (interactive) (insert-bracket-pair-with-space "$" "$"))
(global-set-key (kbd "(") 'insert-pair-paren)
(global-set-key (kbd "[") 'insert-pair-bracket)
(global-set-key (kbd "{") 'insert-pair-brace)
(global-set-key (kbd "<") 'insert-pair-angle-bracket)
(global-set-key (kbd "《") 'insert-pair-double-angle-bracket)
(global-set-key (kbd "\"") 'insert-pair-double-straight-quote)
(global-set-key (kbd "'") 'insert-pair-single-straight-quote)
(global-set-key (kbd "“") 'insert-pair-double-curly-quote)
(global-set-key (kbd "”") 'insert-pair-double-curly-quote)
(global-set-key (kbd "‘") 'insert-pair-single-curly-quote)
(global-set-key (kbd "’") 'insert-pair-single-curly-quote)
(add-hook 'LaTeX-mode-hook
          '(lambda ()
             (define-key LaTeX-mode-map (kbd "$") 'insert-pair-math-bracket)))
(add-hook 'org-mode-hook
          '(lambda ()
             (define-key org-mode-map (kbd "$") 'insert-pair-math-bracket)))
;; ================wrap-region==================
(use-package wrap-region
  ;; Enabled in modes.
  :defer t
  :commands wrap-region-mode
  :init
  (dolist (hook '(LaTeX-mode-hook
                  org-mode-hook))
    (add-hook hook 'wrap-region-mode))
  :config
  (add-hook 'wrap-region-before-wrap-hook 'wrap-region-add-space)
  (defun wrap-region-add-space ()
    "Add space around punctuations."
    (when (member left '("*" "~" "/" "=" "+" "_" "$"))
      (unless (or (char-equal (char-before (region-beginning)) 32)  ;空格
                  (char-equal (char-before (region-beginning)) 10)) ;回车
        (setq left (concat " " left)))
      (unless (or (char-equal (char-after (region-end)) 32)  ;空格
                  (char-equal (char-after (region-end)) 10)) ;回车
        (setq right (concat right " ")))))
  (wrap-region-add-wrappers
   '(("*" "*" nil org-mode)
     ("~" "~" nil org-mode)
     ("/" "/" nil org-mode)
     ("=" "=" nil org-mode)
     ("+" "+" nil org-mode)
     ("_" "_" nil org-mode)
     ("$" "$" nil (org-mode latex-mode)))))
;; ================wrap-region==================
;; ==============rainbow-delimiters=============
(use-package rainbow-delimiters
  ;; Enabled in modes.
  :defer t
  :commands rainbow-delimiters-mode
  :init
  ;; (global-rainbow-delimiters-mode)
  ;; 在org-mode中打开rainbow会让org本身的highlight失效。
  (dolist (hook '(;; org-mode-hook
                  dired-mode-hook
                  octave-mode-hook
                  gnuplot-mode-hook
                  emacs-lisp-mode-hook
                  c-mode-hook
                  graphviz-dot-mode-hook))
    (add-hook hook 'rainbow-delimiters-mode)))
;; ==============rainbow-delimiters=============
;; ============highlight-parentheses============
(use-package highlight-parentheses
  ;; Enabled in modes.
  :defer t
  :commands highlight-parentheses-mode
  :init
  (dolist (hook '(org-mode-hook
                  dired-mode-hook
                  octave-mode-hook
                  gnuplot-mode-hook
                  emacs-lisp-mode-hook
                  c-mode-hook
                  graphviz-dot-mode-hook))
    (add-hook hook 'highlight-parentheses-mode))
  :config
  (set-face-attribute 'hl-paren-face nil
                      :weight 'bold :strike-through t)
  (defun hl-paren-create-overlays ()
    (let ((fg hl-paren-colors)
          (bg hl-paren-background-colors)
          attributes)
      (while (or fg bg)
        (setq attributes (face-attr-construct 'hl-paren-face))
        ;; 取消改变括号前景颜色。
        ;; (when (car fg)
        ;;   (setq attributes (plist-put attributes :foreground (car fg))))
        (pop fg)
        (when (car bg)
          (setq attributes (plist-put attributes :background (car bg))))
        (pop bg)
        (dotimes (i 2) ;; front and back
          (push (make-overlay 0 0 nil t) hl-paren-overlays)
          (overlay-put (car hl-paren-overlays) 'font-lock-face attributes)))
      (setq hl-paren-overlays hl-paren-overlays))))
;; ============highlight-parentheses============
;; ====================括号========================
(provide 'setup_parenthesis)
