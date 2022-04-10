;;; Keybindings
;; =================Keybindings=================
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
          #'(lambda ()
             (define-key LaTeX-mode-map (kbd "$") 'insert-pair-math-bracket)))
(add-hook 'org-mode-hook
          #'(lambda ()
             (define-key org-mode-map (kbd "$") 'insert-pair-math-bracket)))
;; =================Keybindings=================
;;; wrap-region
;; ================wrap-region==================
(def-package! wrap-region
  :diminish wrap-region-mode
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
;;; rainbow-delimiters
;; ==============rainbow-delimiters=============
(def-package! rainbow-delimiters
  :commands rainbow-delimiters-mode
  :init
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'text-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'dired-mode-hook 'rainbow-delimiters-mode)
  ;; 在org-mode中打开rainbow会让org本身的highlight失效。
  (add-hook 'org-mode-hook 'rainbow-delimiters-mode-disable)
  :config
  (custom-set-faces '(rainbow-delimiters-depth-1-face ((t (:foreground "yellow"))))
                    '(rainbow-delimiters-depth-2-face ((t (:foreground "green"))))
                    '(rainbow-delimiters-depth-3-face ((t (:foreground "DeepSkyBlue1"))))
                    '(rainbow-delimiters-depth-4-face ((t (:foreground "#8b7500"))))
                    '(rainbow-delimiters-depth-5-face ((t (:foreground "#8b7500"))))
                    '(rainbow-delimiters-depth-6-face ((t (:foreground "#8b7500"))))
                    '(rainbow-delimiters-depth-7-face ((t (:foreground "#8b7500"))))
                    '(rainbow-delimiters-depth-8-face ((t (:foreground "#8b7500"))))
                    '(rainbow-delimiters-depth-9-face ((t (:foreground "#8b7500"))))
                    '(rainbow-delimiters-unmatched-face ((t (:foreground "red"))))))
;; ==============rainbow-delimiters=============
;;; highlight-parentheses
;; ============highlight-parentheses============
(def-package! highlight-parentheses
  :config
  (set-face-attribute 'highlight-parentheses-highlight nil :weight 'bold :strike-through t)
  (defun hl-paren-create-overlays ()
    (let ((fg highlight-parentheses-colors)
          (bg highlight-parentheses-background-colors)
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
        (dotimes (i 2) ;; Front and back.
          (push (make-overlay 0 0 nil t) hl-paren-overlays)
          (overlay-put (car hl-paren-overlays) 'font-lock-face attributes)))
      (setq hl-paren-overlays hl-paren-overlays)))
  (global-highlight-parentheses-mode))
;; ============highlight-parentheses============
(provide 'setup_parenthesis)
