;;; paredit
;; =================paredit=====================
(use-package paredit
  :diminish paredit-mode
  :commands enable-paredit-mode
  :init
  (add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)
  :config
  (defun paredit-wrap-round-from-behind ()
    (interactive)
    (forward-sexp -1)
    (paredit-wrap-round))
  (defun paredit-wrap-square-from-behind ()
    (interactive)
    (forward-sexp -1)
    (paredit-wrap-square))
  (defun paredit-wrap-curly-from-behind ()
    (interactive)
    (forward-sexp -1)
    (paredit-wrap-curly))
  (defun paredit-wrap-angled-from-behind ()
    (interactive)
    (forward-sexp -1)
    (paredit-wrap-angled))
  (defun paredit-singlequote (&optional n)
    "Insert a pair of single-quotes."
    (interactive "P")
    (cond ((paredit-in-string-p)
           (if (eq (cdr (paredit-string-start+end-points))
                   (point))
               (forward-char)
             (insert ?\\ ?\' )))
          ((paredit-in-comment-p)
           (insert ?\' ))
          ((not (paredit-in-char-p))
           (paredit-insert-pair n ?\' ?\' 'paredit-forward-for-quote))))
  (defun paredit-meta-singlequote (&optional n)
    "Move to the end of the string."
    (interactive "P")
    (if (not (paredit-in-string-p))
        (paredit-singlequote (or n (and (not (paredit-region-active-p)) 1)))
      (goto-char (paredit-enclosing-string-end))))
  (defun swint-backward-kill-word ()
    (interactive)
    (if (and clean-aindent-mode
             (not (save-excursion (re-search-backward "[^[:space:]\n]" (point-at-bol) t))))
        (call-interactively 'clean-aindent--bsunindent)
      (call-interactively 'paredit-backward-kill-word)))
  (define-key paredit-mode-map (kbd "M-s") nil)
  (define-key paredit-mode-map (kbd "C-j") nil)
  (define-key paredit-mode-map (kbd "M-r") nil)
  (define-key paredit-mode-map (kbd "M-J") nil)
  (define-key paredit-mode-map (kbd "M-K") nil)
  (define-key paredit-mode-map (kbd "M-?") nil)
  (define-key paredit-mode-map (kbd "M-\"") nil)
  (define-key paredit-mode-map (kbd "M-DEL") 'swint-backward-kill-word)
  (define-key paredit-mode-map (kbd "M-g (") 'paredit-backward-slurp-sexp)
  (define-key paredit-mode-map (kbd "M-g )") 'paredit-forward-slurp-sexp)
  (define-key paredit-mode-map (kbd "M-g {") 'paredit-backward-barf-sexp)
  (define-key paredit-mode-map (kbd "M-g }") 'paredit-forward-barf-sexp)
  (define-key paredit-mode-map (kbd "M-s (") 'paredit-wrap-round)
  (define-key paredit-mode-map (kbd "M-s )") 'paredit-wrap-round-from-behind)
  (define-key paredit-mode-map (kbd "M-s [") 'paredit-wrap-square)
  (define-key paredit-mode-map (kbd "M-s ]") 'paredit-wrap-square-from-behind)
  (define-key paredit-mode-map (kbd "M-s {") 'paredit-wrap-curly)
  (define-key paredit-mode-map (kbd "M-s }") 'paredit-wrap-curly-from-behind)
  (define-key paredit-mode-map (kbd "M-s <") 'paredit-wrap-angled)
  (define-key paredit-mode-map (kbd "M-s >") 'paredit-wrap-angled-from-behind)
  (define-key paredit-mode-map (kbd "M-s '") 'paredit-meta-singlequote)
  (define-key paredit-mode-map (kbd "M-s \"") 'paredit-meta-doublequote)
  (define-key paredit-mode-map (kbd "M-S") 'paredit-split-sexp)
  (define-key paredit-mode-map (kbd "M-R") 'paredit-raise-sexp)
  (define-key paredit-mode-map (kbd "M-A") 'paredit-join-sexps)
  (define-key paredit-mode-map (kbd "M-D") 'paredit-splice-sexp))
;; (autoload 'enable-paredit-mode "paredit" t)
;; =================paredit=====================
;;; paredit-everywhere
;; ==============paredit-everything=============
(use-package paredit-everywhere
  :diminish paredit-everywhere-mode
  :commands paredit-everywhere-mode
  :init
  (add-hook 'prog-mode-hook 'paredit-everywhere-mode)
  (add-hook 'text-mode-hook 'paredit-everywhere-mode)
  :config
  (define-key paredit-everywhere-mode-map (kbd "M-s") nil)
  (define-key paredit-everywhere-mode-map (kbd "M-r") nil)
  (define-key paredit-everywhere-mode-map (kbd "M-d") nil)
  (define-key paredit-everywhere-mode-map (kbd "M-J") nil)
  (define-key paredit-everywhere-mode-map (kbd "M-K") nil)
  (define-key paredit-everywhere-mode-map (kbd "M-\"") nil)
  (define-key paredit-everywhere-mode-map (kbd "M-DEL") 'swint-backward-kill-word)
  (define-key paredit-everywhere-mode-map (kbd "M-g (") 'paredit-backward-slurp-sexp)
  (define-key paredit-everywhere-mode-map (kbd "M-g )") 'paredit-forward-slurp-sexp)
  (define-key paredit-everywhere-mode-map (kbd "M-g {") 'paredit-backward-barf-sexp)
  (define-key paredit-everywhere-mode-map (kbd "M-g }") 'paredit-forward-barf-sexp)
  (define-key paredit-everywhere-mode-map (kbd "M-s (") 'paredit-wrap-round)
  (define-key paredit-everywhere-mode-map (kbd "M-s )") 'paredit-wrap-round-from-behind)
  (define-key paredit-everywhere-mode-map (kbd "M-s [") 'paredit-wrap-square)
  (define-key paredit-everywhere-mode-map (kbd "M-s ]") 'paredit-wrap-square-from-behind)
  (define-key paredit-everywhere-mode-map (kbd "M-s {") 'paredit-wrap-curly)
  (define-key paredit-everywhere-mode-map (kbd "M-s }") 'paredit-wrap-curly-from-behind)
  (define-key paredit-everywhere-mode-map (kbd "M-s <") 'paredit-wrap-angled)
  (define-key paredit-everywhere-mode-map (kbd "M-s >") 'paredit-wrap-angled-from-behind)
  (define-key paredit-everywhere-mode-map (kbd "M-s '") 'paredit-meta-singlequote)
  (define-key paredit-everywhere-mode-map (kbd "M-s \"") 'paredit-meta-doublequote)
  (define-key paredit-everywhere-mode-map (kbd "M-S") 'paredit-split-sexp)
  (define-key paredit-everywhere-mode-map (kbd "M-R") 'paredit-raise-sexp)
  (define-key paredit-everywhere-mode-map (kbd "M-A") 'paredit-join-sexps)
  (define-key paredit-everywhere-mode-map (kbd "M-D") 'paredit-splice-sexp))
;; ==============paredit-everything=============
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
(use-package wrap-region
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
    (when (member left '("*" "~" "/" "=" "+" "_" "$" "!!" "@@" "%%" "&&"))
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
     ("$" "$" nil (org-mode latex-mode))
     ("!!" "!!" "!" org-mode)
     ("@@" "@@" "@" org-mode)
     ("%%" "%%" "%" org-mode)
     ("&&" "&&" "&" org-mode))))
;; ================wrap-region==================
;;; rainbow-delimiters
;; ==============rainbow-delimiters=============
(use-package rainbow-delimiters
  :commands rainbow-delimiters-mode
  :init
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'text-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'dired-mode-hook 'rainbow-delimiters-mode)
  ;; 在org-mode中打开rainbow会让org本身的highlight失效
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
(use-package highlight-parentheses
  :diminish highlight-parentheses-mode
  :config
  (set-face-attribute 'highlight-parentheses-highlight nil :weight 'bold :strike-through t)
  (setq highlight-parentheses-colors nil)
  (global-highlight-parentheses-mode))
;; ============highlight-parentheses============
(provide 'setup_parenthesis)
