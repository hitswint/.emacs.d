;; =======================rainbow-delimiters==========================
(require 'rainbow-delimiters)
(add-hook 'dired-mode-hook 'rainbow-delimiters-mode)
(add-hook 'octave-mode-hook 'rainbow-delimiters-mode)
;; 在org-mode中打开rainbow会让org本身的highlight失效
;; (add-hook 'org-mode-hook 'rainbow-delimiters-mode)
(add-hook 'gnuplot-mode-hook 'rainbow-delimiters-mode)
(add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)
(add-hook 'c-mode-hook 'rainbow-delimiters-mode)
(add-hook 'graphviz-dot-mode-hook 'rainbow-delimiters-mode)
;; (global-rainbow-delimiters-mode)
;; (custom-set-faces
;;  ;; custom-set-faces was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(completions-common-part ((t (:inherit default :foreground "red"))))
;;  '(diredp-compressed-file-suffix ((t (:foreground "#7b68ee"))))
;;  '(diredp-ignored-file-name ((t (:foreground "#aaaaaa"))))
;;  '(rainbow-delimiters-depth-1-face ((t (:foreground "yellow"))))
;;  '(rainbow-delimiters-depth-2-face ((t (:foreground "green"))))
;;  '(rainbow-delimiters-depth-3-face ((t (:foreground "DeepSkyBlue1"))))
;;  '(rainbow-delimiters-depth-4-face ((t (:foreground "#8b7500"))))
;;  '(rainbow-delimiters-depth-5-face ((t (:foreground "#8b7500"))))
;;  '(rainbow-delimiters-depth-6-face ((t (:foreground "#8b7500"))))
;;  '(rainbow-delimiters-depth-7-face ((t (:foreground "#8b7500"))))
;;  '(rainbow-delimiters-depth-8-face ((t (:foreground "#8b7500"))))
;;  '(rainbow-delimiters-depth-9-face ((t (:foreground "#8b7500"))))
;;  '(rainbow-delimiters-unmatched-face ((t (:foreground "red"))))
;;  '(show-paren-match ((((class color) (background light)) (:background "azure2")))))
;; =======================rainbow-delimiters==========================
(provide 'setup_rainbow_delimiters)
