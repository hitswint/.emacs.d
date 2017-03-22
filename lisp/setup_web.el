;;; web-mode
;; ====================web=====================
(use-package web-mode
  ;; Enabled in modes.
  :defer t
  :mode (("\\.phtml\\'" . web-mode)
         ("\\.tpl\\.php\\'" . web-mode)
         ("\\.[agj]sp\\'" . web-mode)
         ("\\.as[cp]x\\'" . web-mode)
         ("\\.erb\\'" . web-mode)
         ("\\.mustache\\'" . web-mode)
         ("\\.html?\\'" . web-mode)
         ("\\.djhtml\\'" . web-mode))
  :config
  (setq web-mode-engines-alist
        '(("django"    . "\\.html\\'")
          ("django"  . "\\.djhtml\\.")))
  (define-key web-mode-map (kbd "C-c C-v") 'browse-url-of-buffer)
  ;; 在html-mode下，html或嵌入js/css文件的eval实时展现，与web-mode兼容不好。
  (define-key html-mode-map (kbd "C-c s") 'swint-run-skewer)
  (define-key web-mode-map (kbd "C-c s") 'swint-run-skewer)
  (defun swint-run-skewer (&optional arg)
    (interactive "P")
    (let ((skewer-declaration
           (format "<script src=\"http://localhost:%d/skewer\"></script>\n"
                   httpd-port)))
      (when arg (html-mode))
      (save-excursion
        (goto-char (point-min))
        (unless (re-search-forward skewer-declaration nil t)
          (insert skewer-declaration)))
      (httpd-start)
      (browse-url-of-buffer)))
  (smartrep-define-key web-mode-map "C-c"
    '(("n" . web-mode-element-next)
      ("p" . web-mode-element-previous)
      ("u" . web-mode-element-parent)
      ("d" . web-mode-element-child))))
;; ====================web=====================
;;; js2-mode
;; =================js2-mode===================
(use-package js2-mode
  ;; Enabled in modes.
  :defer t
  :mode ("\\.js\\'" . js2-mode)
  :config
  (add-hook 'js2-mode-hook 'js2-imenu-extras-mode)
  (define-key js2-mode-map (kbd "M-.") nil)
  (define-key js2-mode-map (kbd "C-c C-,") 'js2-jump-to-definition)
  ;; 在js2-mode下，run-skewer打开空白网页，eval实时展现。
  (define-key js2-mode-map (kbd "C-c s") 'run-skewer))
;; =================js2-mode===================
;;; skewer-mode
;; ================skewer-mode=================
(use-package skewer-mode
  ;; Enabled in modes.
  :defer t
  :commands (skewer-mode skewer-css-mode skewer-html-mode)
  :init
  (add-hook 'js2-mode-hook 'skewer-mode)
  (add-hook 'css-mode-hook 'skewer-css-mode)
  (add-hook 'html-mode-hook 'skewer-html-mode)
  (add-hook 'web-mode-hook 'skewer-html-mode))
;; ================skewer-mode=================
(provide 'setup_web)
