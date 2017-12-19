;;; web-mode
;; ==================web-mode==================
(use-package web-mode
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
  ;; skewer与web-mode兼容性不好。
  (define-key web-mode-map (kbd "C-c s") 'swint-run-skewer)
  (define-key web-mode-map (kbd "C-c b") 'company-web-bootstrap+)
  (define-key web-mode-map (kbd "C-c C-v") 'browse-url-of-buffer))
;; ==================web-mode==================
;;; html-mode
;; =================html-mode==================
(use-package sgml-mode
  :commands swint-run-skewer
  :config
  (define-key html-mode-map (kbd "C-c s") 'swint-run-skewer)
  ;; 在html-mode下，使用skewer实现html/js/css代码的实时eval。
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
      (browse-url-of-buffer))))
;; =================html-mode==================
;;; js2-mode
;; =================js2-mode===================
(use-package js2-mode
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
  :commands (skewer-mode skewer-css-mode skewer-html-mode)
  :init
  (add-hook 'js2-mode-hook 'skewer-mode)
  (add-hook 'css-mode-hook 'skewer-css-mode)
  (add-hook 'html-mode-hook 'skewer-html-mode)
  (add-hook 'web-mode-hook 'skewer-html-mode))
;; ================skewer-mode=================
;;; emmet-mode
;; ================emmet-mode==================
(use-package emmet-mode
  :diminish emmet-mode
  :commands emmet-mode
  :init
  (add-hook 'html-mode-hook 'emmet-mode)
  (add-hook 'web-mode-hook 'emmet-mode)
  (add-hook 'css-mode-hook 'emmet-mode)
  :config
  (define-key emmet-mode-keymap (kbd "C-j") nil)
  (define-key emmet-mode-keymap (kbd "C-c v") 'emmet-preview)
  (smartrep-define-key emmet-mode-keymap "C-c"
    '(("M-m" . emmet-prev-edit-point)
      ("M-M" . emmet-next-edit-point))))
;; ================emmet-mode==================
(provide 'setup_web)
