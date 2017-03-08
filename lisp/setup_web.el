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
          ("django"  . "\\.djhtml\\."))))
;; ====================web=====================
;;; js2-mode
;; =================js2-mode===================
(use-package js2-mode
  ;; Enabled in modes.
  :defer t
  :mode ("\\.js\\'" . js2-mode)
  :config
  (add-hook 'js2-mode-hook 'js2-imenu-extras-mode))
;; =================js2-mode===================
(provide 'setup_web)
