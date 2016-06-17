;;; Arduino
(use-package arduino-mode
  ;; Enabled in modes.
  :defer t
  :commands arduino-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.pde\\'" . arduino-mode))
  (add-to-list 'auto-mode-alist '("\\.ino\\'" . arduino-mode)))
(provide 'setup_arduino)
