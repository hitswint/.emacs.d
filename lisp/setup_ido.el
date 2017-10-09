;;; ido
;; =========================ido========================
(use-package ido
  ;; Enabled automatically.
  :config
  (ido-mode t)
  (setq ido-auto-merge-delay-time 0.7
        ido-default-buffer-method 'raise-frame
        ido-default-file-method 'raise-frame
        ido-enable-flex-matching t
        ido-file-extensions-order nil
        ido-separator "   "
        ido-use-virtual-buffers nil)
  (custom-set-faces '(ido-first-match ((t (:foreground "yellow" :weight bold))))
                    '(ido-only-match ((((class color)) (:foreground "DeepSkyBlue1" :weight bold))))
                    '(ido-subdir ((t (:foreground "green")))))
  (setq ido-ignore-buffers '("\\`Enjoy\\ Music\\'" "\\`\\*Inferior\\ Octave\\*\\'" "\\`\\*Ibuffer\\*\\'" "\\`\\*MATLAB\\*\\'" "\\`\\*shell\\*\\'" "\\`\\*calculator\\*\\'" "\\`\\*Calendar\\*\\'" "\\`\\*Compile\\=Log\\*\\'" "\\`\\*Completions\\*\\'" "\\`\\*sdcv\\*\\'" "\\`\\*scratch\\*\\'" "\\`\\*Process\\ List\\*\\'" "\\`\\*toc\\*\\'" "\\`\\*helm.*\\*\\'" "\\`\\*Helm.*\\*\\'" "\\`\\*buffer-selection\\*\\'" "\\`\\*Disabled\\ Command\\*\\'" "\\`\\*Mingus\\*\\'" "\\`\\*compilation\\*\\'" "\\`\\*Ido\\ Completions\\*\\'" "\\` " "\\`.english-words\\'")))
;; =========================ido========================
;;; ido-tips
;; ======================ido-tips======================
(use-package ido-completing-read+
  ;; Enabled after features.
  :defer t
  :after ido
  :config
  ;; (setq ido-everywhere t)
  (ido-ubiquitous-mode 1))
(use-package ido-vertical-mode
  ;; Enabled at idle.
  :defer 2
  :config
  (ido-vertical-mode 1)
  (setq ido-vertical-define-keys 'C-n-C-p-up-down-left-right))
(use-package ido-hacks
  ;; Enabled at idle.
  :defer 2)
(use-package flx-ido
  ;; Enabled at idle.
  :defer 2
  :config
  (setq ido-enable-flex-matching t)
  (flx-ido-mode 1))
(use-package ido-at-point
  ;; Enabled at idle.
  :defer 2
  :config
  (ido-at-point-mode))
;; ======================ido-tips======================
(provide 'setup_ido)
