;;; ivy
;; ===========ivy/swiper/counsel/hydra=============
(use-package ivy
  ;; Enabled after features.
  :defer t
  :commands ivy-set-actions
  :after (swiper counsel)
  :config
  (bind-key "M-s y" 'ivy-resume)
  (bind-key "C-h" 'ivy-avy ivy-minibuffer-map)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-height 10)
  (setq ivy-count-format "%d/%d "))
(use-package swiper
  ;; Enabled at commands.
  :defer t
  :bind (("M-s s" . swint-swiper)
         ("M-s S" . swiper-all)
         :map isearch-mode-map
         ("M-s s" . swiper-from-isearch))
  :config
  (defun swint-swiper ()
    (interactive)
    (let ((swint-swiper-current-thing
           (if (region-active-p)
               (buffer-substring (region-beginning) (region-end))
             (symbol-name-at-point))))
      (deactivate-mark)
      (swiper swint-swiper-current-thing))))
(use-package counsel
  ;; Enabled at commands.
  :defer t
  ;; 按键逻辑：helm(C-x c x)/counsel(M-s c x)。
  :bind (("M-X" . counsel-M-x)
         ("C-x C-r" . swint-counsel-history)
         ("M-s `" . counsel-tmm)
         ("M-s c u" . counsel-unicode-char)
         ("M-s c l" . counsel-locate)
         ("M-s c i" . counsel-imenu)
         ("M-s c C-x C-f" . counsel-find-file)
         ("M-s c o" . counsel-outline)
         ("M-s c d" . counsel-dpkg)
         ("M-s c g" . counsel-ag)
         ("M-s c p" . counsel-list-processes)
         ("M-s c M-y" . counsel-yank-pop))
  :config
  (defun swint-counsel-history ()
    "List command history based on major-mode."
    (interactive)
    (cond
     ((memq major-mode '(shell-mode inferior-python-mode inferior-octave-mode))
      (call-interactively 'counsel-shell-history))
     ((eq major-mode 'eshell-mode)
      (call-interactively 'counsel-esh-history))
     (t (call-interactively 'swint-counsel-bash-history))))
  (defun swint-counsel-bash-history ()
    "Insert the bash history."
    (interactive)
    (let (hist-cmd collection val)
      (shell-command "history -r") ; reload history
      (setq collection
            (nreverse
             (split-string (with-temp-buffer (insert-file-contents (file-truename "~/.bash_history"))
                                             (buffer-string))
                           "\n" t)))
      (when (and collection (> (length collection) 0)
                 (setq val (if (= 1 (length collection)) (car collection)
                             (ivy-read (format "Bash history:") collection))))
        (insert val)))))
(use-package ivy-hydra
  ;; Enabled after features.
  :defer t
  :after ivy)
(use-package hydra
  ;; Enabled after features.
  :defer t
  :after ivy-hydra)
;; ===========ivy/swiper/counsel/hydra=============
(provide 'setup_ivy)
