;;; flyspell
;; ================flyspell==================
(use-package flyspell
  ;; Enabled at commands.
  :defer t
  :bind ("M-g f" . swint-toggle-flyspell-mode)
  :config
  (defun swint-toggle-flyspell-mode ()
    (interactive)
    (if flyspell-mode
        (flyspell-mode-off)
      (progn (flyspell-buffer)
             (flyspell-mode))))
  (define-key flyspell-mode-map (kbd "C-,") nil)
  (define-key flyspell-mode-map (kbd "C-.") nil)
  (define-key flyspell-mode-map (kbd "C-;") nil)
  (smartrep-define-key flyspell-mode-map "M-g"
    '(("M-p" . flyspell-goto-previous-error)
      ("M-n" . flyspell-goto-next-error)
      ("M-f" . helm-flyspell-correct)))
  (defun flyspell-goto-previous-error (arg)
    "Go to arg previous spelling error."
    (interactive "p")
    (while (not (= 0 arg))
      (let ((pos (point))
            (min (point-min)))
        (if (and (eq (current-buffer) flyspell-old-buffer-error)
                 (eq pos flyspell-old-pos-error))
            (progn
              (if (= flyspell-old-pos-error min)
                  ;; Goto beginning of buffer.
                  (progn
                    (message "Restarting from end of buffer")
                    (goto-char (point-max)))
                (backward-word 1))
              (setq pos (point))))
        ;; Seek the next error.
        (while (and (> pos min)
                    (let ((ovs (overlays-at pos))
                          (r '()))
                      (while (and (not r) (consp ovs))
                        (if (flyspell-overlay-p (car ovs))
                            (setq r t)
                          (setq ovs (cdr ovs))))
                      (not r)))
          (backward-word 1)
          (setq pos (point)))
        ;; Save the current location for next invocation.
        (setq arg (1- arg))
        (setq flyspell-old-pos-error pos)
        (setq flyspell-old-buffer-error (current-buffer))
        (goto-char pos)
        (if (= pos min)
            (progn
              (message "No more miss-spelled word!")
              (setq arg 0)))))))
;; ================flyspell==================
;;; ispell
;; =================ispell===================
(use-package ispell
  ;; Enabled after features.
  :after (flyspell ac-ispell)
  :commands ispell-word
  :config
  (setq ispell-program-name "aspell")
  (setq ispell-personal-dictionary (expand-file-name "~/.ispell"))
  (ispell-change-dictionary "american" t)
  (unless ispell-alternate-dictionary
    (setq ispell-alternate-dictionary (file-truename "~/.english-words"))))
;; =================ispell===================
;;; helm-flyspell
;; =============helm-flyspell================
(use-package helm-flyspell
  ;; Enabled at commands.
  :defer t
  :bind ("M-g M-f" . helm-flyspell-correct))
;; =============helm-flyspell================
(provide 'setup_flyspell)
