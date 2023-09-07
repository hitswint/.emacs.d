;;; saveplace
;; ==================saveplace===================
(use-package saveplace
  :config
  (if (fboundp 'save-place-mode)
      (save-place-mode)
    (setq-default save-place t))
  (setq save-place-forget-unreadable-files nil))
;; ==================saveplace===================
;;; savehist
;; ==================savehist====================
(use-package savehist
  :config
  (savehist-mode t)
  (setq enable-recursive-minibuffers t
        history-length 1000
        savehist-additional-variables '(mark-ring
                                        global-mark-ring
                                        kill-ring
                                        search-ring
                                        regexp-search-ring
                                        extended-command-history
                                        bibtex-completion-bibliography/curr)
        savehist-autosave-interval 60))
;; ==================savehist====================
;;; desktop
;; ===================desktop====================
(use-package desktop
  :config
  (add-to-list 'desktop-modes-not-to-save 'Info-mode)
  (add-to-list 'desktop-modes-not-to-save 'info-lookup-mode)
  (add-to-list 'desktop-modes-not-to-save 'fundamental-mode)
  (setq desktop-path '("~/.emacs.d/")
        desktop-dirname "~/.emacs.d/"
        desktop-base-file-name "emacs-desktop"
        desktop-restore-frames nil
        desktop-load-locked-desktop t
        desktop-save t
        desktop-auto-save-timeout 5
        desktop-files-not-to-save "\\(\\`/[^/:]*:\\|(ftp)\\|/mnt/share/.*\\|/mnt/sshfs/.*\\'\\)")
  (desktop-save-mode t)
  ;; Minor modes not to be saved.
  (setq desktop-minor-mode-table (append desktop-minor-mode-table
                                         '((abbrev-mode nil)
                                           (auto-complete-mode nil)
                                           (drag-stuff-mode nil)
                                           (anzu-mode nil)
                                           (projectile-mode nil)
                                           (which-key-mode nil)
                                           (yas-minor-mode nil)
                                           (company-mode nil)
                                           (global-auto-revert-mode nil)
                                           (auto-revert-mode nil)
                                           (flx-ido-mode nil)
                                           (helm-mode nil)
                                           (helm-ff-cache-mode nil)
                                           (aggressive-indent-mode nil)
                                           (elisp-slime-nav-mode nil)
                                           (rainbow-delimiters-mode nil)
                                           (paredit-mode nil)
                                           (TeX-PDF-mode nil)
                                           (org-cdlatex-mode nil)
                                           (org-indent-mode nil)
                                           (iimage-mode nil)
                                           (wrap-region-mode nil)
                                           (paredit-everywhere-mode nil)
                                           (flycheck-mode nil)
                                           (flycheck-pos-tip-mode nil)
                                           (flyspell-mode nil)
                                           (auto-fill-mode nil)
                                           (auto-fill-function nil)
                                           (override-global-mode nil)
                                           (outline-minor-mode nil)
                                           (orgtbl-mode nil)
                                           (reftex-mode nil)
                                           (magic-latex-buffer nil)
                                           (hs-minor-mode nil)
                                           (helm-gtags-mode nil)
                                           (function-args-mode nil)
                                           (linum-mode nil)
                                           (ace-pinyin-mode nil)
                                           (hungry-delete-mode nil)
                                           (peep-dired nil)
                                           (global-image-dired-minor-mode nil)
                                           (ivy-mode nil)
                                           (clipmon-mode nil)
                                           (ispell-minor-mode nil)
                                           (volatile-highlights-mode nil)
                                           (highlight-parentheses-mode nil)
                                           (auto-highlight-symbol-mode nil)
                                           (diff-hl-mode nil)
                                           (diff-hl-flydiff-mode nil)
                                           (diff-hl-dired-mode nil)
                                           (interleave-mode nil)
                                           (interleave-pdf-mode nil)
                                           (org-noter-doc-mode nil)
                                           (org-noter-notes-mode nil)
                                           (highlight-indentation-mode nil)
                                           (highlight-indentation-current-column-mode nil)
                                           (eldoc-mode nil)
                                           (rainbow-mode nil)
                                           (emmet-mode nil)
                                           (skewer-html-mode nil)
                                           (elpy-mode nil)
                                           (flymake-mode nil)
                                           (xclipmon-mode nil)
                                           (dired-async-mode nil)
                                           (dired-omit-mode nil)
                                           (outshine-mode nil)
                                           (meghanada-mode nil)
                                           (org-extra-emphasis-intraword-emphasis-mode nil)
                                           (all-the-icons-dired-mode nil)
                                           (lsp-bridge-mode nil)
                                           (pdf-occur-dired-minor-mode nil)
                                           (company-quickhelp-local-mode nil)
                                           (visible-mark-mode nil)
                                           (auto-mark-mode nil)
                                           (dired-hide-details-mode nil)
                                           (org-appear-mode nil)
                                           (semantic-idle-scheduler-mode nil)
                                           (font-lock-mode nil)))))
;; ===================desktop====================
(provide 'setup_desktop)
