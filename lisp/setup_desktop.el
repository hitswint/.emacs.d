;;; saveplace
;; ==================saveplace===================
(def-package! saveplace
  :config
  (if (fboundp 'save-place-mode)
      (save-place-mode)
    (setq-default save-place t))
  (setq save-place-forget-unreadable-files nil))
;; ==================saveplace===================
;;; savehist
;; ==================savehist====================
(def-package! savehist
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
(def-package! desktop
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
  (add-to-list 'desktop-minor-mode-table '(abbrev-mode nil))
  (add-to-list 'desktop-minor-mode-table '(auto-complete-mode nil))
  (add-to-list 'desktop-minor-mode-table '(drag-stuff-mode nil))
  (add-to-list 'desktop-minor-mode-table '(anzu-mode nil))
  (add-to-list 'desktop-minor-mode-table '(projectile-mode nil))
  (add-to-list 'desktop-minor-mode-table '(which-key-mode nil))
  (add-to-list 'desktop-minor-mode-table '(yas-minor-mode nil))
  (add-to-list 'desktop-minor-mode-table '(company-mode nil))
  (add-to-list 'desktop-minor-mode-table '(global-auto-revert-mode nil))
  (add-to-list 'desktop-minor-mode-table '(auto-revert-mode nil))
  (add-to-list 'desktop-minor-mode-table '(flx-ido-mode nil))
  (add-to-list 'desktop-minor-mode-table '(helm-mode nil))
  (add-to-list 'desktop-minor-mode-table '(helm-ff-cache-mode nil))
  (add-to-list 'desktop-minor-mode-table '(aggressive-indent-mode nil))
  (add-to-list 'desktop-minor-mode-table '(elisp-slime-nav-mode nil))
  (add-to-list 'desktop-minor-mode-table '(rainbow-delimiters-mode nil))
  (add-to-list 'desktop-minor-mode-table '(paredit-mode nil))
  (add-to-list 'desktop-minor-mode-table '(TeX-PDF-mode nil))
  (add-to-list 'desktop-minor-mode-table '(org-cdlatex-mode nil))
  (add-to-list 'desktop-minor-mode-table '(org-indent-mode nil))
  (add-to-list 'desktop-minor-mode-table '(iimage-mode nil))
  (add-to-list 'desktop-minor-mode-table '(wrap-region-mode nil))
  (add-to-list 'desktop-minor-mode-table '(paredit-everywhere-mode nil))
  (add-to-list 'desktop-minor-mode-table '(flycheck-mode nil))
  (add-to-list 'desktop-minor-mode-table '(flycheck-pos-tip-mode nil))
  (add-to-list 'desktop-minor-mode-table '(flyspell-mode nil))
  (add-to-list 'desktop-minor-mode-table '(auto-fill-mode nil))
  (add-to-list 'desktop-minor-mode-table '(auto-fill-function nil))
  (add-to-list 'desktop-minor-mode-table '(override-global-mode nil))
  (add-to-list 'desktop-minor-mode-table '(outline-minor-mode nil))
  (add-to-list 'desktop-minor-mode-table '(orgtbl-mode nil))
  (add-to-list 'desktop-minor-mode-table '(reftex-mode nil))
  (add-to-list 'desktop-minor-mode-table '(magic-latex-buffer nil))
  (add-to-list 'desktop-minor-mode-table '(hs-minor-mode nil))
  (add-to-list 'desktop-minor-mode-table '(helm-gtags-mode nil))
  (add-to-list 'desktop-minor-mode-table '(function-args-mode nil))
  (add-to-list 'desktop-minor-mode-table '(linum-mode nil))
  (add-to-list 'desktop-minor-mode-table '(ace-pinyin-mode nil))
  (add-to-list 'desktop-minor-mode-table '(hungry-delete-mode nil))
  (add-to-list 'desktop-minor-mode-table '(peep-dired nil))
  (add-to-list 'desktop-minor-mode-table '(global-image-dired-minor-mode nil))
  (add-to-list 'desktop-minor-mode-table '(ivy-mode nil))
  (add-to-list 'desktop-minor-mode-table '(clipmon-mode nil))
  (add-to-list 'desktop-minor-mode-table '(ispell-minor-mode nil))
  (add-to-list 'desktop-minor-mode-table '(volatile-highlights-mode nil))
  (add-to-list 'desktop-minor-mode-table '(highlight-parentheses-mode nil))
  (add-to-list 'desktop-minor-mode-table '(auto-highlight-symbol-mode nil))
  (add-to-list 'desktop-minor-mode-table '(diff-hl-mode nil))
  (add-to-list 'desktop-minor-mode-table '(diff-hl-flydiff-mode nil))
  (add-to-list 'desktop-minor-mode-table '(diff-hl-dired-mode nil))
  (add-to-list 'desktop-minor-mode-table '(interleave-mode nil))
  (add-to-list 'desktop-minor-mode-table '(interleave-pdf-mode nil))
  (add-to-list 'desktop-minor-mode-table '(org-noter-doc-mode nil))
  (add-to-list 'desktop-minor-mode-table '(org-noter-notes-mode nil))
  (add-to-list 'desktop-minor-mode-table '(highlight-indentation-mode nil))
  (add-to-list 'desktop-minor-mode-table '(highlight-indentation-current-column-mode nil))
  (add-to-list 'desktop-minor-mode-table '(eldoc-mode nil))
  (add-to-list 'desktop-minor-mode-table '(rainbow-mode nil))
  (add-to-list 'desktop-minor-mode-table '(emmet-mode nil))
  (add-to-list 'desktop-minor-mode-table '(skewer-html-mode nil))
  (add-to-list 'desktop-minor-mode-table '(elpy-mode nil))
  (add-to-list 'desktop-minor-mode-table '(flymake-mode nil))
  (add-to-list 'desktop-minor-mode-table '(xclipmon-mode nil))
  (add-to-list 'desktop-minor-mode-table '(dired-async-mode nil))
  (add-to-list 'desktop-minor-mode-table '(dired-omit-mode nil))
  (add-to-list 'desktop-minor-mode-table '(outshine-mode nil))
  (add-to-list 'desktop-minor-mode-table '(meghanada-mode nil))
  (add-to-list 'desktop-minor-mode-table '(org-extra-emphasis-intraword-emphasis-mode nil))
  (add-to-list 'desktop-minor-mode-table '(all-the-icons-dired-mode nil))
  (add-to-list 'desktop-minor-mode-table '(lsp-bridge-mode nil)))
;; ===================desktop====================
(provide 'setup_desktop)
