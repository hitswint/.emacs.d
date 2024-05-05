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
        history-length 100
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
        desktop-files-not-to-save "\\(\\`/[^/:]*:\\|(ftp)\\|/mnt/share/.*\\|/mnt/sshfs/.*\\|/mnt/usb/.*\\'\\)")
  (desktop-save-mode t)
  (advice-add 'desktop-buffer-info :around #'(lambda (fn buffer)
                                               (let ((desktop-minor-mode-table
                                                      (cl-loop for minor-mode in (buffer-local-value 'local-minor-modes buffer)
                                                               collect `(,minor-mode nil))))
                                                 (funcall fn buffer)))))
;; ===================desktop====================
(provide 'setup_desktop)
