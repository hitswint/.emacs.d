;;; eaf
;; =====================eaf=====================
(use-package eaf
  :load-path "repos/emacs-application-framework/"
  :bind (("M-o a" . eaf-open)
         ("M-o M-a" . eaf-open-pdf-from-history)
         ("M-o b" . eaf-open-browser)
         ("M-o M-b" . eaf-open-browser-with-history)
         ("M-o M-RET" . eaf-open-pyqterminal))
  :config
  (pyvenv-activate-py3)
  (setq eaf-webengine-default-zoom "1.5")
  (define-key eaf-mode-map* (kbd "M-'") nil)
  (define-key eaf-mode-map* (kbd "M-/") nil)
  (define-key eaf-mode-map* (kbd "C-c d") 'eaf-duplicate-current-buffer)
  (define-key eaf-mode-map* (kbd "C-c s") 'eaf-get-buffer-screenshot)
  (define-key eaf-mode-map* (kbd "C-c f") 'eaf-toggle-fullscreen)
  (define-key eaf-mode-map* (kbd "C-c g") 'eaf-get-path-or-url)
  (define-key eaf-mode-map* (kbd "C-c a") 'eaf-share-path-or-url)
  (defun eaf-goto-left-tab ()
    (interactive)
    (if (bound-and-true-p awesome-tab-mode)
        (call-interactively 'awesome-tab-backward-tab)
      (let ((switch-to-prev-buffer-skip
             (lambda (_window buffer _bury-or-kill)
               (not (equal eaf--buffer-app-name
                           (buffer-local-value 'eaf--buffer-app-name buffer))))))
        (call-interactively 'previous-buffer))))
  (defun eaf-goto-right-tab ()
    (interactive)
    (if (bound-and-true-p awesome-tab-mode)
        (call-interactively 'awesome-tab-forward-tab)
      (let ((switch-to-prev-buffer-skip
             (lambda (_window buffer _bury-or-kill)
               (not (equal eaf--buffer-app-name
                           (buffer-local-value 'eaf--buffer-app-name buffer))))))
        (call-interactively 'next-buffer))))
  (defun eaf-translate-text (text)
    (swint-sdcv-to-tip text))
  (use-package eaf-browser
    :config
    (setq eaf-browser-default-search-engine "bing"
          eaf-browser-blank-page-url "https://www.bing.com"
          eaf-browser-dark-mode "follow")
    (defun eaf-toggle-proxy/around (fn)
      (if (not (string-empty-p eaf-proxy-host))
          (funcall fn)
        (setq eaf-proxy-host "127.0.0.1"
              eaf-proxy-port "7890"
              eaf-proxy-type "socks5")
        (eaf-restart-process)))
    (advice-add 'eaf-toggle-proxy :around #'eaf-toggle-proxy/around)
    (eaf-bind-key nil "M-," eaf-browser-keybinding)
    (eaf-bind-key nil "M-." eaf-browser-keybinding)
    (eaf-bind-key nil "M-s" eaf-browser-keybinding)
    (eaf-bind-key nil "M-o" eaf-browser-keybinding)
    (eaf-bind-key nil "M-O" eaf-browser-keybinding)
    (eaf-bind-key nil "C-0" eaf-browser-keybinding)
    (eaf-bind-key insert_or_history_backward "S" eaf-browser-keybinding)
    (eaf-bind-key insert_or_history_forward "D" eaf-browser-keybinding)
    (eaf-bind-key select_left_tab "M-p" eaf-browser-keybinding)
    (eaf-bind-key select_right_tab "M-n" eaf-browser-keybinding)
    (eaf-bind-key eaf-toggle-proxy "M-P" eaf-browser-keybinding)
    (eaf-bind-key copy_link "M-W" eaf-browser-keybinding)
    (eaf-bind-key new_blank_page "C-t" eaf-browser-keybinding)
    (eaf-bind-key toggle_password_autofill "M-t" eaf-browser-keybinding)
    (eaf-bind-key insert_or_recover_prev_close_page "X" eaf-browser-keybinding)
    (eaf-bind-key recover_prev_close_page "C-M-q" eaf-browser-keybinding)
    (eaf-bind-key edit_url "C-l" eaf-browser-keybinding)
    (eaf-bind-key insert_or_view_source "s" eaf-browser-keybinding)
    (eaf-bind-key insert_or_select_text "v" eaf-browser-keybinding))
  (use-package eaf-pdf-viewer
    :config
    (setq eaf-pdf-dark-mode "ignore"
          eaf-pdf-show-progress-on-page t)
    (eaf-bind-key nil "M-s" eaf-pdf-viewer-keybinding)
    (eaf-bind-key nil "i" eaf-pdf-viewer-keybinding)
    (eaf-bind-key toggle_inverted_mode "M-i" eaf-pdf-viewer-keybinding)
    (eaf-bind-key select_left_tab "M-p" eaf-pdf-viewer-keybinding)
    (eaf-bind-key select_right_tab "M-n" eaf-pdf-viewer-keybinding)
    (eaf-bind-key add_annot_highlight "M-a h" eaf-pdf-viewer-keybinding)
    (eaf-bind-key add_annot_underline "M-a u" eaf-pdf-viewer-keybinding)
    (eaf-bind-key add_annot_squiggly "M-a s" eaf-pdf-viewer-keybinding)
    (eaf-bind-key add_annot_strikeout_or_delete_annot "M-a d" eaf-pdf-viewer-keybinding)
    (eaf-bind-key add_annot_popup_text "M-a t" eaf-pdf-viewer-keybinding)
    (eaf-bind-key add_annot_inline_text "M-a T" eaf-pdf-viewer-keybinding)
    (eaf-bind-key edit_annot_text "M-a e" eaf-pdf-viewer-keybinding)
    (eaf-bind-key move_annot_text "M-a r" eaf-pdf-viewer-keybinding)
    (eaf-bind-key scroll_to_begin "M-<" eaf-pdf-viewer-keybinding)
    (eaf-bind-key scroll_to_end "M->" eaf-pdf-viewer-keybinding)
    (eaf-bind-key jump_to_page "M-g" eaf-pdf-viewer-keybinding))
  (use-package eaf-image-viewer)
  (use-package eaf-pyqterminal
    :config
    (eaf-bind-key eaf-send-key-sequence "M-p" eaf-pyqterminal-keybinding)
    (eaf-bind-key eaf-send-key-sequence "M-n" eaf-pyqterminal-keybinding)
    (eaf-bind-key eaf-send-key-sequence "C-x" eaf-pyqterminal-keybinding)
    (eaf-bind-key eaf-send-key-sequence "M-i" eaf-pyqterminal-keybinding)
    (eaf-bind-key eaf-send-key-sequence "M-m" eaf-pyqterminal-keybinding)
    (eaf-bind-key eaf-send-key-sequence "M-t" eaf-pyqterminal-keybinding)
    (eaf-bind-key eaf-send-key-sequence "M-z" eaf-pyqterminal-keybinding)
    (eaf-bind-key toggle_mark "C-;" eaf-pyqterminal-cursor-move-mode-keybinding)))
(use-package eaf-interleave
  :load-path "repos/emacs-application-framework/extension/"
  :commands (eaf-interleave-mode eaf-interleave-app-mode)
  :init
  ;; (add-hook 'eaf-browser-hook 'eaf-interleave-app-mode)
  (add-hook 'eaf-pdf-viewer-hook 'eaf-interleave-app-mode)
  :config
  (setq eaf-interleave-disable-narrowing t)
  (setq eaf-interleave-split-direction 'vertical)
  (setq eaf-interleave-split-lines 20)
  (define-key eaf-interleave-mode-map (kbd "C-M-p") 'eaf-interleave-sync-previous-note)
  (define-key eaf-interleave-mode-map (kbd "C-M-o") 'eaf-interleave-sync-current-note)
  (define-key eaf-interleave-mode-map (kbd "C-M-n") 'eaf-interleave-sync-next-note)
  (define-key eaf-interleave-app-mode-map (kbd "C-M-p") 'eaf-interleave-sync-previous-note)
  (define-key eaf-interleave-app-mode-map (kbd "C-M-o") 'eaf-interleave-sync-current-note)
  (define-key eaf-interleave-app-mode-map (kbd "C-M-n") 'eaf-interleave-sync-next-note)
  (define-key eaf-interleave-app-mode-map (kbd "i") 'eaf-interleave-add-note)
  (define-key eaf-interleave-app-mode-map (kbd "M-;") 'eaf-interleave-open-notes-file)
  (define-key eaf-interleave-app-mode-map (kbd "q") 'eaf-interleave-quit))
(use-package eaf-all-the-icons
  :load-path "repos/emacs-application-framework/extension/"
  :after eaf)
;; =====================eaf=====================
(provide 'setup_eaf)
