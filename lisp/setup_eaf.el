;;; eaf
;; =====================eaf=====================
(use-package eaf
  :load-path "repos/emacs-application-framework/"
  :bind (("M-o a a" . eaf-open)
         ("M-o a s" . eaf-stop-process)
         ("M-o a r" . eaf-restart-process)
         ("M-o M-a" . eaf-open-pdf-from-history)
         ("M-o M-w" . eaf-open-browser-with-history)
         ("M-o M-RET" . eaf-open-pyqterminal))
  :config
  (pyvenv-activate-py3)
  (setq eaf-webengine-default-zoom "1.5"
        eaf-goto-right-after-close-buffer t)
  (define-key eaf-mode-map* (kbd "M-'") nil)
  (define-key eaf-mode-map* (kbd "M-/") nil)
  (define-key eaf-mode-map* (kbd "C-c d") 'eaf-duplicate-current-buffer)
  (define-key eaf-mode-map* (kbd "C-c s") 'eaf-get-buffer-screenshot)
  (define-key eaf-mode-map* (kbd "C-c f") 'eaf-toggle-fullscreen)
  (define-key eaf-mode-map* (kbd "C-c g") 'eaf-get-path-or-url)
  (define-key eaf-mode-map* (kbd "C-c a") 'eaf-share-path-or-url)
  (define-key eaf-mode-map* (kbd "C-c p") 'eaf-toggle-proxy)
  (define-key eaf-mode-map* (kbd "M-P") 'eaf-goto-previous-app)
  (define-key eaf-mode-map* (kbd "M-N") 'eaf-goto-next-app)
  (defun eaf-goto-previous-app ()
    (interactive)
    (let* ((first-bufs (cl-loop for buffer in (buffer-list)
                                for app = (buffer-local-value 'eaf--buffer-app-name buffer)
                                with app-list = nil
                                unless (or (null app)
                                           (equal eaf--buffer-app-name app)
                                           (member app app-list))
                                collect (progn (push app app-list)
                                               buffer)))
           (switch-to-prev-buffer-skip
            (lambda (_window buffer _bury-or-kill)
              (not (member buffer first-bufs)))))
      (call-interactively 'previous-buffer)))
  (defun eaf-goto-next-app ()
    (interactive)
    (let* ((first-bufs (cl-loop for buffer in (buffer-list)
                                for app = (buffer-local-value 'eaf--buffer-app-name buffer)
                                with app-list = nil
                                unless (or (null app)
                                           (equal eaf--buffer-app-name app)
                                           (member app app-list))
                                collect (progn (push app app-list)
                                               buffer)))
           (switch-to-prev-buffer-skip
            (lambda (_window buffer _bury-or-kill)
              (not (member buffer first-bufs)))))
      (call-interactively 'next-buffer)))
  (defun eaf-goto-left-tab ()
    (interactive)
    (if (bound-and-true-p awesome-tab-mode)
        (call-interactively 'awesome-tab-backward-tab)
      (let ((switch-to-prev-buffer-skip
             (lambda (_window buffer _bury-or-kill)
               (not (and eaf--buffer-app-name
                         (equal eaf--buffer-app-name
                                (buffer-local-value 'eaf--buffer-app-name buffer)))))))
        (call-interactively 'previous-buffer))))
  (defun eaf-goto-right-tab ()
    (interactive)
    (if (bound-and-true-p awesome-tab-mode)
        (call-interactively 'awesome-tab-forward-tab)
      (let ((switch-to-prev-buffer-skip
             (lambda (_window buffer _bury-or-kill)
               (not (and eaf--buffer-app-name
                         (equal eaf--buffer-app-name
                                (buffer-local-value 'eaf--buffer-app-name buffer)))))))
        (call-interactively 'next-buffer))))
  (defun eaf-request-kill-buffer (buffer-id)
    (let* ((buffer (eaf-get-buffer buffer-id))
           (buffer-same-app (when buffer
                              (cl-remove-if-not (lambda (buf)
                                                  (and eaf--buffer-app-name
                                                       (equal eaf--buffer-app-name
                                                              (buffer-local-value 'eaf--buffer-app-name buf))))
                                                (remove buffer (buffer-list))))))
      (if (and buffer-same-app eaf-goto-right-after-close-buffer)
          (cl-loop for w in (window-list)
                   when (eq (window-buffer w) buffer)
                   do (with-selected-window w (eaf-goto-right-tab))
                   finally (kill-buffer buffer))
        (call-interactively 'swint-kill-buffer))))
  (defun eaf-translate-text (text)
    (swint-sdcv-to-tip text))
  (defun eaf-toggle-proxy/around (fn)
    (if (not (string-empty-p eaf-proxy-host))
        (funcall fn)
      (setq eaf-proxy-host "127.0.0.1"
            eaf-proxy-port "7890"
            eaf-proxy-type "socks5")
      (eaf-restart-process)))
  (advice-add 'eaf-toggle-proxy :around #'eaf-toggle-proxy/around)
  (use-package eaf-browser
    :config
    (setq eaf-browser-default-search-engine "bing"
          eaf-browser-blank-page-url "https://www.bing.com"
          eaf-browser-dark-mode "follow")
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
    (defvar helm-eaf-pdf-annot-list-map
      (let ((map (make-sparse-keymap)))
        (define-key map (kbd "RET") #'(lambda () (interactive) (helm-exit-and-execute-action #'(lambda (cand) (eaf-pdf-jump-to-annot (gethash "annot-content" cand))))))
        (define-key map (kbd "C-c C-e") #'(lambda () (interactive) (helm-eaf-pdf-annot-action "edit_annot_by_id")))
        (define-key map (kbd "C-c RET") #'(lambda () (interactive) (helm-eaf-pdf-annot-action "move_annot_by_id")))
        (define-key map (kbd "C-c C-d") #'(lambda () (interactive) (helm-eaf-pdf-annot-action "delete_annot_by_id")))
        map)
      "Keymap for `helm-eaf-pdf-annot-list'.")
    (defun helm-eaf-pdf-annot-action (action)
      (helm-run-after-exit #'(lambda (_candidates action)
                               (let* ((cand (car _candidates))
                                      (page-num (gethash "page-num" cand))
                                      (annot-id (gethash "annot-id" cand)))
                                 (eaf-pdf-jump-to-annot (gethash "annot-content" cand))
                                 (eaf-call-sync "execute_function_with_args" eaf--buffer-id action page-num annot-id)))
                           (helm-marked-candidates) action))
    (defun helm-eaf-pdf-annot-list ()
      (interactive)
      (let* ((annots-real (cl-loop for page-num being the hash-keys of (json-parse-string (eaf-pdf-get-document-annots))
                                   using (hash-values page-content)
                                   append (cl-loop for annot-id being the hash-keys of (json-parse-string page-content)
                                                   using (hash-values annot-content)
                                                   collect (let ((temp-hash (make-hash-table :test 'equal)))
                                                             (puthash "page-num" page-num temp-hash)
                                                             (puthash "annot-id" annot-id temp-hash)
                                                             (puthash "annot-content" annot-content temp-hash)
                                                             temp-hash))))
             (annots-display (mapcar #'(lambda (x)
                                         (let ((page-num (number-to-string (1+ (string-to-number (gethash "page-num" x)))))
                                               (annot-content (gethash "annot-content" x)))
                                           (concat (format "Page: %-3s Type: %10s Content: %s"
                                                           page-num (gethash "type_name" annot-content)
                                                           (replace-regexp-in-string "\n" "" (gethash "content" (gethash "info" annot-content)))))))
                                     annots-real)))
        (helm-comp-read "Select annot: " (-zip-pair annots-display annots-real)
                        :preselect (format "^Page: %s.*" (cadr mode-line-position))
                        :buffer "*helm eaf annots-swint*"
                        :keymap helm-eaf-pdf-annot-list-map)))
    (eaf-bind-key nil "M-s" eaf-pdf-viewer-keybinding)
    (eaf-bind-key nil "i" eaf-pdf-viewer-keybinding)
    (eaf-bind-key toggle_inverted_mode "M-i" eaf-pdf-viewer-keybinding)
    (eaf-bind-key select_left_tab "M-p" eaf-pdf-viewer-keybinding)
    (eaf-bind-key select_right_tab "M-n" eaf-pdf-viewer-keybinding)
    (eaf-bind-key helm-eaf-pdf-annot-list "M-a a" eaf-pdf-viewer-keybinding)
    (eaf-bind-key add_annot_highlight "M-a h" eaf-pdf-viewer-keybinding)
    (eaf-bind-key add_annot_underline "M-a u" eaf-pdf-viewer-keybinding)
    (eaf-bind-key add_annot_squiggly "M-a s" eaf-pdf-viewer-keybinding)
    (eaf-bind-key add_annot_strikeout_or_delete_annot "M-a d" eaf-pdf-viewer-keybinding)
    (eaf-bind-key add_annot_popup_text "M-a t" eaf-pdf-viewer-keybinding)
    (eaf-bind-key add_annot_inline_text "M-a T" eaf-pdf-viewer-keybinding)
    (eaf-bind-key add_annot_rect "M-a r" eaf-pdf-viewer-keybinding)
    (eaf-bind-key edit_annot_text "M-a e" eaf-pdf-viewer-keybinding)
    (eaf-bind-key move_annot_text "M-a m" eaf-pdf-viewer-keybinding)
    (eaf-bind-key scroll_to_begin "M-<" eaf-pdf-viewer-keybinding)
    (eaf-bind-key scroll_to_end "M->" eaf-pdf-viewer-keybinding)
    (eaf-bind-key jump_to_page "M-g M-g" eaf-pdf-viewer-keybinding)
    (eaf-bind-key eaf-pdf-delete-pages "M-d" eaf-pdf-viewer-keybinding)
    (eaf-bind-key eaf-pdf-extract-page-text "M-t" eaf-pdf-viewer-keybinding)
    (eaf-bind-key undo_annot_action "C-/" eaf-pdf-viewer-keybinding)
    (eaf-bind-key redo_annot_action "C-M-/" eaf-pdf-viewer-keybinding))
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
    (eaf-bind-key eaf-send-key-sequence "C-v" eaf-pyqterminal-keybinding)
    (eaf-bind-key eaf-send-key-sequence "M-v" eaf-pyqterminal-keybinding)
    (eaf-bind-key eaf-send-key-sequence "M-<" eaf-pyqterminal-keybinding)
    (eaf-bind-key eaf-send-key-sequence "M->" eaf-pyqterminal-keybinding)
    (eaf-bind-key scroll_up_page "<prior>" eaf-pyqterminal-keybinding)
    (eaf-bind-key scroll_down_page "<next>" eaf-pyqterminal-keybinding)
    (eaf-bind-key scroll_down_page "C-S-v" eaf-pyqterminal-keybinding)
    (eaf-bind-key scroll_up_page "M-V" eaf-pyqterminal-keybinding)
    (eaf-bind-key scroll_to_begin "S-<prior>" eaf-pyqterminal-keybinding)
    (eaf-bind-key scroll_to_bottom "S-<next>" eaf-pyqterminal-keybinding)
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