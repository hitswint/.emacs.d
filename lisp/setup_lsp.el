;;; lsp-bridge
;; =====================lsp-bridge=====================
(use-package posframe
  :commands (posframe-show posframe-scroll-or-switch)
  :config
  (defun posframe-scroll-or-switch (buffer)
    (let (switch-to-posframe-buffer)
      (unwind-protect
          (let ((curr-event (read-event)))
            ;; 当鼠标位于posframe上时，M-p/M-n/M-e失效
            (setq switch-to-posframe-buffer (catch 'break
                                              (while (member curr-event '(134217838 134217840 134217847 25 134217829))
                                                (cond ((eq curr-event 134217838) ;M-n
                                                       (posframe-funcall buffer #'(lambda () (ignore-errors (scroll-up-command)))))
                                                      ((eq curr-event 134217840) ;M-p
                                                       (posframe-funcall buffer #'(lambda () (ignore-errors (scroll-down-command)))))
                                                      ((or (eq curr-event 134217847)  ;M-w
                                                           (eq curr-event 25))  ;C-y
                                                       (posframe-funcall buffer #'(lambda () (kill-new (buffer-substring-no-properties
                                                                                                        (point-min) (point-max)))))
                                                       (throw 'break (when (eq curr-event 134217847) 0)))
                                                      ((eq curr-event 134217829) ;M-e
                                                       (throw 'break 1)))
                                                (setq curr-event (read-event)))))
            (unless switch-to-posframe-buffer
              (push curr-event unread-command-events)))
        (if (equal switch-to-posframe-buffer 1)
            (progn (posframe-delete-frame buffer)
                   (other-frame 0)
                   (posframe-setup-buffer buffer))
          (posframe-delete buffer)
          (other-frame 0)))))
  (defun posframe-setup-buffer (buffer)
    (unless (member (buffer-name) '("*sdcv*" "*ydcv*" "*online*"))
      (window-configuration-to-register :sdcv))
    (delete-other-windows)
    (switch-to-buffer buffer)
    (set-buffer buffer)
    (setq-local cursor-type t)
    (setq-local cursor-in-non-selected-windows t)
    ;; Use buffer-local keybindings
    (use-local-map (copy-keymap org-mode-map))
    (local-set-key (kbd "q") #'(lambda () (interactive)
                                 (swint-kill-buffer)
                                 (when (get-register :sdcv)
                                   (jump-to-register :sdcv))))))
(use-package lsp-bridge
  :load-path "repos/lsp-bridge/"
  :delight '(:eval (propertize " L" 'face 'font-lock-function-name-face))
  :bind ("M-g l" . swint-toggle-lsp-bridge)
  :init
  (bind-key "C-x C-," 'xref-find-definitions)
  (bind-key "C-x C-." 'xref-pop-marker-stack)
  (bind-key "C-x C-/" 'xref-find-references)
  (bind-key "C-x C-?" 'xref-find-apropos)
  (setq lsp-bridge-enable-mode-line nil)
  :config
  (require 'yasnippet)
  (yas-global-mode 1)
  (setq lsp-bridge-c-lsp-server "ccls")              ;clangd
  (setq lsp-bridge-python-lsp-server "basedpyright")  ;pyright
  (setq lsp-bridge-python-multi-lsp-server "basedpyright_ruff")
  (setq lsp-bridge-complete-manually nil) ;lsp-bridge-popup-complete-menu
  (setq acm-enable-quick-access nil)
  (setq acm-quick-access-use-number-select nil)
  (setq acm-quick-access-modifier 'control)
  (defun swint-toggle-lsp-bridge ()
    (interactive)
    (pyvenv-activate-py3)
    (dolist (buf (cl-remove-if-not (lambda (x)
                                     (equal (buffer-mode x) major-mode))
                                   (buffer-list)))
      (with-current-buffer buf
        (call-interactively 'lsp-bridge-mode)))
    (if lsp-bridge-mode
        (add-hook (intern (concat (symbol-name major-mode) "-hook")) 'lsp-bridge-mode)
      (remove-hook (intern (concat (symbol-name major-mode) "-hook")) 'lsp-bridge-mode)))
  (add-hook 'lsp-bridge-mode-hook #'(lambda ()
                                      (set (make-local-variable 'company-idle-delay) nil)))
  (transient-define-prefix trainsient-scroll-popup-lsp-document ()
    ["scoll popup document"
     ("M-p" "scroll down" lsp-bridge-popup-documentation-scroll-down :transient t)
     ("M-n" "scroll up" lsp-bridge-popup-documentation-scroll-up :transient t)
     ("q" "quit" transient-quit-all)])
  (define-key acm-mode-map (kbd "M-<") #'acm-select-first)
  (define-key acm-mode-map (kbd "M->") #'acm-select-last)
  (define-key acm-mode-map (kbd "M-RET") #'acm-insert-common)
  (define-key acm-mode-map (kbd "C-s") #'acm-filter)
  (define-key acm-mode-map (kbd "C-o") #'acm-doc-toggle)
  (define-key acm-mode-map (kbd "M-p") #'acm-doc-scroll-down)
  (define-key acm-mode-map (kbd "M-n") #'acm-doc-scroll-up)
  (define-key lsp-bridge-mode-map (kbd "C-c C-,") #'lsp-bridge-find-def)
  (define-key lsp-bridge-mode-map (kbd "C-c C-<") #'lsp-bridge-find-def-other-window)
  (define-key lsp-bridge-mode-map (kbd "C-c C-.") #'lsp-bridge-find-def-return)
  (define-key lsp-bridge-mode-map (kbd "C-c M-,") #'lsp-bridge-find-type-def)
  (define-key lsp-bridge-mode-map (kbd "C-c M-<") #'lsp-bridge-find-type-def-other-window)
  (define-key lsp-bridge-mode-map (kbd "C-c M-.") #'lsp-bridge-find-impl)
  (define-key lsp-bridge-mode-map (kbd "C-c M->") #'lsp-bridge-find-impl-other-window)
  (define-key lsp-bridge-mode-map (kbd "C-c /") #'lsp-bridge-find-references)
  (define-key lsp-bridge-mode-map (kbd "C-c M-/") #'lsp-bridge-workspace-list-symbol-at-point)
  (define-key lsp-bridge-mode-map (kbd "C-c M-?") #'lsp-bridge-workspace-list-symbols)
  (define-key lsp-bridge-mode-map (kbd "C-c C-/") #'(lambda () (interactive)
                                                      (lsp-bridge-popup-documentation)
                                                      ;; (trainsient-scroll-popup-lsp-document)
                                                      (smartrep-read-event-loop
                                                       '(("M-p" . lsp-bridge-popup-documentation-scroll-down)
                                                         ("M-n" . lsp-bridge-popup-documentation-scroll-up)))))
  (define-key lsp-bridge-mode-map (kbd "C-c C-?") #'lsp-bridge-show-documentation)
  ;; (define-key lsp-bridge-mode-map (kbd "M-p/n") #'(lambda (&optional arg) (interactive "P")
  ;;                                                   (if (acm-frame-visible-p lsp-bridge-popup-documentation-frame)
  ;;                                                       (call-interactively 'lsp-bridge-popup-documentation-scroll-down/up)
  ;;                                                     (call-interactively 'pixel-scroll-window-move-down/up))))
  (define-key lsp-bridge-mode-map (kbd "C-c M-r") #'lsp-bridge-rename)
  (smartrep-define-key lsp-bridge-mode-map "C-c" '(("M-p" . lsp-bridge-diagnostic-jump-prev)
                                                   ("M-n" . lsp-bridge-diagnostic-jump-next)))
  (define-key lsp-bridge-mode-map (kbd "C-c M-d") #'lsp-bridge-diagnostic-list)
  (define-key lsp-bridge-mode-map (kbd "C-c M-w") #'lsp-bridge-diagnostic-copy)
  (define-key lsp-bridge-mode-map (kbd "C-c M-a") #'lsp-bridge-code-action)
  (define-key lsp-bridge-mode-map (kbd "C-c M-f") #'lsp-bridge-code-format)
  (define-key lsp-bridge-mode-map (kbd "C-c d") #'lsp-bridge-toggle-sdcv-helper)
  (define-key lsp-bridge-mode-map (kbd "C-c ,") #'lsp-bridge-peek)
  (define-key lsp-bridge-peek-keymap (kbd "C-h") #'lsp-bridge-peek-through)  ;hjkl移动
  (define-key lsp-bridge-peek-keymap (kbd ",") #'lsp-bridge-peek-jump)
  (define-key lsp-bridge-peek-keymap (kbd ".") #'lsp-bridge-peek-jump-back))
;; =====================lsp-bridge=====================
(provide 'setup_lsp)
