;;; eaf
;; =====================eaf=====================
(use-package eaf
  :load-path "repos/emacs-application-framework/"
  :bind (("M-o a" . eaf-open)
         ("M-o M-a" . eaf-open-pdf-from-history)
         ("M-o b" . eaf-open-browser)
         ("M-o M-b" . eaf-open-browser-with-history))
  :config
  (pyvenv-activate-py3)
  (setq eaf-webengine-default-zoom "1.5")
  (define-key eaf-mode-map* (kbd "M-'") nil)
  (define-key eaf-mode-map* (kbd "M-/") nil)
  (defun eaf-translate-text (text)
    (baidu-translate-at-point text))
  (use-package eaf-browser
    :config
    (setq eaf-browser-default-search-engine "bing"
          eaf-browser-blank-page-url "https://www.bing.com"
          eaf-browser-dark-mode "follow")
    (eaf-bind-key copy_link "M-W" eaf-browser-keybinding)
    (eaf-bind-key nil "M-," eaf-browser-keybinding)
    (eaf-bind-key nil "M-." eaf-browser-keybinding)
    (eaf-bind-key nil "M-s" eaf-browser-keybinding)
    (eaf-bind-key nil "M-o" eaf-browser-keybinding)
    (eaf-bind-key nil "M-O" eaf-browser-keybinding)
    (eaf-bind-key nil "M-p" eaf-browser-keybinding))
  (use-package eaf-pdf-viewer
    :config
    (setq eaf-pdf-dark-mode "ignore"
          eaf-pdf-show-progress-on-page t)
    (eaf-bind-key nil "M-s" eaf-pdf-viewer-keybinding)
    (eaf-bind-key nil "M-p" eaf-pdf-viewer-keybinding)
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
    (eaf-bind-key jump_to_page "M-g" eaf-pdf-viewer-keybinding)))
;; =====================eaf=====================
(provide 'setup_eaf)
