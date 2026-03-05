;;; ellama
;; ====================ellama======================
(use-package ellama
  :commands ellama-translate-at-point
  :bind-keymap ("M-E" . ellama-command-map)
  :init
  (add-hook 'ellama-session-mode-hook #'(lambda ()
                                          (setq-local backup-inhibited 1)
                                          (setq-local auto-save-default nil)
                                          (auto-save-mode -1)))
  :config
  (define-key ellama-command-map (kbd "p") #'ellama-provider-select)
  (define-key ellama-command-map (kbd "d") #'ellama-define-word)
  (setopt ellama-enable-keymap nil
          ellama-language "English"
          ellama-major-mode 'org-mode
          ellama-nick-prefix-depth 1)
  (require 'llm-ollama)
  (require 'llm-openai)
  (setopt llm-warn-on-nonfree nil)
  (when (load "~/.emacs.d/ellama-providers.el" t)
    (setopt ellama-provider (cdar ellama-providers)))
  (setq ellama-naming-scheme 'ellama-generate-name-by-bytes)
  (defun ellama-generate-name-by-bytes (provider action prompt)
    "Generate name for ACTION by PROVIDER by getting first N words from PROMPT."
    (let* ((cleaned-prompt (replace-regexp-in-string "\\(/\\|\n+[ \t]*\\)" "_" prompt))
           (max-bytes 218)
           (prompt-words (if (> (string-bytes cleaned-prompt) max-bytes)
                             (if (multibyte-string-p cleaned-prompt)
                                 (substring (string-as-multibyte (s-left max-bytes (string-as-unibyte cleaned-prompt))) 0 -2)
                               (s-left max-bytes cleaned-prompt))
                           cleaned-prompt)))
      (string-join
       (flatten-tree
        (list (split-string (format "%s" action) "-")
              prompt-words
              (format "(%s)" (llm-name provider))))
       " ")))
  (defun ellama-translate-at-point (&optional _word)
    (interactive)
    (let* ((word (or _word (swint-get-words-at-point)))
           (ellama-language (if (string-match-p "\\cC" word) "English" "Chinese"))
           (ellama-translation-template "Translation to %s: %s")
           (ellama-provider (cdar ellama-providers)))
      (ellama-instant-to-posframe
       (format ellama-translation-template
               ellama-language word ellama-language)
       :provider ellama-translation-provider)))
  (defun ellama-instant-to-posframe (prompt &rest args)
    (let* ((provider (or (plist-get args :provider)
                         ellama-provider))
           (buffer-name (ellama-generate-name provider real-this-command prompt))
           (buffer (get-buffer-create (if (get-buffer buffer-name)
                                          (make-temp-name (concat buffer-name " "))
                                        buffer-name)))
           filter)
      (with-current-buffer buffer
        (funcall ellama-major-mode)
        (when (derived-mode-p 'org-mode)
          (setq filter 'ellama--translate-markdown-to-org-filter)))
      (ellama-stream prompt
                     :buffer buffer
                     :filter filter
                     :provider provider)
      (if (not (posframe-workable-p))
          (display-buffer buffer)
        (posframe-show buffer
                       :border-color "red"
                       :border-width 2
                       :background-color "black"
                       :width (window-width)
                       :height (/ (window-height) 2))
        (posframe-scroll-or-switch buffer))))
  (defun ellama-get-pdf-text ()
    (shell-command-to-string (format "pdftotext -l %s -nopgbrk -q -- \"%s\" - | fmt -w %s"
                                     (if (derived-mode-p 'pdf-view-mode)
                                         (pdf-view-current-page)
                                       (eaf-call-sync "execute_function" eaf--buffer-id "current_page"))
                                     (or (buffer-file-name)
                                         (bound-and-true-p eaf--buffer-url))
                                     fill-column)))
  (defun ellama-summarize/around (fn)
    (interactive)
    (if (derived-mode-p 'pdf-view-mode 'eaf-mode)
        (let ((text (or (s-presence (swint-get-words-at-point))
                        (ellama-get-pdf-text))))
          (ellama-instant (format ellama-summarize-prompt-template text)))
      (funcall fn)))
  (advice-add 'ellama-summarize :around #'ellama-summarize/around)
  (defun ellama-ask-about/around (fn &rest args)
    (interactive)
    (if (derived-mode-p 'pdf-view-mode 'eaf-mode)
        (let ((input (read-string "Ask ellama about this text: "))
              (content (or (s-presence (swint-get-words-at-point))
                           (ellama-get-pdf-text))))
          (ellama-context-element-add (ellama-context-element-text :content content))
          (ellama-chat input))
      (apply fn args)))
  (advice-add 'ellama-ask-about :around #'ellama-ask-about/around))
;; ====================ellama======================
;;; claude-code
;; =================claude-code====================
(use-package claude-code
  :load-path "repos/claude-code.el/"
  :diminish claude-code-mode
  :bind-keymap ("M-C" . claude-code-command-map)
  :bind (:repeat-map my-claude-code-map ("M" . claude-code-cycle-mode))
  :config
  ;; 直接使用define-prefix-command会导致已有的按键绑定失效
  (if (boundp 'claude-code-command-map)
      (fset 'claude-code-command-map claude-code-command-map)
    (define-prefix-command 'claude-code-command-map))
  (bind-key "M-C" 'code-switch-agent claude-code-command-map)
  ;; 使用~/.claude/settings.json设定大模型，也可(setenv "ANTHROPIC_AUTH_TOKEN" (get-auth-pass "DeepSeek"))
  (use-package inheritenv)
  (use-package vterm
    :config
    (setq vterm-max-scrollback 100000)
    (setopt vterm-min-window-width 40))
  (setq claude-code-terminal-backend 'vterm)
  (use-package monet
    :load-path "repos/monet/"
    :diminish monet-mode
    :config
    (monet-mode 1))
  (add-hook 'claude-code-process-environment-functions #'monet-start-server-function)
  (claude-code-mode))
;; =================claude-code====================
;;; agent-shell
;; =================agent-shell====================
(use-package agent-shell
  ;; :bind-keymap ("M-C" . agent-shell-prefix-map)
  :commands code-switch-agent
  :bind (:map agent-shell-prefix-map
              ("a" . agent-shell)
              ("t" . agent-shell-toggle)
              ("n" . agent-shell-new-shell)
              ("f" . agent-shell-send-file)
              ("r" . agent-shell-send-region)
              ("d" . agent-shell-send-dwim)
              ("c" . agent-shell-prompt-compose)
              ("?" . agent-shell-help-menu)
              ("M-C" . code-switch-agent))
  :init
  (defvar agent-shell-prefix-map (make-sparse-keymap)
    "Keymap for agent-shell commands.")
  (define-prefix-command 'agent-shell-prefix-map)
  :config
  (defun code-switch-agent ()
    (interactive)
    (if (equal (lookup-key global-map (kbd "M-C")) 'agent-shell-prefix-map)
        (global-set-key (kbd "M-C") 'claude-code-command-map)
      (global-set-key (kbd "M-C") 'agent-shell-prefix-map)))
  (bind-key "C-<tab>" nil agent-shell-mode-map)
  (bind-key "C-c C-<tab>" 'agent-shell-cycle-session-mode agent-shell-mode-map))
;; =================agent-shell====================
(provide 'setup_ai)
