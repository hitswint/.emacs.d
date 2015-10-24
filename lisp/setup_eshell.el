(add-hook 'eshell-mode-hook (lambda()
                              (outline-minor-mode 1)
                              (setq outline-regexp "^[^#$\n]* [#>]+ "
                                    scroll-margin 0
                                    eshell-scroll-to-bottom-on-output t
                                    eshell-scroll-show-maximum-output t)
                              (add-to-list 'eshell-output-filter-functions
                                           'eshell-postoutput-scroll-to-bottom)
                              (define-key eshell-mode-map (kbd "C-c C-i") nil)
                              (define-key eshell-mode-map (kbd "C-c C-o") nil)
                              (define-key eshell-mode-map (kbd "M-s") nil)
                              ))
(defvar ac-source-eshell-pcomplete
  '((candidates . (pcomplete-completions))))
(defun ac-complete-eshell-pcomplete ()
  (interactive)
  (auto-complete '(ac-source-eshell-pcomplete)))
;; 自动开启 ac-mode
;; 需要 (global-auto-complete-mode 1)
(add-to-list 'ac-modes 'eshell-mode)
(setq ac-sources '(ac-source-eshell-pcomplete
                   ;; ac-source-files-in-current-dir
                   ;; ac-source-filename
                   ;; ac-source-abbrev
                   ;; ac-source-words-in-buffer
                   ;; ac-source-imenu
                   ))
;; 让shell命令在windows下默认启用cygwin bash
;; cmdproxy.exe则是windows自带命令行工具
(cond
 (is-win
  (setq explicit-shell-file-name "bash.exe"))
 (is-lin
  (setq explicit-shell-file-name "bash")
  (setq explicit-bash-args '("-c" "export EMACS=; stty echo; bash"))))
(provide 'setup_eshell)
