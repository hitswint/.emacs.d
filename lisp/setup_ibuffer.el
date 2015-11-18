;; ===========================ibuffer==================================
(use-package ibuffer
  ;; Enabled at commands.
  :defer t
  :bind ("C-c `" . ibuffer)
  :config
  (use-package ibuf-ext)
  ;; (add-to-list 'ibuffer-never-show-predicates "^\\*")
  (autoload 'ibuffer "ibuffer" "List buffers." t)
  (setq ibuffer-delete-window-on-quit t)
  (defadvice ibuffer-quit (after kill-ibuffer activate)
    ;;Kill the ibuffer buffer on exit.
    (kill-buffer "*Ibuffer*"))
  (setq ibuffer-expert t)
  (setq ibuffer-show-empty-filter-groups nil)
  ;; ================================ibuffer分组======================================
  ;; Enable ibuffer-filter-by-filename to filter on directory names too.
  ;; 这个版本的ibuffer有问题，dired的路径为"~/"，而文件的路径为"/home/swint/"，所以要分开设置。
  (define-ibuffer-filter filename
      "Toggle current view to buffers with file or directory name matching QUALIFIER."
    (:description "filename"
                  :reader (read-from-minibuffer "Filter by file/directory name (regexp): "))
    (ibuffer-awhen (or (buffer-local-value 'buffer-file-name buf)
                       (buffer-local-value 'dired-directory buf))
      (string-match qualifier it)))
  (when is-lin
    (setq ibuffer-saved-filter-groups
          (quote (("default"
                   ("CODING" (or
                              (filename . "~/Documents")
                              (filename . "~/Dropbox")
                              (filename . "~/Nutstore")
                              (filename . "~/.emacs.d")))
                   ("REFERENCE" (or
                                 (filename . "~/papers")
                                 (filename . "~/book")
                                 (filename . "~/linux")))
                   ("TEX" (or
                           (filename . "~/tex")
                           (filename . "~/myfile")
                           (filename . "~/Music")
                           (filename . "~/Pictures"))))))))
  (when is-win
    (setq ibuffer-saved-filter-groups
          (quote (("default"
                   ("CODING" (or
                              (filename . "c:/Users/swint/Documents")
                              (filename . "c:/Users/swint/Dropbox")
                              (filename . "c:/Users/swint/Nutstore")
                              (filename . "c:/Users/swint/.emacs.d")))
                   ("REFERENCE" (or
                                 (filename . "c:/Users/swint/papers")
                                 (filename . "c:/Users/swint/linux")
                                 (filename . "c:/Users/swint/book")))
                   ("TEX" (or
                           (filename . "c:/Users/swint/tex")
                           (filename . "c:/Users/swint/myfile")
                           (filename . "c:/Users/swint/Music")
                           (filename . "c:/Users/swint/Pictures"))))))))
  (add-hook 'ibuffer-mode-hook
            (lambda ()
              (ibuffer-switch-to-saved-filter-groups "default")))
  (define-key ibuffer-mode-map (kbd "A") 'ibuffer-do-view-horizontally)
  ;; ================================ibuffer分组======================================
  ;; ========================按路径排列==========================
  (define-ibuffer-sorter filename-or-dired
    "Sort the buffers by their pathname."
    (:description "filenames plus dired")
    (string-lessp
     (with-current-buffer (car a)
       (or buffer-file-name
           (if (eq major-mode 'dired-mode)
               (expand-file-name dired-directory))
           ;; so that all non pathnames are at the end
           "~"))
     (with-current-buffer (car b)
       (or buffer-file-name
           (if (eq major-mode 'dired-mode)
               (expand-file-name dired-directory))
           ;; so that all non pathnames are at the end
           "~"))))
  (define-key ibuffer-mode-map (kbd ";") 'ibuffer-do-sort-by-filename-or-dired)
  ;; ========================按路径排列==========================
  )
;; ===========================ibuffer==================================
(provide 'setup_ibuffer)
