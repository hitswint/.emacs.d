;;; ibuffer
;; =========================ibuffer==============================
(use-package ibuffer
  ;; Enabled at commands.
  :defer t
  :bind ("C-x C-b" . ibuffer)
  :config
  (setq ibuffer-expert t
        ibuffer-show-empty-filter-groups nil)
  (define-key ibuffer-mode-map (kbd "i") 'ibuffer-jump-to-filter-group)
;;;; ibuffer分组
  ;; ======================ibuffer分组===========================
  (use-package ibuf-ext
    ;; Enabled automatically.
    :config
;;;;; 按persp分组。
    (define-ibuffer-filter persp
        "Toggle current view to buffers with file or directory name matching QUALIFIER."
      (:description "persp" :reader (read-from-minibuffer "Filter by persp (regexp): "))
      (memq buf (persp-buffers (gethash qualifier perspectives-hash))))
    (defun ibuffer-create-saved-filter-groups-with-persp ()
      (interactive)
      (let* ((ibuffer-saved-filter-groups-without-persp
              (remove-if #'(lambda (x)
                             (equal (car x) "Persp"))
                         ibuffer-saved-filter-groups))
             (it (mapcar #'(lambda (x)
                             (list x (cons 'persp x))) (delete "i" (persp-names))))
             (itt (push "Persp" it)))
        (setq ibuffer-saved-filter-groups
              (push itt ibuffer-saved-filter-groups-without-persp))))
    (add-hook 'ibuffer-mode-hook '(lambda ()
                                    (ibuffer-create-saved-filter-groups-with-persp)
                                    (ibuffer-switch-to-saved-filter-groups "Persp")))
    (define-key ibuffer-mode-map (kbd ":") '(lambda () (interactive)
                                              (ibuffer-create-saved-filter-groups-with-persp)
                                              (ibuffer-switch-to-saved-filter-groups "Persp")))
;;;;; 按filename分组。
    ;; 这个版本的ibuffer有问题，dired路径为"~/"，文件路径为"/home/swint/"，要分开设置。
    (define-ibuffer-filter filename
        "Toggle current view to buffers with file or directory name matching QUALIFIER."
      (:description "filename" :reader (read-from-minibuffer "Filter by file/directory name (regexp): "))
      (ibuffer-awhen (or (buffer-local-value 'buffer-file-name buf)
                         (buffer-local-value 'dired-directory buf))
        (string-match qualifier it)))
    (cond
     (is-lin (add-to-list 'ibuffer-saved-filter-groups
                          '("Filename"
                            ("Coding" (or
                                       (filename . "~/Documents")
                                       (filename . "~/Dropbox")
                                       (filename . "~/Nutstore")
                                       (filename . "~/.emacs.d")))
                            ("Reference" (or
                                          (filename . "~/papers")
                                          (filename . "~/book")
                                          (filename . "~/linux")))
                            ("Tex" (or
                                    (filename . "~/tex")
                                    (filename . "~/myfile")
                                    (filename . "~/Music")
                                    (filename . "~/Pictures"))))))
     (is-win (add-to-list 'ibuffer-saved-filter-groups
                          '("Filename"
                            ("Coding" (or
                                       (filename . "c:/Users/swint/Documents")
                                       (filename . "c:/Users/swint/Dropbox")
                                       (filename . "c:/Users/swint/Nutstore")
                                       (filename . "c:/Users/swint/.emacs.d")))
                            ("Reference" (or
                                          (filename . "c:/Users/swint/papers")
                                          (filename . "c:/Users/swint/linux")
                                          (filename . "c:/Users/swint/book")))
                            ("Tex" (or
                                    (filename . "c:/Users/swint/tex")
                                    (filename . "c:/Users/swint/myfile")
                                    (filename . "c:/Users/swint/Music")
                                    (filename . "c:/Users/swint/Pictures")))))))
    (define-key ibuffer-mode-map (kbd ";") '(lambda () (interactive)
                                              (ibuffer-switch-to-saved-filter-groups "Filename"))))
  ;; ======================ibuffer分组===========================
  )
;; =========================ibuffer==============================
(provide 'setup_ibuffer)
