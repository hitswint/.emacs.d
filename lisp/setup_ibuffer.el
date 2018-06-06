;;; ibuffer
;; =========================ibuffer==============================
(def-package! ibuffer
  :bind ("C-x C-b" . ibuffer)
  :config
  (setq ibuffer-expert t
        ibuffer-show-empty-filter-groups nil)
  (define-key ibuffer-mode-map (kbd "i") 'ibuffer-jump-to-filter-group)
;;;; ibuffer分组
  ;; ======================ibuffer分组===========================
  (def-package! ibuf-ext
    :config
    (add-hook 'ibuffer-mode-hook '(lambda () (if (not (bound-and-true-p persp-mode))
                                                 (ibuffer-switch-to-saved-filter-groups "Filename")
                                               (ibuffer-create-saved-filter-groups-with-persp)
                                               (ibuffer-switch-to-saved-filter-groups "Persp"))))
;;;; 按persp分组
    (define-ibuffer-filter persp
        "Toggle current view to buffers with file or directory name matching QUALIFIER."
      (:description "persp" :reader (read-from-minibuffer "Filter by persp (regexp): "))
      (memq buf (persp-buffers (gethash qualifier (perspectives-hash)))))
    (defun ibuffer-create-saved-filter-groups-with-persp ()
      (interactive)
      (let* ((ibuffer-saved-filter-groups-without-persp
              (cl-remove-if #'(lambda (x)
                                (equal (car x) "Persp"))
                            ibuffer-saved-filter-groups))
             (it (cl-loop for x in (delete "i" (persp-names))
                          collect (list x (cons 'persp x))))
             (itt (push "Persp" it)))
        (setq ibuffer-saved-filter-groups
              (push itt ibuffer-saved-filter-groups-without-persp))))
    (define-key ibuffer-mode-map (kbd ":") '(lambda () (interactive)
                                              (ibuffer-create-saved-filter-groups-with-persp)
                                              (ibuffer-switch-to-saved-filter-groups "Persp")))
;;;; 按filename分组
    (define-ibuffer-filter filename
        "Toggle current view to buffers with file or directory name matching QUALIFIER."
      (:description "filename" :reader (read-from-minibuffer "Filter by file/directory name (regexp): "))
      (ibuffer-awhen (or (buffer-local-value 'buffer-file-name buf)
                         (buffer-local-value 'dired-directory buf))
        (string-match qualifier it)))
    (add-to-list 'ibuffer-saved-filter-groups
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
                           (filename . "~/org")
                           (filename . "~/tex")
                           (filename . "~/myfile")
                           (filename . "~/Music")
                           (filename . "~/Pictures")))))
    (define-key ibuffer-mode-map (kbd ";") '(lambda () (interactive)
                                              (ibuffer-switch-to-saved-filter-groups "Filename"))))
  ;; ======================ibuffer分组===========================
  )
;; =========================ibuffer==============================
(provide 'setup_ibuffer)
