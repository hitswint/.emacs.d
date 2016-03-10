;; ========================recentf=========================
(use-package recentf
  ;; Enabled at commands.
  :defer t
  :bind ("C-x M-f" . recentf-ido-find-file)
  :config
  (recentf-mode 1)
  ;; recentf改用helm前端。
  (defun recentf-ido-find-file ()
    "Find a recent file using Ido."
    (interactive)
    (let* ((file-assoc-list
            (mapcar (lambda (x)
                      (cons (file-name-nondirectory x)
                            x))
                    recentf-list))
           (filename-list
            (remove-duplicates (mapcar #'car file-assoc-list)
                               :test #'string=))
           (filename (ido-completing-read "Recentf: "
                                          filename-list
                                          nil
                                          t)))
      (when filename
        (find-file (cdr (assoc filename
                               file-assoc-list)))))))
;; ========================recentf=========================
(provide 'setup_recentf)
