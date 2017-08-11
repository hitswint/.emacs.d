;;; recentf
;; =====================recentf====================
(use-package recentf
  ;; Enabled at commands.
  :defer t
  :after recentf-ext
  :bind ("C-x M-f" . recentf-ido-find-file)
  :config
  (recentf-mode 1)
  (setq recentf-max-saved-items 100)
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
;; =====================recentf====================
;;; recent file/recent directory
;; ======recent file/recent directory==============
(use-package recentf-ext
  ;; Enabled at idle.
  :defer 2
  :config
  ;; 定义swint-helm-source-recentf-file。
  (defclass swint-helm-recentf-file-source (helm-source-sync)
    ((init :initform (lambda ()
                       (recentf-mode 1)))
     (candidates :initform (lambda () (remove-if (lambda (x)
                                                   (or (file-directory-p x)
                                                       (member x (mapcar (lambda (xx)
                                                                           (buffer-file-name xx))
                                                                         (buffer-list)))))
                                                 recentf-list)))
     (pattern-transformer :initform 'helm-recentf-pattern-transformer)
     (match-part :initform (lambda (candidate)
                             (if (or helm-ff-transformer-show-only-basename
                                     helm-recentf--basename-flag)
                                 (helm-basename candidate) candidate)))
     (filter-one-by-one :initform (lambda (c)
                                    (if (and helm-ff-transformer-show-only-basename
                                             (not (consp c)))
                                        (cons (helm-basename c) c)
                                      c)))
     (keymap :initform helm-generic-files-map)
     (help-message :initform helm-generic-file-help-message)
     (action :initform (helm-actions-from-type-file))))
  (defcustom swint-helm-recentf-file-fuzzy-match nil
    "Enable fuzzy matching in `helm-source-recentf' when non--nil."
    :group 'helm-files
    :type 'boolean
    :set (lambda (var val)
           (set var val)
           (setq swint-helm-source-recentf-file
                 (helm-make-source "Recentf File" 'swint-helm-recentf-file-source
                   :fuzzy-match swint-helm-recentf-file-fuzzy-match))))
  ;; 定义swint-helm-source-recentf-directory。
  (defclass swint-helm-recentf-directory-source (helm-source-sync)
    ((init :initform (lambda ()
                       (recentf-mode 1)))
     (candidates :initform (lambda () (remove-if (lambda (x)
                                                   (or (not (file-directory-p x))
                                                       (member x (mapcar (lambda (xx)
                                                                           (expand-file-name (buffer-local-value 'default-directory xx)))
                                                                         (remove-if-not (lambda (x)
                                                                                          (equal (buffer-mode x) 'dired-mode))
                                                                                        (buffer-list))))))
                                                 recentf-list)))
     (pattern-transformer :initform 'helm-recentf-pattern-transformer)
     (match-part :initform (lambda (candidate)
                             (if (or helm-ff-transformer-show-only-basename
                                     helm-recentf--basename-flag)
                                 (helm-basename candidate) candidate)))
     (filter-one-by-one :initform (lambda (c)
                                    (if (and helm-ff-transformer-show-only-basename
                                             (not (consp c)))
                                        (cons (helm-basename c) c)
                                      c)))
     (keymap :initform helm-generic-files-map)
     (help-message :initform helm-generic-file-help-message)
     (action :initform (helm-actions-from-type-file))))
  (defcustom swint-helm-recentf-directory-fuzzy-match nil
    "Enable fuzzy matching in `helm-source-recentf' when non--nil."
    :group 'helm-files
    :type 'boolean
    :set (lambda (var val)
           (set var val)
           (setq swint-helm-source-recentf-directory
                 (helm-make-source "Recentf Directory" 'swint-helm-recentf-directory-source
                   :fuzzy-match swint-helm-recentf-directory-fuzzy-match)))))
;; ======recent file/recent directory==============
(provide 'setup_recentf)
