;; ========================recentf=========================
(recentf-mode 1) ; keep a list of recently opened files
;; recentf改用helm前端。
(global-set-key (kbd "C-c f") 'recentf-ido-find-file)
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
                             file-assoc-list))))))
;; =============为helm分别定义recent file和recent directory的source==================
(require 'recentf-ext)
;; 定义 swint-helm-source-recentf-file
(defclass swint-helm-recentf-file-source (helm-source-sync)
  ((init :initform (lambda ()
                     (require 'recentf)
                     (recentf-mode 1)))
   (candidates :initform (lambda () (remove-if (lambda (x)
                                                 (or (string-match-p ".*\/$" x)
                                                     (member x (mapcar (lambda (xx)
                                                                         (buffer-file-name (get-buffer xx)))
                                                                       ido-temp-list/all-persps))))
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
(defvar swint-helm-source-recentf-file nil
  "See (info \"(emacs)File Conveniences\").
Set `recentf-max-saved-items' to a bigger value if default is too small.")
(defcustom swint-helm-recentf-file-fuzzy-match nil
  "Enable fuzzy matching in `helm-source-recentf' when non--nil."
  :group 'helm-files
  :type 'boolean
  :set (lambda (var val)
         (set var val)
         (setq swint-helm-source-recentf-file
               (helm-make-source "Recentf File" 'swint-helm-recentf-file-source
                 :fuzzy-match swint-helm-recentf-file-fuzzy-match))))
;; 定义 swint-helm-source-recentf-directory
(defclass swint-helm-recentf-directory-source (helm-source-sync)
  ((init :initform (lambda ()
                     (require 'recentf)
                     (recentf-mode 1)))
   (candidates :initform (lambda () (remove-if-not (lambda (x)
                                                     (and (string-match-p ".*\/$" x)
                                                          (not (member x (mapcar (lambda (xx)
                                                                                   (with-current-buffer xx
                                                                                     (expand-file-name default-directory)))
                                                                                 iswitchb-temp-buflist/all-persps)))))
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
(defvar swint-helm-source-recentf-directory nil
  "See (info \"(emacs)File Conveniences\").
Set `recentf-max-saved-items' to a bigger value if default is too small.")
(defcustom swint-helm-recentf-directory-fuzzy-match nil
  "Enable fuzzy matching in `helm-source-recentf' when non--nil."
  :group 'helm-files
  :type 'boolean
  :set (lambda (var val)
         (set var val)
         (setq swint-helm-source-recentf-directory
               (helm-make-source "Recentf Directory" 'swint-helm-recentf-directory-source
                 :fuzzy-match swint-helm-recentf-directory-fuzzy-match))))
;; =============为helm分别定义recent file和recent directory的source==================
;; ========================recentf=========================
(provide 'setup_recentf)
