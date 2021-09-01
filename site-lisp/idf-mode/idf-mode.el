;;; idf-mode.el --- Major mode for editing idf files.

;; Copyright (C) 2019-2019 Guiqiang Wang
;;
;; Authors: Guiqiang Wang <wguiqiang@hotmail.com>
;; URL: https://github.com/hitswint
;; Package-Version: 20190628.01
;; Version: 0.0.1
;; Keywords: idf

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Installation:

;; To install, just drop this file into a directory in your
;; `load-path' and (optionally) byte-compile it.  To automatically
;; handle files ending in '.idf', add something like:
;;
;; (require 'idf-mode)
;; (add-to-list 'auto-mode-alist '("\\.idf\\'" . idf-mode))
;; (define-key idf-mode-map (kbd "C-c C-p") 'idf-prev-object)
;; (define-key idf-mode-map (kbd "C-c C-n") 'idf-next-object)
;; (define-key idf-mode-map (kbd "C-c C-b") 'idf-prev-type)
;; (define-key idf-mode-map (kbd "C-c C-f") 'idf-next-type)
;; (define-key idf-mode-map (kbd "C-c C-,") 'idf-find-object-at-point)
;;
;; to your .emacs file.

;;; Code:

(require 'ctable)
(require 's)
(require 'dash)

(defvar idf-mode-syntax-table
  (let ((st (make-syntax-table)))
    ;; Comments
    (modify-syntax-entry ?! "<" st)
    (modify-syntax-entry ?\n ">" st)
    st)
  "Syntax table for idf-mode.")

(defvar idf-type-prefix-regexp (concat "\\(^[ \t]*!.*\\|^[ \t]*\\|;.*\\)\n" "[ \t]*"))
(defvar idf-type-regexp "\\([a-zA-Z0-9:]*\\)")
(defvar idf-type-variable-separator-regexp "\\(,[^,;\n]*\n[ \t]*\\|,[ \t]*\\)")

(defconst idf-font-lock-keywords
  `((,(concat idf-type-prefix-regexp idf-type-regexp ",")
     (2 font-lock-type-face))           ;前一行为注释行/空行/以;结尾
    (,(concat idf-type-prefix-regexp idf-type-regexp idf-type-variable-separator-regexp "\\([^,;\n]*\\)" "[,;]")
     (4 font-lock-variable-name-face))  ;variable与type同一行或不同行
    (,(concat "^[ \t]*" "\\([^,;]*\\)" "[,;][ \t]*![ \t]*-.+[nN]ame")
     (1 font-lock-builtin-face))
    (,(concat ".+![ \t]*-[ \t]*" "\\(.*\\)" "\n")
     (1 font-lock-keyword-face)))
  "Keywords for highlighting.")

(defcustom idf-imenu-generic-expression ;包含type与variable
  '((nil "\\(^[ \t]*!.*\\|^[ \t]*\\|;.*\\)\n[ \t]*\\([a-zA-Z0-9:]*,[^,;]*\\)[,;]" 2))
  "The imenu regex to parse an outline of the idf file.")

(defun idf-imenu-create-index-function ()
  (let ((default-index (imenu-default-create-index-function)))
    (cl-loop for i in default-index
             collect (cons (replace-regexp-in-string (concat idf-type-variable-separator-regexp "\\([^,;\n]*\\)")
                                                     " | " (car i) nil nil 1)
                           (cdr i)))))

(defun idf-set-imenu-generic-expression ()
  (make-local-variable 'imenu-generic-expression)
  (make-local-variable 'imenu-create-index-function)
  (setq imenu-create-index-function 'idf-imenu-create-index-function)
  (setq imenu-generic-expression idf-imenu-generic-expression))

(add-hook 'idf-mode-hook 'idf-set-imenu-generic-expression)

(defun idf-prev-object ()
  (interactive)
  (re-search-backward (concat idf-type-prefix-regexp idf-type-regexp ",") nil t)
  (goto-char (match-beginning 2)))

(defun idf-next-object ()
  (interactive)
  (goto-char (+ (line-end-position) 1))
  (if (re-search-forward (concat idf-type-prefix-regexp idf-type-regexp ",") nil t)
      (goto-char (match-beginning 2))))

(defun idf-next-type ()
  (interactive)
  (goto-char (+ (line-end-position) 1))
  (re-search-backward (concat idf-type-prefix-regexp idf-type-regexp ",") nil t)
  (let* ((current-type (match-string 2))
         (next-type current-type))
    (while (equal next-type current-type)
      (if (idf-next-object)
          (setq next-type (match-string 2))
        (setq next-type nil)))))

(defun idf-prev-type ()
  (interactive)
  (goto-char (+ (line-end-position) 1))
  (re-search-backward (concat idf-type-prefix-regexp idf-type-regexp ",") nil t)
  (let* ((current-type (match-string 2))
         (prev-type current-type))
    (while (equal prev-type current-type)
      (if (idf-prev-object)
          (setq prev-type (match-string 2))
        (setq prev-type nil)))))

(defun idf-find-object-at-point ()
  "Find the object at point."
  (interactive)
  (let ((current-point (point))
        (current-object (save-excursion
                          (goto-char (line-beginning-position))
                          (re-search-forward "^[ \t]*\\(.*\\)[,;].*\n"
                                             (line-beginning-position 2) t)
                          (match-string 1))))
    (if (fboundp 'xref-push-marker-stack)
        (xref-push-marker-stack)
      (with-no-warnings
        (ring-insert find-tag-marker-ring (point-marker))))
    (cond
     (current-object
      (goto-char (point-max))
      (if (re-search-backward (concat idf-type-prefix-regexp idf-type-regexp
                                      idf-type-variable-separator-regexp
                                      current-object "[,;]")
                              nil t)
          (goto-char (match-beginning 2))
        (goto-char current-point)))
     (t
      (pop-tag-mark)
      (error "Don't know how to find '%s'" current-object)))))

(defun idf-show-ctable ()
  (interactive)
  (let* ((idf-ctable-buffer "*idf-ctable*")
         (search-beg (if (region-active-p)(region-beginning) (point-min)))
         (search-end (if (region-active-p)(region-end) (point-max)))
         (idf-object-list (save-excursion
                            (goto-char (+ (line-end-position) 1))
                            (re-search-backward (concat idf-type-prefix-regexp idf-type-regexp ",") nil t)
                            (let ((current-type (match-string 2))
                                  current-class-string)
                              (goto-char search-beg)
                              (while (re-search-forward (concat idf-type-prefix-regexp current-type) search-end t)
                                (push (buffer-substring (match-beginning 0) (re-search-forward ".*;.*\n" nil t)) current-class-string))
                              (reverse current-class-string))))
         (idf-keys (mapcar (lambda (x) (car (reverse (split-string x "[,;]" nil "[ \t]+"))))
                           (split-string (car (sort (copy-sequence idf-object-list)
                                                    (lambda (a b)
                                                      (> (s-count-matches "\n" a) (s-count-matches "\n" b))))) "\n" t)))
         (idf-contents (mapcar (lambda (x)
                                 (mapcar (lambda (y) (cadr (reverse (split-string y "[,;]" nil "[ \t]+"))))
                                         (split-string x "\n" t)))
                               idf-object-list)))
    (deactivate-mark)
    (lexical-let ((curr-buf (current-buffer))
                  (idf-class-name (caar idf-contents))
                  (search-beg search-beg))
      (switch-to-buffer-other-window idf-ctable-buffer)
      (set-buffer idf-ctable-buffer)
      (let* ((data (apply '-zip-fill (cons "" (cons idf-keys idf-contents))))
             (param (copy-ctbl:param ctbl:default-rendering-param)))
        (lexical-let ((cp
                       (ctbl:create-table-component-buffer
                        :buffer idf-ctable-buffer :width nil
                        :model
                        (make-ctbl:model
                         :column-model
                         (cons (make-ctbl:cmodel
                                :title idf-class-name
                                :min-width 5 :align 'left)
                               (cl-loop for i from 1 to (length idf-contents)
                                        collect (make-ctbl:cmodel
                                                 :title (number-to-string i)
                                                 :min-width 5 :align 'left)))
                         :data
                         (if (= (length idf-contents) 1)
                             (-mapcat (lambda (x) (list (list (car x) (cdr x)))) (cdr data))
                           (cdr data)))
                        :param param)))
          (ctbl:cp-add-click-hook
           cp (lambda ()
                (let ((column-id (cdr (ctbl:cp-get-selected cp)))
                      (row-key (car (ctbl:cp-get-selected-data-row cp))))
                  (switch-to-buffer-other-window curr-buf)
                  (goto-char search-beg)
                  (re-search-forward (concat idf-type-prefix-regexp idf-class-name) nil t column-id)
                  (re-search-forward (concat "[,;].*" row-key) nil t)
                  (goto-char (match-beginning 0)))))
          (pop-to-buffer (ctbl:cp-get-buffer cp)))))))

;;;###autoload
(define-derived-mode idf-mode text-mode "idf mode"
  "Major mode for editing idf file."
  ;; code for syntax highlighting
  :syntax-table idf-mode-syntax-table
  (set 'font-lock-defaults '(idf-font-lock-keywords))
  (setq-local comment-start "!")
  (setq-local comment-start-skip "!- ")
  (setq-local comment-end ""))

(provide 'idf-mode)

;;; idf-mode.el ends here
