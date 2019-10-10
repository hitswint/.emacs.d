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

(defvar idf-mode-syntax-table
  (let ((st (make-syntax-table)))
    ;; Comments
    (modify-syntax-entry ?! "<" st)
    (modify-syntax-entry ?\n ">" st)
    st)
  "Syntax table for idf-mode.")

(defconst idf-font-lock-keywords
  `((,(concat "^[ \t]*" "\\([a-zA-Z0-9:]*\\)" ",[ \t]*\n")
     (1 font-lock-type-face))
    (,(concat "^[ \t]*" "\\([a-zA-Z0-9:]*\\)" ",.*;[ \t]*\n")
     (1 font-lock-type-face))
    (,(concat "^[ \ta-zA-Z0-9:]*" ",[ \t]*\n" "[ \t]*" "\\(.*\\)" "[,;].*\n")
     (1 font-lock-variable-name-face))
    (,(concat "^[ \t]*" "\\(.*\\)" "[,;][ \t]*!- .+Name[ \t]*\n")
     (1 font-lock-builtin-face))
    (,(concat ".+!- " "\\(.*\\)" "\n")
     (1 font-lock-keyword-face)))
  "Keywords for highlighting.")

(defcustom idf-imenu-generic-expression
  '((nil "^[ \t]*\\([a-zA-Z0-9:]*,\n[ \t]*.*\\)[,;].*\n" 1))
  "The imenu regex to parse an outline of the idf file.")

(defun idf-imenu-create-index-function ()
  (let ((default-index (imenu-default-create-index-function)))
    (cl-loop for i in default-index
             collect (cons (replace-regexp-in-string ",[ \t]*\n[ \t]*" " | " (car i)) (cdr i)))))

(defun idf-set-imenu-generic-expression ()
  (make-local-variable 'imenu-generic-expression)
  (make-local-variable 'imenu-create-index-function)
  (setq imenu-create-index-function 'idf-imenu-create-index-function)
  (setq imenu-generic-expression idf-imenu-generic-expression))

(add-hook 'idf-mode-hook 'idf-set-imenu-generic-expression)

(defun idf-prev-object ()
  (interactive)
  (re-search-backward (concat "^[ \t]*\\([a-zA-Z0-9:]*\\),\\([ \t]*\n\\|.*;[ \t]*\n\\)") nil t))

(defun idf-next-object ()
  (interactive)
  (goto-char (+ (line-end-position) 1))
  (if (re-search-forward (concat "^[ \t]*\\([a-zA-Z0-9:]*\\),\\([ \t]*\n\\|.*;[ \t]*\n\\)") nil t)
      (forward-line -1)))

(defun idf-next-type ()
  (interactive)
  (goto-char (+ (line-end-position) 1))
  (re-search-backward (concat "^[ \t]*\\([a-zA-Z0-9:]*\\),\\([ \t]*\n\\|.*;[ \t]*\n\\)") nil t)
  (let* ((current-type (match-string 1))
         (next-type current-type))
    (while (equal next-type current-type)
      (if (idf-next-object)
          (setq next-type (match-string 1))
        (setq next-type nil)))))

(defun idf-prev-type ()
  (interactive)
  (goto-char (+ (line-end-position) 1))
  (re-search-backward (concat "^[ \t]*\\([a-zA-Z0-9:]*\\),\\([ \t]*\n\\|.*;[ \t]*\n\\)") nil t)
  (let* ((current-type (match-string 1))
         (prev-type current-type))
    (while (equal prev-type current-type)
      (if (idf-prev-object)
          (setq prev-type (match-string 1))
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
      (unless (re-search-backward (concat "^[ \ta-zA-Z0-9:]*,[ \t]*\n[ \t]*" current-object "[,;].*\n") nil t)
        (goto-char current-point)))
     (t
      (pop-tag-mark)
      (error "Don't know how to find '%s'" current-object)))))

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
