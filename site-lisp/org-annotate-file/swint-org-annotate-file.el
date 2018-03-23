;; * swint-org-annotate-file.el --- Annotate a file with org syntax

;; Copyright (C) 2008 Philip Jackson

;; Author: Philip Jackson <phil@shellarchive.co.uk>
;; Version: 0.2

;; This file is not currently part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program ; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This is yet another implementation to allow the annotation of a
;; file without modification of the file itself. The annotation is in
;; org syntax so you can use all of the org features you are used to.

;; To use you might put the following in your .emacs:
;;
;; (require 'org-annotate-file)
;; (global-set-key (kbd "C-c C-l") 'org-annotate-file)
;;
;; To change the location of the annotation file:
;;
;; (setq swint-org-annotation-storage-file "~/annotated.org")
;;
;; Then when you visit any file and hit C-c C-l you will find yourself
;; in an org buffer on a headline which links to the file you were
;; visiting, e.g:

;; * ~/org-annotate-file.el

;; Under here you can put anything you like, save the file
;; and next time you hit C-c C-l you will hit those notes again.
;;
;; To put a subheading with a text search for the current line set
;; `swint-org-annotate-file-add-search` to non-nil value. Then when you hit
;; C-c C-l (on the above line for example) you will get:

;; * ~/org-annotate-file.el
;; ** `swint-org-annotate-file-add-search` to non-nil value. Then whe...

;; Note that both of the above will be links.

;; (defvar swint-org-annotation-storage-file "~/.org-annotate-file.org"
;;   "File in which to keep annotations.")

(defun swint-org-annotation-storage-file ()
  "Modified from var to function"
  (concat "~/org/annotated/annotated-("
          (replace-regexp-in-string
           "/" "_" (substring-no-properties (abbreviate-file-name default-directory) 1))
          ").org"))

(defvar swint-org-annotate-file-add-search nil
  "If non-nil then add a link as a second level to the actual
location in the file")

(defvar swint-org-annotate-file-always-open t
  "non-nil means always expand the full tree when you visit
`swint-org-annotation-storage-file'.")

(defun swint-org-annotate-file-prettyfy-desc (string)
  "Strip starting and ending whitespace and replace any chars
after the 60th with '...'"
  (let ((replace-map '(("^[ \t]*" . "")
                       ("[ \t]*$" . "")
                       ("^\\(.\\{60\\}\\).*" . "\\1..."))))
    (dolist (replace replace-map)
      (when (string-match (car replace) string)
        (setq string (replace-match (cdr replace) nil nil string))))
    string))

(defun swint-org-annotate-file-add-upper-level (link)
  (swint-new-entry)
  (insert "* ")
  (insert link)
  (newline))

(defun swint-org-annotate-file-add-second-level (link)
  (swint-new-entry)
  (insert "** ")
  (insert link)
  (newline)
  (newline)
  (newline)
  (forward-line -1))

(defun swint-new-entry ()
  "Safely positions cursor for a new entry."
  (goto-char (point-max))
  (unless (equal (char-before) 10)
    (newline)))

(defun swint-org-annotate-file (filename)
  (let* ((filename (file-name-nondirectory filename))
         (filelink (concat "file:" filename))
         (link (org-make-link-string filelink filelink)))
    (with-current-buffer (find-file (swint-org-annotation-storage-file))
      (unless (org-mode)
        (org-mode))
      (widen)                             ;交换上下两句，解决无法显示已建注释
      (goto-char (point-min))
      (when swint-org-annotate-file-always-open
        (outline-show-all))
      (unless (search-forward-regexp
               (concat "^* " (regexp-quote link)) nil t)
        (swint-org-annotate-file-add-upper-level link))
      (beginning-of-line)
      (org-narrow-to-subtree))))

(defun swint-org-annotate-file-current ()
  (interactive)
  (cond
   ((equal major-mode 'dired-mode)
    (swint-org-annotate-file (dired-get-filename)))
   ((equal major-mode 'pdf-view-mode)
    (dired-jump-other-window)
    (swint-org-annotate-file (dired-get-filename)))
   (t
    (switch-to-buffer-other-window (current-buffer))
    (swint-org-annotate-file (buffer-file-name)))))

(provide 'swint-org-annotate-file)
;;; swint-org-annotate-file.el ends here
