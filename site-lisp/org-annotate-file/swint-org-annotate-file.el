;;; org-annotate-file.el --- Annotate a file with org syntax

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
;; (setq swint-org-annotate-file-storage-file "~/annotated.org")
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

(require 'org)

;; (defvar swint-org-annotate-file-storage-file "~/.org-annotate-file.org"
;;   "File in which to keep annotations.")
(defun swint-org-annotate-file-storage-file ()
  "Modified from var to function"
  (concat "~/org/annotated/annotated-["
          (replace-regexp-in-string
           "/" "_" (substring-no-properties (abbreviate-file-name default-directory) 1 -1))
          "].org"))

(defvar swint-org-annotate-file-add-search nil
  "If non-nil then add a link as a second level to the actual
location in the file")

(defvar swint-org-annotate-file-always-open t
  "non-nil means always expand the full tree when you visit
`swint-org-annotate-file-storage-file'.")

(defvar swint-org-annotate-file-append-to-end t
  "If non-nil add each second-level link to the end of the file.")

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

(defun swint-org-annotate-file (&optional filename)
  "Put a section for the current file into your annotation file"
  (interactive "FFile to annotate: ")
                                        ; if a file is specified, bypass the check for error when no file
  (if filename
      (swint-org-annotate-file-show-section filename)
    (progn
      (error-if-no-file)
      (swint-org-annotate-file-show-section))))

(defun error-if-no-file ()
  "Raises an error if the current buffer doesn't have a file."
  (unless (buffer-file-name)
    (error "This buffer has no associated file.")))

(defun swint-org-annotate-file-show-section (&optional buffer-or-file)
  "Visit the buffer named `swint-org-annotate-file-storage-file' and
show the relevant section"
  (let* ((line (buffer-substring-no-properties (point-at-bol) (point-at-eol)))
         (filename (if (stringp buffer-or-file)
                       buffer-or-file
                     (swint-get-filename buffer-or-file (buffer-file-name))))
         (link (swint-get-link buffer-or-file))
         (search-link (org-make-link-string
                       (concat "file:" filename "::" line)
                       (swint-org-annotate-file-prettyfy-desc line))))
    (swint-show-annotations filename link)
    (with-current-buffer (find-file (swint-org-annotate-file-storage-file))
      ;; deal with a '::' search if need be
      (when swint-org-annotate-file-add-search
        (unless (search-forward-regexp
                 (concat "^** " (regexp-quote search-link)) nil t)
          (swint-org-annotate-file-add-second-level search-link))))))

(defun swint-get-filename (buffer buffer-file-name)
  (file-name-nondirectory (or (buffer-file-name) buffer)))

(defun swint-get-link (filename)
                                        ; TODO make this handle non-file links
                                        ; like docview: or html: links.
  (swint-org-store-link (point-at-bol)))

(defun swint-org-annotate-file-add-upper-level (link)
  (swint-new-entry)
  (insert "* ")
  (insert link)
  (newline))

(defun swint-org-annotate-file-add-second-level (link)
  (swint-new-entry)
  (insert "** ")
  (insert link)
  (swint-put-cursor-in-annotation))

(defun swint-new-entry ()
  "Safely positions cursor for a new entry."
  (goto-char (point-max))
  (unless (equal (char-before) 10)
    (newline)))

(defun swint-put-cursor-in-annotation ()
  "After a link is inserted, position cursor with newlines to avoid
clobbering sucessive entries."
  (newline)
  (newline)
  (newline)
  (previous-line))

(defun swint-org-annotate-file-show-annotations (&optional buffer)
  "Show the annotations in the current file, without adding new ones."
  (interactive)
  (error-if-no-file)
  (let* ((filename (swint-get-filename buffer (buffer-file-name)))
         (link (swint-get-link filename)))
    (swint-show-annotations filename link)))

(defun swint-show-annotations (filename link)
  (with-current-buffer (find-file (swint-org-annotate-file-storage-file))
    (unless (org-mode)
      (org-mode))
    (widen)                             ;交换上下两句，解决无法显示已建注释
    (goto-char (point-min))
    (when swint-org-annotate-file-always-open
      (show-all))
    (unless (search-forward-regexp
             (concat "^* " (regexp-quote link)) nil t)
      (swint-org-annotate-file-add-upper-level link))
    (beginning-of-line)
    (org-narrow-to-subtree)))

;; 加swint-org-store-link
(defun swint-org-store-link (arg)
  "\\<org-mode-map>Store an org-link to the current location.
This link is added to `org-stored-links' and can later be inserted
into an org-buffer with \\[org-insert-link].

For some link types, a prefix arg is interpreted.
For links to Usenet articles, arg negates `org-gnus-prefer-web-links'.
For file links, arg negates `org-context-in-file-links'.

A double prefix arg force skipping storing functions that are not
part of Org's core.

A triple prefix arg force storing a link for each line in the
active region."
  (interactive "P")
  (org-load-modules-maybe)
  (if (and (equal arg '(64)) (org-region-active-p))
      (save-excursion
        (let ((end (region-end)))
          (goto-char (region-beginning))
          (set-mark (point))
          (while (< (point-at-eol) end)
            (move-end-of-line 1) (activate-mark)
            (let (current-prefix-arg)
              (call-interactively 'org-store-link))
            (move-beginning-of-line 2)
            (set-mark (point)))))
    (org-with-limited-levels
     (setq org-store-link-plist nil)
     (let (link cpltxt desc description search
                txt custom-id agenda-link sfuns sfunsn)
       (cond

        ;; Store a link using an external link type
        ((and (not (equal arg '(16)))
              (setq sfuns
                    (delq
                     nil (mapcar (lambda (f)
                                   (let (fs) (if (funcall f) (push f fs))))
                                 org-store-link-functions))
                    sfunsn (mapcar (lambda (fu) (symbol-name (car fu))) sfuns))
              (or (and (cdr sfuns)
                       (funcall (intern
                                 (completing-read
                                  "Which function for creating the link? "
                                  sfunsn t (car sfunsn)))))
                  (funcall (caar sfuns)))
              (setq link (plist-get org-store-link-plist :link)
                    desc (or (plist-get org-store-link-plist
                                        :description) link))))

        ;; Store a link from a source code buffer
        ((org-src-edit-buffer-p)
         (let (label gc)
           (while (or (not label)
                      (save-excursion
                        (save-restriction
                          (widen)
                          (goto-char (point-min))
                          (re-search-forward
                           (regexp-quote (format org-coderef-label-format label))
                           nil t))))
             (when label (message "Label exists already") (sit-for 2))
             (setq label (read-string "Code line label: " label)))
           (end-of-line 1)
           (setq link (format org-coderef-label-format label))
           (setq gc (- 79 (length link)))
           (if (< (current-column) gc) (org-move-to-column gc t) (insert " "))
           (insert link)
           (setq link (concat "(" label ")") desc nil)))

        ;; We are in the agenda, link to referenced location
        ((equal (org-bound-and-true-p org-agenda-buffer-name) (buffer-name))
         (let ((m (or (get-text-property (point) 'org-hd-marker)
                      (get-text-property (point) 'org-marker))))
           (when m
             (org-with-point-at m
               (setq agenda-link
                     (if (org-called-interactively-p 'any)
                         (call-interactively 'org-store-link)
                       (org-store-link nil)))))))

        ((eq major-mode 'calendar-mode)
         (let ((cd (calendar-cursor-to-date)))
           (setq link
                 (format-time-string
                  (car org-time-stamp-formats)
                  (apply 'encode-time
                         (list 0 0 0 (nth 1 cd) (nth 0 cd) (nth 2 cd)
                               nil nil nil))))
           (org-store-link-props :type "calendar" :date cd)))

        ((eq major-mode 'help-mode)
         (setq link (concat "help:" (save-excursion
                                      (goto-char (point-min))
                                      (looking-at "^[^ ]+")
                                      (match-string 0))))
         (org-store-link-props :type "help"))

        ((eq major-mode 'w3-mode)
         (setq cpltxt (if (and (buffer-name)
                               (not (string-match "Untitled" (buffer-name))))
                          (buffer-name)
                        (url-view-url t))
               link (url-view-url t))
         (org-store-link-props :type "w3" :url (url-view-url t)))

        ((eq major-mode 'image-mode)
         (setq cpltxt (concat "file:"
                              (abbreviate-file-name buffer-file-name))
               link cpltxt)
         (org-store-link-props :type "image" :file buffer-file-name))

        ;; In dired, store a link to the file of the current line
        ((eq major-mode 'dired-mode)
         (let ((file (dired-get-filename nil t)))
           (setq file (if file
                          (file-name-nondirectory (dired-get-filename nil t))
                        ;; (abbreviate-file-name
                        ;;  (expand-file-name (dired-get-filename nil t)))
                        ;; otherwise, no file so use current directory.
                        default-directory))
           (setq cpltxt (concat "file:" file)
                 link cpltxt)))

        ((setq search (run-hook-with-args-until-success
                       'org-create-file-search-functions))
         (setq link (concat "file:" (abbreviate-file-name buffer-file-name)
                            "::" search))
         (setq cpltxt (or description link)))

        ((and (buffer-file-name (buffer-base-buffer)) (derived-mode-p 'org-mode))
         (setq custom-id (org-entry-get nil "CUSTOM_ID"))
         (cond
          ;; Store a link using the target at point
          ((org-in-regexp "[^<]<<\\([^<>]+\\)>>[^>]" 1)
           (setq cpltxt
                 (concat "file:"
                         (abbreviate-file-name
                          (buffer-file-name (buffer-base-buffer)))
                         "::" (match-string 1))
                 link cpltxt))
          ((and (featurep 'org-id)
                (or (eq org-id-link-to-org-use-id t)
                    (and (org-called-interactively-p 'any)
                         (or (eq org-id-link-to-org-use-id 'create-if-interactive)
                             (and (eq org-id-link-to-org-use-id
                                      'create-if-interactive-and-no-custom-id)
                                  (not custom-id))))
                    (and org-id-link-to-org-use-id (org-entry-get nil "ID"))))
           ;; Store a link using the ID at point
           (setq link (condition-case nil
                          (prog1 (org-id-store-link)
                            (setq desc (plist-get org-store-link-plist
                                                  :description)))
                        (error
                         ;; Probably before first headline, link only to file
                         (concat "file:"
                                 (abbreviate-file-name
                                  (buffer-file-name (buffer-base-buffer))))))))
          (t
           ;; Just link to current headline
           (setq cpltxt (concat "file:"
                                (abbreviate-file-name
                                 (buffer-file-name (buffer-base-buffer)))))
           ;; Add a context search string
           (when (org-xor org-context-in-file-links arg)
             (let* ((ee (org-element-at-point))
                    (et (org-element-type ee))
                    (ev (plist-get (cadr ee) :value))
                    (ek (plist-get (cadr ee) :key))
                    (eok (and (stringp ek) (string-match "name" ek))))
               (setq txt (cond
                          ((org-at-heading-p) nil)
                          ((and (eq et 'keyword) eok) ev)
                          ((org-region-active-p)
                           (buffer-substring (region-beginning) (region-end)))))
               (when (or (null txt) (string-match "\\S-" txt))
                 (setq cpltxt
                       (concat cpltxt "::"
                               (condition-case nil
                                   (org-make-org-heading-search-string txt)
                                 (error "")))
                       desc (or (and (eq et 'keyword) eok ev)
                                (nth 4 (ignore-errors (org-heading-components)))
                                "NONE")))))
           (if (string-match "::\\'" cpltxt)
               (setq cpltxt (substring cpltxt 0 -2)))
           (setq link cpltxt))))

        ((buffer-file-name (buffer-base-buffer))
         ;; Just link to this file here.
         (setq cpltxt (concat "file:"
                              (file-name-nondirectory (buffer-file-name (buffer-base-buffer)))))
         ;; Add a context string.
         (when (org-xor org-context-in-file-links arg)
           (setq txt (if (org-region-active-p)
                         (buffer-substring (region-beginning) (region-end))
                       (buffer-substring (point-at-bol) (point-at-eol))))
           ;; Only use search option if there is some text.
           (when (string-match "\\S-" txt)
             (setq cpltxt
                   (concat cpltxt "::" (org-make-org-heading-search-string txt))
                   desc "NONE")))
         (setq link cpltxt))

        ((org-called-interactively-p 'interactive)
         (user-error "No method for storing a link from this buffer"))
        (t (setq link nil)))
       ;; We're done setting link and desc, clean up
       (if (consp link) (setq cpltxt (car link) link (cdr link)))
       (setq link (or link cpltxt)
             desc (or desc cpltxt))
       (cond ((equal desc "NONE") (setq desc nil))
             ((string-match org-bracket-link-analytic-regexp desc)
              (let ((d0 (match-string 3 desc))
                    (p0 (match-string 5 desc)))
                (setq desc
                      (replace-regexp-in-string
                       org-bracket-link-regexp
                       (concat (or p0 d0)
                               (if (equal (length (match-string 0 desc))
                                          (length desc)) "*" "")) desc)))))
       ;; Return the link
       (if (not (and (or (org-called-interactively-p 'any)
                         executing-kbd-macro) link))
           (or agenda-link (and link (org-make-link-string link desc)))
         (push (list link desc) org-stored-links)
         (message "Stored: %s" (or desc link))
         (when custom-id
           (setq link (concat "file:" (abbreviate-file-name
                                       (buffer-file-name)) "::#" custom-id))
           (push (list link desc) org-stored-links)))))))
(provide 'swint-org-annotate-file)
;;; swint-org-annotate-file.el ends here
